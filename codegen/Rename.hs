{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rename (
  renameDefs,
  setFVsDefs
) where


import Prelude
import Parser
import Data.List

--import Control.Monad.State.Lazy

newtype State s a = State (s -> (a, s))

state :: (s -> (a, s)) -> State s a
state x = State x

runState :: State s a -> s -> (a, s)
runState (State f) x = f x

instance Monad (State s) where
    return x = state (\st -> (x, st))
    act >>= k = state $ \st -> 
                          let (x, st') = runState act st
                          in runState (k x) st'

get = State $ \s -> (s,s)

put newState = State $ \s -> ((), newState)  

isNum c = c >= '0' && c <= '9'

withsuff s [] = error "this should never happen"
withsuff s (x:xs) = 
    let (h,t) = splitAt (length s) x in
    if h == s && t /= [] && head t == '_' && all isNum (tail t) then
        (h, tail t)
    else withsuff s xs
          
nextv s used =
    if not $ elem (s ++ "_0") used then s ++ "_0"
    else -- find most recent
        let (base, suff) = withsuff s used
        in base ++ "_" ++ show (read suff + 1)

-- only have to get into the monad one time, here
suffixname v = State $ \used -> let nv = nextv v used in (nv, nv:used)

suffixnames = mapM suffixname

-- uniquify obj names, systematically renaming 

-- no need to flail in the monad at Atom level
nameAtom (Var v) tt = Var $ condrepl v tt
nameAtom (Lit l) tt = Lit l
nameAtoms as tt = map ((flip nameAtom) tt) as

renameDefs defs =
  -- preserve top-level names--names is initial state
  let (names, objs) = unzip defs
      (objs', _) = runState (nameObjs objs []) names
  in zip names objs'

-- this is the tricky one because they're all in the same letrec scope
-- need to figure out how to do circular programs with the state monad involved

nameObjs :: [Obj a] -> [(Var, Var)] -> State [Var] [Obj a]
nameObjs objs tt = mapM ((flip nameObj) tt) objs

nameObj (FUN md vs e) tt =
    do
      e' <- nameExpr e (dropall vs tt)
      return (FUN md vs e')

nameObj (THUNK md e) tt =
    do
      e' <- nameExpr e tt
      return (THUNK md e')

nameObj (PAP md f as) tt =
    let f' = condrepl f tt
        as' = nameAtoms as tt in
    return (PAP md f' as')

nameObj (CON md c as) tt =
    return (CON md c $ nameAtoms as tt)

nameObj (BLACKHOLE md) tt = return (BLACKHOLE md)

--          Defs a  -> subst map    -> State usednames [Def a]
nameDefs :: [Def a] -> [(Var, Var)] -> State [Var]     [Def a]
nameDefs defs tt = 
    do
      let (names, objs) = unzip defs
      names' <- suffixnames names
      let tt' = (zip names names') ++ tt
      objs' <- mapM ((flip nameObj) tt') objs
      return $ zip names' objs'
      
nameExpr (ELet md defs e) tt =
    do
      let names = map fst defs
      defs' <- nameDefs defs tt
      let names' = map fst defs'
      e' <- nameExpr e ((zip names names')++tt)
      return (ELet md defs' e')

nameExpr (ECase md e alts) tt =
    do
      e' <- nameExpr e tt
      alts' <- mapM ((flip nameAlt) tt) alts
      return (ECase md e' alts')

nameExpr (EAtom md a) tt =
    return $ EAtom md (nameAtom a tt)

nameExpr (EFCall md f as) tt =
    let f' = condrepl f tt
        as' = nameAtoms as tt in
    return (EFCall md f' as')

nameExpr (EPrimop md p as) tt =
    let as' = nameAtoms as tt in
    return (EPrimop md p as')


nameAlt (ACon md c vs e) tt = 
    do
      e' <- nameExpr e (dropall vs tt)
      return (ACon md c vs e')

nameAlt (ADef md v e) tt = 
    do
      e' <- nameExpr e (dropall [v] tt)
      return (ADef md v e')

-- drop all references to vs in map tt
-- there's a fold in here...

dropall [] tt = tt
dropall vs [] = [] -- shortcut
dropall (v:vs) tt = dropall vs (filter ((/=v).fst) tt)

condrepl v tt = case lookup v tt of
                  Just v' -> v'
                  Nothing -> v

-- *****************************************************

-- after rename, make the fvs meaningful
-- TLDs are not considered free vars in expressions

setFVsDefs :: [Def a] -> [Def [Var]] -- monomorphism restriction
setFVsDefs defs = snd (setfvs (map fst defs) defs)

class FVs a where fvsof :: [Var] -> a -> [Var]

instance FVs Atom where
    fvsof tlds (Var v) = if elem v tlds then [] else [v]
    fvsof tlds (Lit l) = []

instance FVs [Atom] where
    fvsof tlds as = nub $ concatMap (fvsof tlds) as

-- setfvs both sets the metadata to the thing's fvs and returns the fvs
-- here "a" is e.g. "Expr ()" from the parser and "b" is "Expr [Var]"

class SetFVs a b where 
    setfvs :: [Var] -> a -> ([Var], b)

instance SetFVs (Expr a) (Expr [Var]) where
    setfvs tlds (EAtom _ a) = 
        let myfvs = fvsof tlds a \\ tlds
        in (myfvs, EAtom myfvs a)

    --  efcall introduces a var
    setfvs tlds (EFCall _ f as) = 
        -- nub superfluous if typed, \\ tlds just for f
        let myfvs = (nub $ f : (fvsof tlds as)) \\ tlds  
        in (myfvs, EFCall myfvs f as)

    setfvs tlds (EPrimop _ p as) = 
        let myfvs = fvsof tlds as 
        in (myfvs, EPrimop myfvs p as)

    -- let introduces scope
    setfvs tlds (ELet _ defs e) = 
        let (defsfvs, defs') = setfvs tlds defs 
            defslhs = map fst defs'
            -- \\ not need if objs have unique names
            (efvs, e') = setfvs (tlds \\ defslhs) e  
            myfvs = nub $ defsfvs ++ (efvs \\ defslhs)
        in (myfvs, ELet myfvs defs' e')

    setfvs tlds (ECase _ e alts) = 
        let (altsfvs, alts') = setfvs tlds alts
            (efvs, e') = setfvs tlds e
            myfvs = nub $ efvs ++ altsfvs
        in (myfvs, ECase myfvs e' alts')

-- alts introduce scope
instance SetFVs (Alt a) (Alt [Var]) where
    setfvs tlds (ACon _ c vs e) = 
        let (efvs, e') = setfvs (tlds \\ vs) e
            myfvs = efvs \\ vs
        in (myfvs, ACon myfvs c vs e')
    setfvs tlds (ADef _ v e) = 
        let (efvs, e') = setfvs (tlds \\ [v]) e
            myfvs = efvs \\ [v]
        in  (myfvs, ADef myfvs v e')

instance SetFVs [Alt a] [Alt [Var]] where
    setfvs tlds alts =
        let (altsfvss, alts') = unzip $ map (setfvs tlds) alts
        in (nub $ concat altsfvss, alts')

-- FUN introduces scope
instance SetFVs (Obj a) (Obj [Var]) where
    setfvs tlds (FUN _ vs e) = 
        let (efvs, e') = setfvs (tlds \\ vs) e -- shadow vs in tlds
            myfvs = efvs \\ vs
        in (myfvs, FUN myfvs vs e')

    -- PAP introduces a var
    setfvs tlds (PAP _ f as) = 
        let asfvs = fvsof tlds as
            myfvs = (nub $ f : asfvs) \\ tlds -- like EFCall
        in (myfvs, PAP myfvs f as)

    setfvs tlds (CON _ c as) = 
        let myfvs = fvsof tlds as
        in (myfvs, CON myfvs c as)

    setfvs tlds (THUNK _ e) = 
        let (myfvs, e') = setfvs tlds e
        in (myfvs, THUNK myfvs e')

    setfvs tlds (BLACKHOLE _) = 
        ([], BLACKHOLE [])

--only interesting for non-recursive let
--instance SetFVs Def where
--    setfvs tlds (v, o) = (setfvs tlds o) \\ [v]

instance SetFVs [Def a] [Def [Var]] where
    setfvs tlds defs = 
        let (vs, os) = unzip defs 
            (osfvss, os') = unzip $ map (setfvs tlds) os
            osfvs = nub $ concat osfvss
        in (osfvs \\ vs, zip vs os')
