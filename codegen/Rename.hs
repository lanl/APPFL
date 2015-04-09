{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Rename (
  renameObjs,
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

renameObjs objs =
  -- preserve top-level names--
  let (objs', _) = runState (topNameObjs objs []) []
  in objs'

-- this is the tricky one because they're all in the same letrec scope
-- need to figure out how to do circular programs with the state monad involved

topNameObjs :: [Obj a] -> [(Var, Var)] -> State [Var] [Obj a]
topNameObjs objs tt = mapM ((flip nameObj) tt) objs

-- do name substitution at [Obj] level so top-level names can be preserved
nameObjs objs tt =
    do
      let names = map oname objs
      names' <- suffixnames names
      let objs' = [o {oname = name'} | (name',o) <- zip names' objs]
      let tt' = (zip names names') ++ tt
      objs'' <- mapM ((flip nameObj) tt') objs'
      return objs''

nameObj (FUN md vs e name) tt =
    do
      e' <- nameExpr e (dropall vs tt)
      return (FUN md vs e' name)

nameObj (THUNK md e name) tt =
    do
      e' <- nameExpr e tt
      return (THUNK md e' name)

nameObj (PAP md f as name) tt =
    let f' = condrepl f tt
        as' = nameAtoms as tt
    in
      return (PAP md f' as' name)

nameObj (CON md c as name) tt =
    return (CON md c (nameAtoms as tt) name)

nameObj (BLACKHOLE md name) tt = 
    return (BLACKHOLE md name)

nameExpr (ELet md objs e) tt =
    do
      let names = map oname objs
      objs' <- nameObjs objs tt
      let names' = map oname objs'
      e' <- nameExpr e ((zip names names')++tt)
      return (ELet md objs' e')

nameExpr (ECase md e alts) tt =
    do
      e' <- nameExpr e tt
      alts' <- nameAlts alts tt
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

nameAlts (Alts md alts name) tt =
    do 
      name' <- suffixname name
      alts' <- mapM ((flip nameAlt) tt) alts
      return (Alts md alts' name')

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

