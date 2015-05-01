{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SetFVs (
  setFVsDefs
) where

import Prelude
import AST
import ADT
import Data.List

-- *****************************************************

dropkey k = filter (\(k',_)->k==k')

-- for no top-level dead code elimination
-- close lhs rhs = rhs ++ [lhs] -- main at end

close lhs rhs =
    let lhsfvs = (nub $ concatMap (fst.snd) lhs) \\ map fst lhs in
    if lhsfvs == [] then lhs 
    else case filter (\(k,_) -> elem k lhsfvs) rhs of
           [] -> error $ "close:  unbound variables " ++ intercalate " " lhsfvs
           xs -> close (lhs ++ xs) rhs

-- if there's no main don't do anything
deadCode fvsdefs =
    let (fvs,defs) = unzip fvsdefs
        nameMap = zip (map oname defs) fvsdefs
    in case lookup "main" nameMap of
         Nothing -> defs
         Just v -> map (snd.snd) $ close [("main", v)] nameMap
                 

-- after rename, make the fvs meaningful
-- TLDs are not considered free vars in expressions
{-
setFVsDefs :: [Def a] -> [String] -> [Def [Var]]
setFVsDefs defs runtimeGlobals = 
    let (ts, os) = splitDefs defs
    in unsplitDefs (ts, setFVsObjs' os runtimeGlobals)
-}

setFVsDefs :: [String] -> [Def a] -> [Def [Var]]
setFVsDefs runtimeGlobals = onObjs (setFVsObjs runtimeGlobals)

setFVsObjs :: [String] -> [Obj a] -> [Obj [Var]] -- monomorphism restriction
setFVsObjs runtimeGlobals defs
    -- tlds can shadow runtime globals
    = let tlds = map oname defs
          triples = map (setfvs (tlds ++ runtimeGlobals)) defs
          (fvss, truefvss, defs') = unzip3 triples in
      case concat fvss of
        [] -> deadCode $ zip (map (\\ runtimeGlobals) truefvss) defs'
        fvs -> error $ "SetFVs.setFVsDefs:  top level free variables:  " ++  intercalate " " fvs

-- the two base cases are the f in function calls and Var v
class FVs a where fvsof :: [Var] -> a -> ([Var], [Var])

instance FVs Atom where
    fvsof tlds (Var v) = (if elem v tlds then [] else [v], [v])
    fvsof tlds _ = ([],[])

instance FVs [Atom] where
    fvsof tlds as = 
        let (xs, ys) = unzip $ map (fvsof tlds) as
        in (nub $ concat xs, nub $ concat ys)

-- setfvs both sets the metadata to the thing's fvs and returns the fvs
-- here "a" is e.g. "Expr ()" from the parser and "b" is "Expr [Var]"

class SetFVs a b where 
    setfvs :: [Var] -> a -> ([Var], [Var], b)

instance SetFVs (Expr a) (Expr [Var]) where
    setfvs tlds (EAtom _ a) = 
        let (myfvs, truefvs) = fvsof tlds a
        in (myfvs, truefvs, EAtom myfvs a)

    --  efcall introduces a var
    setfvs tlds (EFCall _ f as) = 
        -- nub superfluous if typed, \\ tlds just for f
        let (asfvs, astruefvs) = fvsof tlds as
            myfvs   = (nub $ f : asfvs)  \\ tlds  
            truefvs = (nub $ f : astruefvs)
        in (myfvs, truefvs, EFCall myfvs f as)

    setfvs tlds (EPrimop _ p as) = 
        let (myfvs, truefvs) = fvsof tlds as 
        in (myfvs, truefvs, EPrimop myfvs p as)

    -- let introduces scope
    setfvs tlds (ELet _ defs e) = 
        let (defsfvs, defstruefvs, defs') = setfvs tlds defs 
            names = map oname defs'
            -- \\ not needed if objs have unique names
            (efvs, etruefvs, e') = setfvs (tlds \\ names) e  
            myfvs    = nub $ defsfvs     ++ (efvs     \\ names)
            truefvs  = nub $ defstruefvs ++ (etruefvs \\ names)
        in (myfvs, truefvs, ELet myfvs defs' e')

    setfvs tlds (ECase _ e alts) = 
        let (altsfvs, altstruefvs, alts') = setfvs tlds alts
            (efvs, etruefvs, e') = setfvs tlds e
            myfvs   = nub $ efvs     ++ altsfvs
            truefvs = nub $ etruefvs ++ altstruefvs
        in (myfvs, truefvs, ECase myfvs e' alts')

-- alts introduce scope
instance SetFVs (Alt a) (Alt [Var]) where
    setfvs tlds (ACon _ c vs e) = 
        let (efvs, etruefvs, e') = setfvs (tlds \\ vs) e
            myfvs = efvs \\ vs
            truefvs = etruefvs \\ vs
        in (myfvs, truefvs, ACon myfvs c vs e')
    setfvs tlds (ADef _ v e) = 
        let (efvs, etruefvs, e') = setfvs (tlds \\ [v]) e
            myfvs = efvs \\ [v]
            truefvs = etruefvs \\ [v]
        in  (myfvs, truefvs, ADef myfvs v e')

instance SetFVs (Alts a) (Alts [Var]) where
    setfvs tlds (Alts _ alts name) = 
        let (altsfvss, altstruefvss, alts') = unzip3 $ map (setfvs tlds) alts
            myfvs   = nub $ concat altsfvss
            truefvs = nub $ concat altstruefvss
        in (myfvs, truefvs, Alts myfvs alts' name)

-- FUN introduces scope
instance SetFVs (Obj a) (Obj [Var]) where
    setfvs tlds (FUN _ vs e n) = 
        let (efvs, etruefvs, e') = setfvs (tlds \\ vs) e -- shadow vs in tlds
            myfvs = efvs \\ vs
            truefvs = etruefvs \\ vs
        in (myfvs, truefvs, FUN myfvs vs e' n)

    -- PAP introduces a var
    setfvs tlds (PAP _ f as n) = 
        let (asfvs, astruefvs) = fvsof tlds as
            myfvs =  (nub $ f : asfvs) \\ tlds -- like EFCall
            truefvs = nub $ f : astruefvs
        in (myfvs, truefvs, PAP myfvs f as n)

    setfvs tlds (CON _ c as n) = 
        let (myfvs, truefvs) = fvsof tlds as
        in (myfvs, truefvs, CON myfvs c as n)

    setfvs tlds (THUNK _ e n) = 
        let (myfvs, truefvs, e') = setfvs tlds e
        in (myfvs, truefvs, THUNK myfvs e' n)

    setfvs tlds (BLACKHOLE _ n) = 
        ([], [], BLACKHOLE [] n)

--only interesting for non-recursive let
--instance SetFVs Def where
--    setfvs tlds (v, o) = (setfvs tlds o) \\ [v]

instance SetFVs [Obj a] [Obj [Var]] where
    setfvs tlds objs = 
        let names = map oname objs 
            (osfvss, ostruefvss, objs') = unzip3 $ map (setfvs tlds) objs
            myfvs =   nub $ concat osfvss     \\ names
            truefvs = nub $ concat ostruefvss \\ names
        in (myfvs, truefvs, objs')


