{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module SetFVs (
  setFVsObjs
) where

import Prelude
import AST
import Data.List as Data
import Data.Set as Set


-- *****************************************************

dropkey k = filter (\(k',_)->k==k')

-- for no top-level dead code elimination
-- close lhs rhs = rhs ++ [lhs] -- main at end
close -- transitive closure
  :: [([Char], ([[Char]], b))]
     -> [([Char], ([[Char]], b))] 
        -> [([Char], ([[Char]], b))]
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

setFVsObjs :: [String] -> [Obj a] -> [Obj (Set.Set Var, Set.Set Var)] -- monomorphism restriction
setFVsObjs runtimeGlobals defl
    -- tlds shadow runtime globals
    = let tlds = map oname defs
          defs' = map (setfvs (Set.fromList $ tlds ++ runtimeGlobals)) defs
          (myfvls, truefvls) = unzip $ map omd defs'
          myfvl = Set.toList $ Set.unions myfvls
      in case myfvl of
        [] -> deadCode $ zip (map (\\ runtimeGlobals) truefvls) defs'
        fvs -> error $ "SetFVs.setFVsDefs:  top level free variables:  " ++  intercalate " " fvs

-- vars introduced by EFCall f, PAP f, and EAtom Var v
-- scope introduced by FUN vs, x ->, C xi ->

class FVs a where fvsof :: Set.Set Var -> a -> (Set.Set Var, Set.Set Var)

instance FVs Atom where
    fvsof tlds (Var v) = 
        let vset = Set.singleton v 
        in (vset `Set.difference` tlds, vset)
    fvsof tlds _ = (Set.empty, Set.empty)

instance FVs [Atom] where
    fvsof tlds as = 
        let (xls, yls) = unzip $ map (fvsof tlds) as
        in (Set.union xls, Set.union yls)

-- setfvs both sets the metadata to the thing's nominal fvs (less tlds and globals) and true fvs
-- here "a" is e.g. "Expr ()" from the parser and "b" is "Expr (Set.Set Var, Set.Set Var))"

class SetFVs a b where 
    setfvs :: Set.Set Var -> a -> b

instance SetFVs (Expr a) (Expr (Set.Set Var, Set.Set Var)) where
    setfvs tlds (EAtom _ a) = 
        let (myfvs, truefvs) = fvsof tlds a
        in EAtom (myfvs,truefvs) a

    -- EFCall introduces a Var
    setfvs tlds (EFCall _ f as) = 
        let (asfvs, astruefvs) = fvsof tlds as
            myfvs   = (Set.singleton f `Set.union` asfvs) `Set.difference` tlds
            truefvs = Set.singleton f `Set.union` astruefvs
        in EFCall (myfvs,truefvs) f as

    setfvs tlds (EPrimop _ p as) = 
        let (myfvs, truefvs) = fvsof tlds as 
        in EPrimop (myfvs,truefvs) p as

    -- let introduces scope
    setfvs tlds (ELet _ defs e) = 
        let defs' = map (setfvs tlds) defs
            (defsfvls, defstruefvls) = unzip $ map omd defs'
            defsfvs =     Set.unions defsfvls
            defstruefvs = Set.unions defstruefvls
            names = Set.fromList map oname defs'
            e' = setfvs (Set.difference tlds names) e
            (efvs, etruefvs) = emd e'
            myfvs    = defsfvs     `Set.union` (Set.difference efvs names)
            truefvs  = defstruefvs `Set.union` (Set.difference efvs names)
        in ELet (myfvs,truefvs) defs' e'

    setfvs tlds (ECase _ e alts) = 
        let e' = setfvs tlds e
            (efvs, etruefvs) = emd e'
            alts' = setfvs tlds alts
            (altsfvs, altstruefvs) = altsmd alts'
            myfvs   = Set.union efvs     altsfvs
            truefvs = Set.union etruefvs altstruefvs
        in ECase (myfvs,truefvs) e' alts'

-- alts introduce scope
instance SetFVs (Alt a) (Alt (Set.Set Var, Set.Set Var)) where
    setfvs tlds (ACon _ c vs e) = 
        let vset = Set.fromList vs
            e' = setfvs (Set.difference tlds vset) e
            (efvs, etruefvs) = emd e'
            myfvs =   Set.difference efvs     vset
            truefvs = Set.difference etruefvs vset
        in ACon (myfvs,truefvs) c vs e'

    setfvs tlds (ADef _ v e) = 
        let vset = Set.singleton v
            e' = setfvs (Set.difference tlds vset) e
            (efvs, etruefvs) = emd e'
            myfvs =   Set.difference efvs     vset
            truefvs = Set.difference etruefvs vset
        in  ADef (myfvs,truefvs) v e'

instance SetFVs (Alts a) (Alts (Set.Set Var, Set.Set Var)) where
    setfvs tlds (Alts _ alts name) = 
        let alts' = map (setfvs tlds) alts
            (altsfvls, altstruefvls) = unzip $ map amd alts'
            myfvs   = Set.unions altsfvls
            truefvs = Set.unions altstruefvls
        in Alts (myfvs,truefvs) alts' name

-- FUN introduces scope
instance SetFVs (Obj a) (Obj (Set.Set Var, Set.Set Var)) where
    setfvs tlds (FUN _ vs e n) = 
        let vset = Set.fromList
            e' = setfvs (Set.difference tlds vset) e
            (efvs, etruefvs) = emd e'
            myfvs   = Set.difference efvs     vset
            truefvs = Set.difference etruefvs vset
        in FUN (myfvs,truefvs) vs e' n

    -- PAP introduces a var
    setfvs tlds (PAP _ f as n) = 
        let (asfvs, astruefvs) = fvsof tlds as
            fset = Set.singleton f
            myfvs   = (fset `Set.union` asfvs) `Set.difference` tlds
            truefvs = fset `Set.union` astruefvs
        in PAP (myfvs,truefvs) f as n

    setfvs tlds (CON _ c as n) = 
        CON (fvsof tlds as) c as n

    setfvs tlds (THUNK _ e n) = 
        let e' = setfvs tlds e
        in THUNK (emd e') e' n

    setfvs tlds (BLACKHOLE _ n) = 
        BLACKHOLE ([],[]) n
