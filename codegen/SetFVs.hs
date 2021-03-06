{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE CPP #-}

module SetFVs (
  setFVsObjs, tc
) where

import Prelude
import AST
import PPrint
import qualified Data.List as List
import qualified Data.Set as Set
import Debug.Trace
-- *****************************************************

dropkey k = filter (\(k',_)->k==k')

-- transitive closure
-- i.e. given a graph in adjacency list (or Set, in this case) form
--   what nodes (type 'a' below) in the rhs can be reached
--   from the those in the lhs
tc  ::
  (Show a, Ord a)
  => Set.Set a        -- ^ lhs, "roots" of connectivity
  -> [(a, Set.Set a)] -- ^ rhs, list of (node, neighbors) pairs
  -> Set.Set a        -- ^ set of all reachable nodes from roots

tc lhs rhs = tc' lhs lhs rhs
    where
      tc' acc newroots rhs =
          let fvs = Set.unions [ s | (v, s) <- rhs, Set.member v newroots ]
                    `Set.difference` acc
          in case Set.null fvs of
               True -> Set.union acc newroots -- done
               _ -> let bvs = Set.fromList $ map fst rhs
                        undefs = Set.difference fvs bvs
                    in case Set.null undefs of
                         False -> error $ "SetFVs.tc:  free vars " ++ show undefs
                         _ -> let (rhsl, rhsr) = List.partition (\(v,_) -> Set.member v fvs) rhs
                              in tc' (Set.unions [acc, newroots, fvs])
                                     (Set.unions $ map snd rhsl)
                                     rhsr

rhsTest = [(1, [1,2]),
           (1, []),
           (2, [5]),
           (3, [1]),
           (4, []),
           (5, [3])]
lhsTest = [1]
a = tcList lhsTest rhsTest

tcList :: (Show a, Ord a) => [a] -> [(a, [a])] -> [a]

tcList roots rhs = Set.toList $ tc (Set.fromList roots) [(v, Set.fromList l) | (v,l) <- rhs]

-- if there's no main don't do anything
deadCode defs =
    let onamel = map oname defs
        onames = Set.fromList onamel
    in case Set.member "main" onames of
         False -> defs
         True -> let fvls = map (\o -> snd (omd o) `Set.intersection` onames) defs
                     namefvs = zip onamel fvls
                     closeds = tc (Set.singleton "main") namefvs
                 in [ o | o <- defs, oname o `Set.member` closeds ]


-- after rename, make the fvs meaningful
-- TLDs are not considered free vars in expressions

setFVsObjs :: [String] -> [Obj ()] -> [Obj ([Var],[Var])] -- monomorphism restriction

setFVsObjs runtimeGlobals objs =
    -- use Set internally
    map stgToList $ toplevel (Set.fromList runtimeGlobals) objs

class STGToList a b where
    stgToList :: a -> b

p2p (s1,s2) = (Set.toList s1, Set.toList s2)

instance STGToList (Obj (Set.Set Var, Set.Set Var)) (Obj ([Var],[Var])) where
    stgToList FUN{..} = FUN{omd = p2p omd, e = stgToList e, ..}
    stgToList PAP{..} = PAP{omd = p2p omd, as = map stgToList as, ..}
    stgToList CON{..} = CON{omd = p2p omd, as = map stgToList as, ..}
    stgToList THUNK{..} = THUNK{omd = p2p omd, e = stgToList e, ..}
--BH     stgToList BLACKHOLE{..} = BLACKHOLE{omd = p2p omd,..}

instance STGToList (Expr (Set.Set Var, Set.Set Var)) (Expr ([Var],[Var])) where
    stgToList EAtom{..} = EAtom{emd = p2p emd, ..}
    stgToList EFCall{..} = EFCall{emd = p2p emd, eas = map stgToList eas, ..}
    stgToList EPrimOp{..} = EPrimOp{emd = p2p emd, eas = map stgToList eas, ..}
    stgToList ELet{..} = ELet{emd = p2p emd, edefs = map stgToList edefs, ee = stgToList ee, ..}
    stgToList ECase{..} = ECase{emd = p2p emd, ee = stgToList ee, ealts = stgToList ealts, ..}

instance STGToList (Alts (Set.Set Var, Set.Set Var)) (Alts ([Var],[Var])) where
    stgToList Alts{..} = Alts{altsmd = p2p altsmd,
                              alts = map stgToList alts,
                              scrt = stgToList scrt, ..}

instance STGToList (Alt (Set.Set Var, Set.Set Var)) (Alt ([Var],[Var])) where
    stgToList ACon{..} = ACon{amd = p2p amd, ae = stgToList ae, ..}
    stgToList ADef{..} = ADef{amd = p2p amd, ae = stgToList ae, ..}

showFVs vs = "[" ++ List.intercalate " " vs ++ "] "

instance Show ([Var],[Var]) where
    show (a,b) = "(" ++ showFVs a ++ "," ++ showFVs b ++ ")"

instance Unparse ([Var],[Var]) where
  unparse (a,b) = parens $
                  brackList (map stgName a)
                  <> comma <+>
                  brackList (map stgName b)

toplevel :: (Set.Set Var) -> [Obj a] -> [Obj (Set.Set Var, Set.Set Var)]
toplevel rtgs defs =
    -- tlds shadow runtime globals
    let tlds = Set.fromList $ map oname defs
        defs' = setfvs (Set.union tlds rtgs) defs
        (myfvls, truefvls) = unzip $ map omd defs'
        myfvl = Set.toList $ Set.unions myfvls

    in case myfvl of
         [] -> deadCode defs'
         fvs -> error $ unlines
                [ "SetFVs.setFVsDefs:  top level free variables:  "
                , List.intercalate " " fvs, "" -- extra line
                , show $ objListDoc $ filter (not . null . fst . omd) defs']

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
        in (Set.unions xls, Set.unions yls)

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
        let as'                = map (setfvs tlds) as
            (easFVs, easTFVs)  = unzip $ map emd as'
            (asfvs, astruefvs) = (Set.unions easFVs, Set.unions easTFVs)
            myfvs              = (Set.singleton f `Set.union` asfvs) `Set.difference` tlds
            truefvs            = Set.singleton f  `Set.union` astruefvs
        in EFCall (myfvs,truefvs) f as'


    setfvs tlds (EPrimOp _ p i as) =
        let as' = map (setfvs tlds) as
            (easFVs, easTFVs) = unzip $ map emd as'
            (myfvs, truefvs) = (Set.unions easFVs, Set.unions easTFVs)
        in EPrimOp (myfvs,truefvs) p i as'

    -- let introduces scope
    setfvs tlds (ELet _ defs e) =
        let names = Set.fromList $ map oname defs
            defs' = setfvs (Set.difference tlds names) defs
            e'    = setfvs (Set.difference tlds names) e
            (defsfvls, defstruefvls) = unzip $ map omd defs'
            defsfvs     = Set.unions defsfvls
            defstruefvs = Set.unions defstruefvls
            (efvs, etruefvs) = emd e'
            myfvs    = (defsfvs     `Set.union` efvs)     `Set.difference` names
            truefvs  = (defstruefvs `Set.union` etruefvs) `Set.difference` names
        in ELet (myfvs,truefvs) defs' e'

    setfvs tlds (ECase _ e alts) =
        let e' = setfvs tlds e
            (efvs, etruefvs) = emd e'
            alts' = setfvs tlds alts
            (altsfvs, altstruefvs) = altsmd alts'
            myfvs   = Set.union efvs altsfvs
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

-- alts block introduces scope with scrutinee binding
instance SetFVs (Alts a) (Alts (Set.Set Var, Set.Set Var)) where
    setfvs tlds (Alts _ alts name scrt) =
        let vset = Set.singleton $ scrtVarName scrt
            -- I'm not positive there's much meaning to the notion of a
            -- free variable in the scrt (EAtom type), but it's set here
            -- for completeness. -dmr
            scrt' = setfvs tlds scrt
            alts' = setfvs (tlds Set.\\ vset) alts
            (altsfvls, altstruefvls) = unzip $ map amd alts'
            -- variable bound to scrutinee is not free in Alts block
            myfvs   = Set.unions altsfvls Set.\\ vset
            truefvs = Set.unions altstruefvls Set.\\ vset
        in Alts (myfvs,truefvs) alts' name scrt'


instance SetFVs (Obj a) (Obj (Set.Set Var, Set.Set Var)) where
  -- FUN introduces scope
  setfvs tlds (FUN _ vs e n) =
    let vset             = Set.fromList vs
        e'               = setfvs (Set.difference tlds vset) e
        (efvs, etruefvs) = emd e'
        myfvs            = Set.difference efvs     vset
        truefvs          = Set.difference etruefvs vset
    in FUN (myfvs,truefvs) vs e' n

  -- PAP introduces a var
  setfvs tlds (PAP _ f as n) =
    let as' = map (setfvs tlds) as
        (asFVs, asTFVs) = unzip $ map emd as'
        (asfvs, astruefvs) = (Set.unions asFVs, Set.unions asTFVs)
        fset = Set.singleton f
        myfvs   = (fset `Set.union` asfvs) `Set.difference` tlds
        truefvs = fset `Set.union` astruefvs
    in PAP (myfvs,truefvs) f as' n

  setfvs tlds (CON _ c as n) =
    let as' = map (setfvs tlds) as
        (asFVs, asTFVs) = unzip $ map emd as'
    in CON (Set.unions asFVs, Set.unions asTFVs) c as' n

  setfvs tlds (THUNK _ e n) =
    let e' = setfvs tlds e
    in THUNK (emd e') e' n

  --BH setfvs tlds (BLACKHOLE _ n) =
  --BH   BLACKHOLE (Set.empty,Set.empty) n


instance SetFVs a b => SetFVs [a] [b] where
    setfvs tlds = map (setfvs tlds)

instance PPrint (Set.Set Var, Set.Set Var) where
  pprint (fvs, tfvs) = parens (text "fvs:" <+> pprint fvs <> comma <+> text "tfvs:" <+> pprint tfvs)
