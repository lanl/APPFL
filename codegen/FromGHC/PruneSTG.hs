{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module FromGHC.PruneSTG where

import           CoreSyn ( AltCon (..) )
import           StgSyn
                 ( StgBinding, GenStgBinding (..)
                 , StgExpr, GenStgExpr (..), StgOp (..)
                 , StgRhs, GenStgRhs (..)
                 , StgArg, GenStgArg (..)
                 , StgAlt, AltType (..)
                 )
import           Var (Id)
import           Name

import qualified Data.Set as Set
import           Data.List (find)

import SetFVs (tc)



instance Show Name where
  show = getOccString


unrec :: [StgBinding] -> [(Id, StgRhs)]
unrec binds = concatMap getPairs binds
  where getPairs (StgNonRec b rhs) = [(b,rhs)]
        getPairs (StgRec pairs) = pairs

        
pruneGhcStg :: [StgBinding] -> [StgBinding]
pruneGhcStg bindings =
  let bndrPairs = unrec bindings
      tlds = map (getName . fst) bndrPairs
      tldset = Set.fromList tlds
      refSets = map (tlRefs tldset . snd) bndrPairs
      lhs = case find (("main" ==) . getOccString) tlds of
              Just main -> Set.singleton main
              Nothing   -> tldset
      prunedNames = tc lhs (zip tlds refSets)      
      reachable (StgRec pairs) = any ((`elem` prunedNames) . getName . fst) pairs
      reachable (StgNonRec id _) = getName id `elem` prunedNames
  in filter reachable bindings
            


class TLRefs a where
  tlRefs :: Set.Set Name -> a -> Set.Set Name
  
instance TLRefs StgRhs where
  tlRefs tlds rhs =
    case rhs of
      StgRhsClosure _ _ _ _ _ args expr ->
        let newNames = Set.fromList $ map getName args
            tlds1 = tlds Set.\\ newNames
        in tlRefs tlds1 expr

      StgRhsCon _ dc args ->
        let refs = Set.unions $ map (tlRefs tlds) args in
          refs `Set.union` tlds `Set.intersection` Set.singleton (getName dc)


instance TLRefs StgExpr where
  tlRefs tlds expr =
    case expr of
      StgApp fun args ->
        let refs = Set.unions $ map (tlRefs tlds) args in
          refs `Set.union` tlds `Set.intersection` Set.singleton (getName fun)

      StgConApp dc args ->
        let refs = Set.unions $ map (tlRefs tlds) args in
          refs `Set.union` tlds `Set.intersection` Set.singleton (getName dc)


      StgOpApp op args _ ->
        tlds `Set.intersection` Set.unions (map (tlRefs tlds) args)

      StgCase scrt _ _ bind _ _ alts ->
        let refs  = tlRefs tlds scrt
            tlds1 = tlds Set.\\ Set.singleton (getName bind)
        in Set.unions (refs : map (tlRefs tlds1) alts)

      StgLet             binds body -> tlRefsLet binds body
      StgLetNoEscape _ _ binds body -> tlRefsLet binds body

      StgTick _ realExpr -> tlRefs tlds realExpr

      _ -> Set.empty

    where tlRefsLet binds body =
            let bndrPairs = unrec [binds]
                newNames = Set.fromList $ map (getName . fst) bndrPairs
                tlds1 = tlds Set.\\ newNames
                bodyRefs = tlRefs tlds1 body
            in Set.unions (bodyRefs : map (tlRefs tlds1 . snd) bndrPairs)


instance TLRefs StgAlt where
  tlRefs tlds (acon, binds, _, rhs) =
    let newNames = Set.fromList (map getName binds)
        tlds1    = tlds Set.\\ newNames
        refs     = tlRefs tlds1 rhs
    in case acon of
         DataAlt dc
           | dcname <- getName dc,
             dcname `Set.member` tlds1 -> Set.insert dcname refs

         _ -> refs
    
instance TLRefs StgArg where
  tlRefs tlds arg = case arg of
    StgVarArg id | argname <- getName id,
                   argname `Set.member` tlds -> Set.singleton argname

    _ -> Set.empty
  
  
      
