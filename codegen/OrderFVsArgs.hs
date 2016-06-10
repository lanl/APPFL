{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}

module OrderFVsArgs(
  orderFVsArgs
) where

import Util
import AST
import ADT
import InfoTab
import CMap(instantiateDataConAt)
import qualified Data.Map as Map


-- typeFVs is a poor choice of name
orderFVsArgs :: [Obj InfoTab] -> [Obj InfoTab]
orderFVsArgs = typeFVs Map.empty

-- replace bogus type with correct type, sort by boxedness, Var flavor
repv m vts = boxedFirst [(k, f k) | (k,_) <- vts]
    where f k = case Map.lookup k m of
                  Nothing -> error $ "HMStg.repv:  unbound variable " ++ show k ++
                                     " " ++ show m
                  Just m -> m

-- replace bogus type with correct type, sort by boxedness, Atom flavor
repa m ats = boxedFirst [(k, f k) | (k,_) <- ats]
    where f a = case a of
                  Var v ->
                      case Map.lookup v m of
                        Nothing -> error $ "HMStg.repa:  unbound variable " ++
                                           show v ++ " " ++ show m
                        Just m -> m
                  LitI{} -> biIntMCon
                  LitL{} -> biLongMCon
                  LitF{} -> biFloatMCon
                  LitD{} -> biDoubleMCon
                  LitC c -> MCon False c []

-- partition (a,Monotype) pairs into boxed and unboxed, preserving order
-- also return  perm such that old[i] = new[perm[i]]
boxedFirst xs =
    let (bcount, parted, perm) = partPerm (isBoxed . snd) xs
    in (bcount, length parted - bcount, parted, perm)

-- order fvs boxed first, set bfvc, ufvc in InfoTab
-- order args for PAP, CON boxed first, set bargc, uargc, argPerm in InfoTab
-- make valid types for fvs, and PAP and CON args

-- typeFVsCommon* handles just FVs directly, deferring subtrees and arguments
-- of PAP and CON to typeFVs

class TypeFVs a where
    typeFVs :: Map.Map Var Monotype -> a -> a

-- This is the entry point because typeFVs augments the type environment.
-- augmenting type environment is done at
--   [Obj] (top level), FUN, ELet, EFCall, ACon, ADef
instance TypeFVs [Obj InfoTab] where
    typeFVs m os =
        let m' = foldr (uncurry Map.insert) m [(oname o, (typ . omd) o) | o <- os]
        in map (typeFVsCommon m') os

instance TypeFVs (Obj InfoTab) where
    typeFVs m o@FUN{vs,e} =
        let (rt, vstyps) = unfoldr $ (typ . omd) o
            m' = foldr (uncurry Map.insert) m (zip vs vstyps)
        in o{e = typeFVsCommon m' e}

    typeFVs m o@PAP{omd,as} =
        let (bc, uc, args', perm) = repa m $ zzip (map fst $ args omd)
                                                  (map (typ . emd) as)
        in o{omd = omd{args = args',
                       bargc = bc,
                       uargc = uc,
                       argPerm = perm},
             as = map (typeFVsCommon m) as}

    typeFVs m o@CON{omd,as} =
        let (bc, uc, args', perm) = repa m $ zzip (map fst $ args omd)
                                                  (map (typ . emd) as)
        in o{omd = omd{args = args',
                       bargc = bc,
                       uargc = uc,
                       argPerm = perm},
             as = map (typeFVsCommon m) as}

    typeFVs m o@THUNK{omd, e} =
        o{e = typeFVsCommon m e}

--BH    typeFVs m o@BLACKHOLE{omd} = o

instance TypeFVs [Expr InfoTab] where
    typeFVs m = map (typeFVsCommon m)

instance TypeFVs (Expr InfoTab) where
    typeFVs m e@EAtom{} = e

    typeFVs m e@EFCall{ev, eas} =
        let eastyps = map (typ . emd) eas
            etyp = (typ . emd) e
            evtyp = foldr MFun etyp eastyps
            m' = Map.insert ev evtyp m
            eas' = map (typeFVsCommon m') eas
        in e{eas = eas'}

    typeFVs m e@EPrimop{eas} =
        e{eas = map (typeFVsCommon m) eas}

    typeFVs m e@ELet{edefs, ee} =
        let edefs' = typeFVs m edefs -- [Obj InfoTab] entry point
            m' = foldr (uncurry Map.insert) m [(oname o, typ $ omd o) | o <- edefs]
            ee' = typeFVsCommon m' ee
        in e{edefs = edefs',
             ee = ee'}

    typeFVs m e@ECase{ee, ealts} =
        let ee' = typeFVsCommon m ee
            ealts' = typeFVsCommonAlts (typ $ emd ee) m ealts
        in e{ee = ee', ealts = ealts'}

typeFVsAlts t0 m a@Alts{alts} =
    let alts' = map (typeFVsCommonAlt t0 m) alts
    in a{alts = alts'}

typeFVsAlt t0 m a@ADef{av, ae} =
    let m' = Map.insert av t0 m
        ae' = typeFVsCommon m' ae
    in a{ae = ae'}

typeFVsAlt t@(MCon b tc ms0) m a@ACon{amd, ac, avs, ae} =
    let tis = instantiateDataConAt ac (cmap amd) ms0
        m' = foldr (uncurry Map.insert) m (zip avs tis)
        ae' = typeFVsCommon m' ae
    in a{ae = ae'}

--typeFVsAlt (MPrim _) m a@ACon{ac, avs, ae} =
--    let ae' = typeFVsCommon m ae
--    in a{ae = ae'}

typeFVsAlt t0 m a@ACon{amd, ac, avs, ae} =
    error $ "HMStg.typeFVsAlt: " ++ show t0 ++ " " ++ show a


class TypeFVsCommon a where
    typeFVsCommon :: Map.Map Var Monotype -> a -> a

instance TypeFVsCommon (Obj InfoTab) where
    typeFVsCommon m o =
        let (bc, uc, fvs', _) = repv m (fvs $ omd o)
            o' = o{omd = (omd o){fvs=fvs', bfvc=bc, ufvc=uc}}
        in typeFVs m o'

instance TypeFVsCommon (Expr InfoTab) where
    typeFVsCommon m e =
        let (bc, uc, fvs', _) = repv m (fvs $ emd e)
            e' = e{emd = (emd e){fvs=fvs', bfvc=bc, ufvc=uc}}
        in typeFVs m e'

typeFVsCommonAlts t0 m alts =
        let (bc, uc, fvs', _) = repv m (fvs $ altsmd alts)
            alts' = alts{altsmd = (altsmd alts){fvs=fvs', bfvc=bc, ufvc=uc}}
        in typeFVsAlts t0 m alts'

typeFVsCommonAlt t0 m a =
        let (bc, uc, fvs', _) = repv m (fvs $ amd a)
            a' = a{amd = (amd a){fvs=fvs', bfvc=bc, ufvc=uc}}
        in typeFVsAlt t0 m a'
