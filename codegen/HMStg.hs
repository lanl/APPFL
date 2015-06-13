{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE ParallelListComp     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards      #-}

module HMStg (
  hmstg
) where

import ADT
import AST
import InfoTab
import BU
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State

-- unboxed primitives
monoBool = MCon False "B" []
monoInt  = MCon False "I" []
monoJunk = MCon False "not initialized" []

-- newtype State s a = State (s -> (a, s))

-- Atoms have types.  Except for Var v these are manifest.  Store
-- fresh type variables in the parent entity (EAtom, EFCall, EPrimop, PAP, CON)
-- encoded into the metadata Monotype.
-- In the lambda calculus we need a ftv for Var, App, and Lam.  Our Lam is FUN with
-- named variables, App is EFCall with a named caller, so we don't need a special
-- slot for type variables.  Atoms don't need the list of monomorphic type
-- variables.

-- EAtom  - emd is freshMonoVar or concrete type
-- EFCall - emd is function type

-- this is the entry point
hmstg tycons objs = error "hmstg not implemented"

-- distribute fresh type variables, monomorphic type variables, for BU in advance

errTyp = error "typ not defined"

getMono (Var v)  = freshMonoVar
getMono (LitI _) = return monoInt
getMono (LitB _) = return monoBool


class SetTyp a where
    setTyp :: Monotype -> a -> a
    getTyp :: a -> Monotype

instance SetTyp (Expr InfoTab) where
    -- is there a more idiomatic way to do this?
    setTyp m e = let it = emd e
                     it' = it{typ = m}
                 in e{emd = it'}
    getTyp = typ . emd

instance SetTyp (Alts InfoTab) where
    setTyp m e = let it = altsmd e
                     it' = it{typ = m}
                 in e{altsmd = it'}
    getTyp = typ . altsmd

instance SetTyp (Alt InfoTab) where
    setTyp m e = let it = amd e
                     it' = it{typ = m}
                 in e{amd = it'}
    getTyp = typ . amd

instance SetTyp (Obj InfoTab) where
    setTyp m e = let it = omd e
                     it' = it{typ = m}
                 in e{omd = it'}
    getTyp = typ . omd

class DV a b where
    dv :: a -> State Int b

instance DV (Expr InfoTab) (Expr InfoTab) where
    dv e@EAtom{ea} =
        do m <- getMono ea
           return $ setTyp m e
    dv e@EFCall{..} = 
        do retMono <- freshMonoVar -- return type
           ms <- mapM getMono eas   -- arg types
           let m = foldr MFun retMono ms -- function type
           return $ setTyp m e

    dv e@EPrimop{..} =
        let retMono = case eprimop of
                        Piadd -> monoInt
                        Pisub -> monoInt

                        Pieq  -> monoBool
                        -- etc.

        in do ms <- mapM getMono eas
              let m = foldr MFun retMono ms
              return $ setTyp m e

    dv e@ELet{..} =
        do edefs <- mapM dv edefs
           ee <- dv ee
           return ELet{..} -- error type already embedded

    dv ECase{..} =
        do ee <- dv ee
           ealts <- dv ealts
           return ECase{..} -- error type already embedded

instance DV (Alts InfoTab) (Alts InfoTab) where
    dv Alts{..} =
        do alts <- mapM dv alts
           return Alts{..}

instance DV (Alt InfoTab) (Alt InfoTab) where
    dv ADef{..} =
        do ae <- dv ae
           return ADef{..}
           
    dv a@ACon{amd,ac,ae} =
        do ae' <- dv ae
           -- stash type constructor TC b1 .. bn, bi fresh
           let TyCon boxed tcon tvs dcs = 
                   getTyConDefFromConstructor (dconMap amd) (tconMap amd) ac
           ntvs <- freshMonoVars $ length tvs
           let m = MCon boxed tcon ntvs
           return $ setTyp m a{ae = ae'}

instance DV (Obj InfoTab) (Obj InfoTab) where
    dv o@FUN{vs,e} =
        do bs <- freshMonoVars (length vs) -- one for each arg
           let m = MCon True "hack" bs  --hack, just hold them
           e' <- dv e
           return $ setTyp m o{e=e'}  --what's the precedence, if it mattered?

    dv o@PAP{f,as} = 
        do retMono <- freshMonoVar -- return type
           ms <- mapM getMono as
           let m = foldr MFun retMono ms
           return $ setTyp m o

    dv o@CON{omd,c,as} = 
        -- stash type constructor TC b1 .. bn, bi fresh
        let TyCon boxed tcon tvs dcs = 
                getTyConDefFromConstructor (dconMap omd) (tconMap omd) c
        in do ntvs <- freshMonoVars $ length tvs
              let m = MCon boxed tcon ntvs
              return $ setTyp m o



    dv o@THUNK{e} =
        do e' <- dv e
           return o{e = e'}

class BU a where
    bu :: (Set.Set Monotype) -> a -> (Assumptions, Constraints, a)

instance BU (Expr InfoTab) where
    bu mtvs e@EAtom{ea = (Var v), emd = ITAtom{typ}} =
        (Set.singleton (v,typ),
         Set.empty,
         e) -- EAtom monotype set in dv

    bu mtvs e@EAtom{} =
        (Set.empty,
         Set.empty,
         e) -- EAtom monotype set in dv

    bu mtvs e@EFCall{emd = ITFCall{typ},ev,eas} =
        let (m,ms) = unfoldr typ -- m is return type
        in (Set.fromList $ (ev, typ) : [ (v, t) | (Var v, t) <- zip eas ms ],
            Set.empty,
            e) -- EFCall monotypes set in dv

    bu mtvs e@EPrimop{emd = ITPrimop{typ}, eprimop, eas} =
        let (m,ms) = unfoldr typ -- get atoms assocs, m is result type
            pts = case eprimop of
                    Piadd -> [monoInt,monoInt]
                    Pisub -> [monoInt,monoInt]
                    Pieq  -> [monoInt,monoInt]
            as = Set.fromList [(v,m) | (Var v, m) <- zip eas ms] --drop LitX cases
            cs = Set.fromList [EqC m1 m2 | m1 <- ms | m2 <- pts]
        in (as, cs, setTyp m e)

    bu mtvs e@ELet{edefs,ee} = 
        let (as, cs, edefs') = buRec mtvs edefs
            onames = map oname edefs'
            ots = map getTyp edefs'
            (asee, csee, ee') = buIn mtvs (zip onames ots) ee
        in (foldr dropAss (Set.union as asee) onames,
            Set.union cs csee,
            setTyp (getTyp ee') e{edefs = edefs',
                                  ee = ee'})

    bu mtvs e@ECase{ee,ealts} =
        let (asee, csee, ee')    = bu mtvs ee
            (asas, csas, ealts') = butAlts (getTyp ee') mtvs ealts
            talts = getTyp ealts
        in (Set.union asee asas,
            csee `Set.union` csas,
            setTyp talts e{ee = ee',
                           ealts = ealts'})

buRec mtvs os =
    let (ass, css, os') = unzip3 $ map (bu mtvs) os
        xts = zip (map oname os) (map getTyp os')
        ncs = Set.fromList
              [ EqC t' t |
                (x,t) <- xts, as <- ass, (x',t') <- Set.toList as, x==x' ]
        cs = foldr Set.union ncs css
        as = foldr Set.union Set.empty ass
    in (as, cs, os')

buIn mtvs xts e = 
    let (as, cs, e') = bu mtvs e
        ncs = Set.fromList [ ImpC t' mtvs t 
                             | (x,t) <- xts, (x',t') <- Set.toList as, x == x' ]
    in (as, Set.union cs ncs, e')

butAlts t0 mtvs e@Alts{alts} =
      let (ass, css, alts') = unzip3 $ map (butAlt t0 mtvs) alts
          (a:as) = alts'
          ncs = Set.fromList [EqC (getTyp a) (getTyp a') | a'<-as]
      in (foldr1 Set.union ass,
          foldr Set.union ncs css,
          setTyp (getTyp $ head alts') e{alts = alts'}) -- any one will do

butAlt t0 mtvs e@ADef{av,ae} =
    -- av monomorphic:  mtvs and EqC
    let (as, cs, ae') = bu (Set.union mtvs $ Set.singleton (MVar av)) ae  
        ncs = Set.fromList [ EqC t0 t | (x,t) <- Set.toList as, x == av ]
    in (dropAss av as,
        Set.union cs ncs,
        setTyp (getTyp ae') e{ae = ae'})

butAlt t0 mtvs e@ACon{amd,ac,avs,ae} =
    let -- instantiate type constructor definition
        -- get type constructor definition
        TyCon boxed tcon tvs dcs = 
            getTyConDefFromConstructor (dconMap amd) (tconMap amd) ac
        -- get data constructor definition C [Monotype]
        [ms] = [ ms | DataCon b c ms <- dcs, c == ac ] -- ms :: [Monotype]
        -- instantiate those Monotypes 
        MCon boxed' c ntvs = getTyp e --stashed by dv
        subst = Map.fromList $ zip tvs ntvs
        tis = apply subst ms
        xtis = zip avs tis
        -- note ntvs == freeVars tis (as sets) so ntvs are new mtvs
        (as,cs,ae') = bu (Set.union mtvs $ Set.fromList ntvs) ae
        ncs = Set.fromList $ (EqC t0 $ getTyp e) : 
                             [ EqC ti t' | (xi,ti) <- xtis, 
                                           (x,t') <- Set.toList as, 
                                           x == xi ]
    in (foldr dropAss as avs,
        Set.union cs ncs,
        setTyp (getTyp ae') e{ae = ae'})

instance BU (Obj InfoTab) where
  bu mtvs o@FUN{vs,e} = 
      -- get new type vars for args
      let MCon _ _ ntvs = getTyp o
          (as,cs,e') = bu (Set.union mtvs $ Set.fromList ntvs) e
          xbis = zip vs ntvs
          ncs = Set.fromList [EqC t' bi | (xi,bi) <- xbis, (x,t') <- Set.toList as]
          typ = foldr MFun (getTyp e') ntvs
      in (foldr dropAss as vs,
          Set.union cs ncs,
          setTyp typ o{e = e'})

  bu mtvs o@PAP{f,as} =
      let typ = getTyp o
          (m,ms) = unfoldr typ
      in (Set.fromList $ (f,typ) : [ (v, t) | (Var v, t) <- zip as ms ],
          Set.empty,
          o)

  bu mtvs CON{} = error ""

  bu mtvs o@THUNK{e} =
      let (as,cs,e') = bu mtvs e
      in (as, cs, setTyp (getTyp e') o{e = e'})


  
-- m is rightmost element
unfoldr m@(MVar _) = (m,[])
unfoldr m@(MCon{}) = (m,[])
unfoldr (MFun m1 m2) = let (m,ms) = unfoldr m2 in (m, m1:ms)

