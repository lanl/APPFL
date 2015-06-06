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

instance DV (Obj InfoTab) (Obj InfoTab) where
    dv o@FUN{..} =
        do ts <- freshMonoVars (length vs) -- one for each arg
           let m = MCon True "hack" ts  --hack, just hold them
           e' <- dv e
           return $ setTyp m o{e=e'}  --what's the precedence, if it mattered?

instance DV (Alts InfoTab) (Alts InfoTab) where
    dv Alts{..} =
        do alts <- mapM dv alts
           return Alts{..}

instance DV (Alt InfoTab) (Alt InfoTab) where
    dv a@ADef{} =
        do m <- freshMonoVar
           return $ setTyp m a
           
    dv a@ACon{amd=it,..} =
        let dConMap = dconMap it :: DataConMap
            tConMap = tconMap it :: TyConMap
            Just dataConParam = Map.lookup ac dConMap :: Maybe DataConParam
            tyConName = dtycon dataConParam :: Con
            Just tyConParam = Map.lookup tyConName tConMap :: Maybe TyConParam
            TyCon boxed tcon tvs dcs = tycon tyConParam :: TyCon
        -- get constructor defn
            [m] = [ m | DataCon b c m <- dcs, c == ac ]
        in do ntvs <- freshMonoVars $ length tvs
              let subst = Map.fromList $ zip tvs ntvs
              -- hack:  this is the instantiation of a type constructor,
              -- i.e. a list of monotypes, one for each xi in 
              -- pattern C x1 .. xn
              let instantiation = MCon boxed ac (apply subst m)
              return $ setTyp instantiation a

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

    bu mtvs e@EFCall{emd = ITFCall{typ},ev} =
        (Set.singleton (ev, typ), -- mixture of fresh vars and monoXtype
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
            (asas, csas, ealts') = butAlts (getTyp ee) mtvs ealts
            talts = getTyp ealts
        in (Set.union asee asas,
            csee `Set.union` csas 
                 `Set.union` Set.singleton (EqC (getTyp ee') talts),
            setTyp talts e{ee = ee',
                           ealts = ealts'})

buRec mtvs os =
    let (ass, css, os') = unzip3 $ map (bu mtvs) os
        ias = zip [0..] ass
        ixts = zip3 [0..] (map oname os) (map getTyp os')
        ncs = Set.fromList
              [ if (i==j) then EqC t' t else ImpC t' mtvs t |
                (i,x,t) <- ixts, (j,as) <- ias, (x',t') <- Set.toList as, x==x' ]
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
          cs = Set.fromList [EqC (getTyp a) (getTyp a') | a'<-as]
      in (foldr1 Set.union ass,
          foldr Set.union cs css,
          setTyp (getTyp $ head alts') e{alts = alts'}) -- any one will do

butAlt t0 mtvs e@ADef{av,ae} =
    let (as, cs, ae') = bu mtvs ae  -- av monomorphic or polymorphic???
        ncs = Set.fromList [ EqC t0 t | (x,t) <- Set.toList as, x == av ]
    in (dropAss av as,
        Set.union cs ncs,
        setTyp (getTyp ae') e{ae = ae'})

butAlt t0 mtvs e@ACon{} = error ""

instance BU (Obj InfoTab) where
  bu = error ""


  
-- m is rightmost element
unfoldr m@(MVar _) = (m,[])
unfoldr m@(MCon{}) = (m,[])
unfoldr (MFun m1 m2) = let (m,ms) = unfoldr m2 in (m, m1:ms)

