{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module HMStg (
  hmstgdebug,
  hmstg,
  hmstgAssums,
  hmstgAssumsdebug,
  showObjs
) where

import Util
import ADT
import PPrint
import CMap
import AST
import InfoTab
import BU
import SCC
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (intercalate)
--import Debug.Trace
import WiredIn

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
--          ([Assumption], [(TyVar, Monotype)], [Constraint], String)

hmstgAssums :: [Obj InfoTab] -> Assumptions -> [Obj InfoTab]
hmstgAssums os0 assums =
    let (os1,i1) = runState (dv os0) 0
        (as,cs,os2) = buNest Set.empty os1 assums :: (Set.Set Assumption,
                                                      Set.Set Constraint,
                                                      [Obj InfoTab])
        (subst, _) = runState (solve cs) i1
--    in typeFVs Map.empty $ co Set.empty $ backSub subst os2 :: [Obj InfoTab]
--    in let res = typeFVs Map.empty $ co Set.empty $ backSub subst os2 :: [Obj InfoTab]
    in let res = co Set.empty $ backSub subst os2 :: [Obj InfoTab]
       in if res == res then res else error "this shouldn't happen"
    -- Note for future reader: res == res forces evaluation and will catch
    --   any errors that laziness hides

hmstg :: [Obj InfoTab] -> [Obj InfoTab]
hmstg os = hmstgAssums os Set.empty


hmstgdebug :: [Obj InfoTab] -> IO ()
hmstgdebug os = hmstgAssumsdebug os Set.empty

hmstgAssumsdebug :: [Obj InfoTab] -> Assumptions -> IO ()
hmstgAssumsdebug os0 assums =
    let (os1,i1) = runState (dv os0) tyVarsUsed
        (as,cs,os2) = buNest Set.empty os1 assums :: (Set.Set Assumption,
                                                      Set.Set Constraint,
                                                      [Obj InfoTab])
        (subst, _) = runState (solve cs) i1
    in
--       putStrLn $ show $ vcat $ map (text . show) $ Set.toList cs
       putStrLn $ showObjs (co Set.empty $ backSub subst os2)


-- distribute fresh type variables, monomorphic type variables, for BU in advance

errTyp = error "typ not defined"

class SetTyp a where
    setTyp :: Monotype -> a -> a
    getTyp :: a -> Monotype

instance SetTyp (Expr InfoTab) where
    -- is there a more idiomatic way to do this?--if emd = omd etc.?
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


class SetCTyp a where
    setCTyp :: Polytype -> a -> a
    getCTyp :: a -> Polytype

instance SetCTyp (Expr InfoTab) where
    -- is there a more idiomatic way to do this?--if emd = omd etc.?
    setCTyp m e = let it = emd e
                      it' = it{ctyp = m}
                  in e{emd = it'}
    getCTyp = ctyp . emd

instance SetCTyp (Alts InfoTab) where
    setCTyp m e = let it = altsmd e
                      it' = it{ctyp = m}
                  in e{altsmd = it'}
    getCTyp = ctyp . altsmd

instance SetCTyp (Alt InfoTab) where
    setCTyp m e = let it = amd e
                      it' = it{ctyp = m}
                  in e{amd = it'}
    getCTyp = ctyp . amd

instance SetCTyp (Obj InfoTab) where
    setCTyp m e = let it = omd e
                      it' = it{ctyp = m}
                  in e{omd = it'}
    getCTyp = ctyp . omd


class DV a b where
    dv :: a -> State Int b

instance DV (Expr InfoTab) (Expr InfoTab) where
    dv e@EAtom{emd, ea} =
        do m <- case ea of
                  Var v  -> freshMonoVar
                  LitI _ -> pure $ MPrim PInt
                  LitD _ -> pure $ MPrim PDouble
                  LitStr s -> pure $ MPrim PString
                  LitC c -> case maybeCMap emd of
                              Nothing -> error "dv LitC maybeCMap is Nothing"
                              Just cmap -> return $
                                           let (TyCon boxed tname _ _) =
                                                  luTCon c cmap
                                           in case boxed of
                                                True -> error $ "dv:  boxed constructor " ++
                                                                c ++ " cannot be literal"
                                                False -> MCon (Just False) tname []
           return $ setTyp m e

    dv e@EFCall{eas} =
        do retMono <- freshMonoVar -- return type
           eas' <- mapM dv eas -- setTyp(s) of args
           return $ setTyp retMono e{eas = eas'}

    dv e@EPrimOp{..} =
          let (retMono,_) = unfoldMTy $ opType eopInfo
          in do eas' <- mapM dv eas -- setTyp(s) of Primop args
                let e' = e{eas = eas'}
                return $ setTyp retMono e'

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
           scrt <- dv scrt
           return Alts{..}

instance DV (Alt InfoTab) (Alt InfoTab) where
    dv ADef{..} =
        do ae <- dv ae
           return ADef{..}

    dv a@ACon{amd,ac,ae} =
        do ae' <- dv ae
           -- stash type constructor TC b1 .. bn, bi fresh
           let t@(TyCon boxed tcon tvs dcs) =
                 luTCon ac $ cmap amd -- NEW
           ntvs <- freshMonoVars $ length tvs
           let m = MCon (Just boxed) tcon ntvs

           return $ setTyp m a{ae = ae'}

instance DV (Obj InfoTab) (Obj InfoTab) where
    dv o@FUN{vs,e} =
        do bs <- freshMonoVars (length vs) -- one for each arg
           let m = MCon (Just True) "hack" bs  --hack, just hold them
           e' <- dv e
           return $ setTyp m o{e=e'}  --what's the precedence, if it mattered?

    dv o@PAP{f,as} = -- identical to EFCall
        do retMono <- freshMonoVar -- return type
           as' <- mapM dv as
           return $ setTyp retMono o{as=as'}

    dv o@CON{omd,c,as} =

        -- stash type constructor TC b1 .. bj, bi fresh
        let TyCon boxed tcon tvs dcs = luTCon c $ cmap omd
        in do as' <- mapM dv as
              ntvs <- freshMonoVars $ length tvs
              let m = MCon (Just boxed) tcon ntvs
              return $ setTyp m o{as=as'}

    dv o@THUNK{e} =
        do e' <- dv e
           return o{e = e'} -- don't need setTyp as bu is done on e

--BH    dv o@BLACKHOLE{} =
--BH        do typ <- freshMonoVar
--BH           return $ setTyp typ o

instance DV [Obj InfoTab] [Obj InfoTab] where
  dv = mapM dv

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

    bu mtvs e@EFCall{ev,eas} =
        let (ass, css, eas') = unzip3 $ map (bu mtvs) eas
            ftyp = foldr (MFun . getTyp) (getTyp e) eas'
        in (Set.insert (ev,ftyp) $ Set.unions ass,
            Set.unions css,
            e{eas=eas'}) -- EFCall monotype set in dv

    bu mtvs e@EPrimOp{emd = ITPrimop{typ}, eprimOp, eopInfo, eas} =
        let (ass, _, eas') = unzip3 $ map (bu mtvs) eas
            as = Set.unions ass
            (_,pts) = unfoldMTy $ opType eopInfo
            cs = Set.fromList [EqC m1 m2 | (_,m1) <- Set.toList as | m2 <- pts]
        in (as, cs, e) -- EPrimop monotype set in dv

    bu mtvs e@ECase{ee,ealts} =
        let (asee, csee, ee')    = bu mtvs ee
            (asas, csas, ealts') = butAlts (getTyp ee') mtvs ealts
        in (Set.union asee asas,
            csee `Set.union` csas,
            setTyp (getTyp ealts') e{ee = ee', ealts = ealts'})

    bu mtvs e@ELet{edefs,ee} =
        let (asdefs, csdefs, edefs') = buNest mtvs edefs Set.empty
            (asee, csee, ee') = bu mtvs ee
            xts = map2 oname getTyp edefs'
            (asee', cseen) = buIn mtvs xts asee
        in (Set.union asdefs asee',
            csdefs `Set.union` csee `Set.union` cseen,
            setTyp (getTyp ee') e{edefs = edefs', ee = ee'})

    bu mtvs e = error $ "HMStg.bu Expr: " ++ show e

-- ^ Entry to bottom up generation of Constraints
buNest
  :: Set.Set Monotype -- ^ environment
  -> [Obj InfoTab]    -- ^ Letrec'd Objects to process
  -> Assumptions      -- ^ initial assumptions (generated by mhs type signatures)
                      --   or empty, if STG frontend is used
  -> (Assumptions, Constraints, [Obj InfoTab])

buNest mtvs os assums =
        -- get strongly connected components
        let fvss = map (Set.fromList . truefvs . omd) os
            onamel = map oname os
            localfvll = map (Set.toList . Set.intersection (Set.fromList onamel)) fvss
            sccnames = scc $ zip onamel localfvll -- sccnames is list of lists of onames
            nameobjm = Map.fromList $ zip onamel os
            sccobjs = map (map (flip lookupOrElse nameobjm)) sccnames
--        in error $ show sccobjs
        in foldr f (assums, Set.empty, []) sccobjs -- seed with initial assumptions
            where
              f defs (as, cs, ds) =
                  let (asdefs, csdefs, defs') = buRec mtvs defs
                      xts = map2 oname getTyp defs'
                      (as', ncs) = buIn mtvs xts as
                  in (Set.union asdefs as',
                      csdefs `Set.union` cs `Set.union` ncs,
                      defs'++ds)



--buRec discharges assumptions about the objects it defines
buRec
  :: (BU (Obj a), SetTyp (Obj a))
  => Set.Set Monotype
  -> [Obj a]
  -> (Set.Set (Var, Monotype), Set.Set Constraint, [Obj a])
buRec mtvs os =
    let (ass, css, os') = unzip3 $ map (bu mtvs) os
        xts = map2 oname getTyp os'
        onames = map fst xts
        ncs = Set.fromList
              [ EqC t' t |
                (x,t) <- xts, as <- ass, (x',t') <- Set.toList as, x==x' ]
    in (foldr dropAss (foldr1 Set.union ass) onames,
        foldr Set.union ncs css,
        os')

-- buIn takes: mtvs, an environment of x:t, and a set of assumptions as
--    returns: as\xi, i.e. assumptions with xi discharged, set of constraints
buIn
  :: Set.Set Monotype           -- ^ environment
  -> [Assumption]               -- ^ Assumptions generated from dv
  -> Assumptions                -- ^ Assumptions generated by buRec
  -> (Assumptions, Constraints) -- ^ Assumptions and (Implicit) Constraints,
                                --   less discharged Assumptions
buIn mtvs xts as =
    let xi = map fst xts
        ncs = Set.fromList [ ImpC t' mtvs t
                             | (x,t) <- xts, (x',t') <- Set.toList as, x == x' ]
    in (foldr dropAss as xi, ncs)


butAlts
  :: Monotype         -- ^ Type of the scrutinee of the enclosing ECase
  -> Set.Set Monotype -- ^ environment
  -> Alts InfoTab     -- ^ Alts block to process
  -> (Assumptions, Constraints, Alts InfoTab)
butAlts t0 mtvs e@Alts{alts, scrt} =
      let mtvs' = Set.insert (MVar sv) mtvs
          (ass, css, alts') = unzip3 $ map (butAlt t0 $ mtvs') alts
          (a1:as) = alts'

          -- Type of scrutinee binding must be the same as that of the scrutinee
          --   expression (passed in as t0).  Constraint only created if an
          --   Assumption about is found in the Alts. This matches behavior of
          --   butAlt given an equivalent binding from an ADef.
          --   The EAtom is still given a type, but the assumptions (singleton)
          --   and constraints (empty) are discarded
          (_, _, scrt') = bu mtvs scrt
          scrtCS = [EqC (getTyp scrt') t0 | (sv',tp) <- concatMap Set.toList ass, sv' == sv]
          sv = case scrt of
            EAtom _ (Var v) -> v
            _               -> error "HMStg.butAlts scrt not atomic var"


          ncs = Set.fromList $ scrtCS ++ [EqC (getTyp a1) (getTyp a') | a'<-as]
      in (dropAss sv $ foldr1 Set.union ass,
          foldr Set.union ncs css,
          setTyp (getTyp a1) e{alts = alts', scrt = scrt'}) -- any one will do


butAlt
  :: Monotype         -- ^ Type of the scrutinee of the enclosing ECase
  -> Set.Set Monotype -- ^ environment
  -> Alt InfoTab     -- ^ Alt block to process
  -> (Assumptions, Constraints, Alt InfoTab)

butAlt t0 mtvs e@ADef{av,ae} =
    -- av monomorphic:  mtvs and EqC
    let (as, cs, ae') = bu (Set.union mtvs $ Set.singleton (MVar av)) ae
        ncs = Set.fromList [ EqC t0 t | (x,t) <- Set.toList as, x == av ]
    in (dropAss av as,
        Set.union cs ncs,
        setTyp (getTyp ae') e{ae = ae'})

butAlt t0 mtvs e@ACon{amd,ac,avs,ae} =
    let MCon _ _ ntvs = getTyp e --stashed by dv
        tis = instantiateDataConAt ac (cmap amd) ntvs
        --tis = trace ("trace butAlt " ++ ac) (instantiateDataConAt ac (cmap amd) ntvs)
        xtis = zzip avs tis
        -- note ntvs == freeVars tis (as sets) so ntvs are new mtvs
        (as,cs,ae') = bu (Set.union mtvs $ Set.fromList ntvs) ae
        ncs = Set.fromList $ (EqC t0 $ getTyp e) : -- TC b1...bn
                             [ EqC ti t' | (xi,ti) <- xtis,
                                           (x,t') <- Set.toList as,
                                           x == xi ]
    in (foldr dropAss as avs,
        Set.union cs ncs,
        setTyp (getTyp ae') e{ae = ae'})

{-
butAlt t0 mtvs e@ACon{amd,ac,avs,ae} =
    let -- instantiate type constructor definition
        -- get type constructor definition

        TyCon _ tcon tvs dcs =
            luTCon ac $ cmap amd
        -- get data constructor definition C [Monotype]

        ms = case [ ms | DataCon c ms <- dcs, c == ac] of
               [ms] -> ms
               _ -> error $ "butAlt: not finding " ++ ac ++ " in " ++ show dcs ++
                    " for TyCon: " ++ tcon ++
                    "\nCMap:\n" ++ show (cmap amd)
        -- instantiate the Monotypes
        MCon _ _ ntvs = getTyp e --stashed by dv

        subst = Map.fromList $ zzip tvs ntvs
        tis = apply subst ms
        xtis = zzip avs tis
        -- note ntvs == freeVars tis (as sets) so ntvs are new mtvs
        (as,cs,ae') = bu (Set.union mtvs $ Set.fromList ntvs) ae
        ncs = Set.fromList $ (EqC t0 $ getTyp e) : -- TC b1...bn
                             [ EqC ti t' | (xi,ti) <- xtis,
                                           (x,t') <- Set.toList as,
                                           x == xi ]
    in (foldr dropAss as avs,
        Set.union cs ncs,
        setTyp (getTyp ae') e{ae = ae'})
-}

instance BU (Obj InfoTab) where
  bu mtvs o@FUN{vs,e,oname} =
      -- get new type vars for args
    let MCon _ "hack" ntvs = getTyp o
        (as,cs,e') = bu (Set.union mtvs $ Set.fromList ntvs) e
        ncs = Set.fromList [EqC t' bi | (xi,bi) <- zzip vs ntvs,
                            (x,t') <- Set.toList as,
                            x == xi ]
        typ = foldr MFun (getTyp e') ntvs
        nas = Set.singleton (oname, typ)
    in (foldr dropAss as vs `Set.union` nas,
        Set.union cs ncs,
        setTyp typ o{e = e'})

  -- identical to EFCall
  bu mtvs o@PAP{f,as} =
      let (ass, css, as') = unzip3 $ map (bu mtvs) as
          ftyp = foldr MFun (getTyp o) (map getTyp as')
      in (Set.insert (f,ftyp) $ Set.unions ass,
          Set.unions css,
          o{as=as'}) -- PAP monotype set in dv



{- [Atom version]
  bu mtvs o@PAP{f,as} =
      let typ = getTyp o
          (m,ms) = unfoldMTy typ
      in (Set.fromList $ (f,typ) : [ (v, t) | (Var v, t) <- zzip as ms ],
          Set.empty,
          setTyp m o)
-}

  bu mtvs o@CON{omd,c,as} =
      let (ass,css,as') = unzip3 $ map (bu mtvs) as
          typ@(MCon _ tcon ntvs) = getTyp o
          ms = instantiateDataConAt c (cmap omd) ntvs
          asts = map getTyp as'
      in (Set.unions ass,
          Set.unions css `Set.union` Set.fromList [EqC t m | (t,m) <- zzip asts ms],
          setTyp typ o{as = as'})

  bu mtvs o@THUNK{e} =
      let (as,cs,e') = bu mtvs e
      in (as, cs, setTyp (getTyp e') o{e = e'})

--BH  bu mtvs o@BLACKHOLE{} =
--BH      (Set.empty, Set.empty, o) -- typ as fresh var set in dv


-- MPVar is just for type inference
polyToMono m@(MVar _) = m
polyToMono (MFun a b) = MFun (polyToMono a) (polyToMono b)
polyToMono (MCon b c ms) = MCon b c $ map (polyToMono) ms
polyToMono m@MPrim{} = m
polyToMono (MPVar v) = MVar v
polyToMono m = error $ "HMStg.polyToMono: " ++ show m

class BackSub a where
    backSub :: Subst -> a -> a

instance BackSub a => BackSub [a] where
    backSub s = map $ backSub s

instance BackSub (Obj InfoTab) where
    backSub s o =
        let o' = setTyp (polyToMono $ apply s $ getTyp o) o
        in case o' of
             o@FUN{e} -> o{e = backSub s e}
             o@THUNK{omd, e} | isBoxed $ typ omd -> o{e = backSub s e}
                             | otherwise -> error "THUNKs must evaluate to boxed types"
             _ -> o'

instance BackSub (Expr InfoTab) where
    backSub s e =
        let e' = setTyp (polyToMono $ apply s $ getTyp e) e
        in case e' of
             e@ELet{edefs,ee}   -> e{edefs = backSub s edefs, ee = backSub s ee}
             e@ECase{ee, ealts} -> e{ee = backSub s ee, ealts = backSub s ealts}
             e@EPrimOp{eas}     -> e{eas = map (backSub s) eas}
             e@EFCall{eas}      -> e{eas = map (backSub s) eas}
             EAtom{}            -> e'

instance BackSub (Alts InfoTab) where
    backSub s as@Alts{alts, scrt} =
        setTyp (polyToMono $ apply s $ getTyp as)
        as{alts = backSub s alts,
           scrt = backSub s scrt}

instance BackSub (Alt InfoTab) where
    backSub s a =
        setTyp (polyToMono $ apply s $ getTyp a) a{ae = backSub s $ ae a}


-- close over free type variables
-- type variables closed at an outer scope (bvs) cannot be closed in an inner scope
-- ovs - "open variables"

class CO a where
    co :: Set.Set TyVar -> a -> a

instance CO a => CO [a] where
    co bvs = map $ co bvs

instance CO (Obj InfoTab) where
    co bvs o =
        let m    = getTyp o
            fvs  = freevars m
            ovs  = Set.difference fvs bvs
            o'   = setCTyp (PPoly (Set.toList ovs) m) o
            bvs' = Set.union bvs ovs
        in case o' of
             o@FUN{e}   -> o{e = co bvs' e}
             o@THUNK{e} -> o{e = co bvs' e}
             _          -> o'

instance CO (Expr InfoTab) where
    co bvs e =
        let m    = getTyp e
            fvs  = freevars m
            ovs  = Set.difference fvs bvs
            ptyp = PPoly (Set.toList ovs) m
            e'   = setCTyp ptyp e
            bvs' = Set.union bvs ovs
        in case e' of
             e@ELet{edefs,ee}   -> e{edefs = co bvs' edefs, ee = co bvs' ee}
             -- I think this is right.
             e@ECase{ee, ealts} -> e{ee = co bvs' ee, ealts = setCTyp ptyp $ co bvs' ealts}
             e@EPrimOp{eas}     -> e{eas = map (co bvs') eas}
             e@EFCall{eas}      -> e{eas = map (co bvs') eas}
             EAtom{}            -> e'

instance CO (Alts InfoTab) where
    co bvs as@Alts{alts} =
        let m    = getTyp as
            fvs  = freevars m
            ovs  = Set.difference fvs bvs
            as'  = setCTyp (PPoly (Set.toList ovs) m) as
            bvs' = Set.union bvs ovs
        in as'{alts = co bvs' alts}

instance CO (Alt InfoTab) where
    co bvs a =
        let m    = getTyp a
            fvs  = freevars m
            ovs  = Set.difference fvs bvs
            a'   = setCTyp (PPoly (Set.toList ovs) m) a
            bvs' = Set.union bvs ovs
        in a'{ae = co bvs' $ ae a}



-- utilities
map2 f g = map (\x -> (f x, g x))

class PP a where
    pp :: a -> String

-- unparser ****************************************************************

dropspaces = dropWhile (==' ')

interpolate ('%':'%':s) = interpolate $ '%':s
interpolate ('%':'\n':'%':'%':s) = interpolate $ '%':'\n':'%':s
interpolate ('%':'\n':'%':s) = interpolate $ dropspaces s
interpolate ('%':'\n':s) = interpolate $ dropspaces s
interpolate ('\n':'%':s) = interpolate $ dropspaces s
interpolate (c:s) = c : interpolate s
interpolate [] = []

showObjs objs = interpolate $ intercalate "\n" $ ppstg 0 objs

iindent n s@('%':_) = s
iindent n s = (take n $ repeat ' ') ++ s

indents n ss = map (iindent n) ss

-- instance Ppstg [Atom] where
showas as = intercalate " " $ map show as

showFVs vs = "[" ++ intercalate " " vs ++ "] "

class Ppstg a where ppstg :: Int -> a -> [String]

instance Show a => Ppstg (Expr a) where
    ppstg n (EAtom emd a) =
        indents n [show emd ++ " " ++ show a]

    ppstg n (EFCall emd f as) =
        indents n [show emd ++ " " ++ f ++ " " ++ showas as]

    ppstg n (EPrimOp emd p i as) =
        indents n [show emd ++ " PRIMOP " ++ show p ++ " " ++ show i ++ " " ++ showas as]

    ppstg n (ELet emd defs e) =
        let ss = [show emd ++ " let { %"] ++
                 ppstg 6 defs ++
                 ["} in %"] ++
                 ppstg 5 e
        in indents n ss

    ppstg n (ECase emd e alts) =
        let ss = [show emd ++ " case %"] ++
                 ppstg 5 e ++
                 ["of { %"] ++
                 ppstg 5 alts ++
                 ["%}"]
        in indents n ss

instance Show a => Ppstg (Alt a) where
    ppstg n (ACon amd c vs e) =
        let line = show amd ++ " " ++ c ++ precalate " " vs ++ " -> %"
            ss = [line] ++
                 ppstg (length line - 1) e
        in indents n ss

    ppstg n (ADef amd v e) =
        let ss = [show amd ++ " " ++ v ++ " -> %"] ++
                 ppstg (4 + length v) e
        in indents n ss

instance Show a => Ppstg (Alts a) where
    ppstg n (Alts {alts}) =
        concatMap (ppstg n) alts

instance Show a => Ppstg (Obj a) where
    ppstg n (FUN omd vs e _) =
        let ss = [show omd ++ " FUN( " ++ intercalate " " vs ++ " ->"] ++
                 ppstg 2 e ++
                 ["%)"]
        in indents n ss

    ppstg n (PAP omd f as _) =
        let ss = [show omd ++ " PAP( " ++ f ++ " " ++ showas as ++ " )"]
        in indents n ss

    ppstg n (CON omd c as _) =
        let ss = [show omd ++ " CON( " ++ c ++ " " ++ showas as ++ " )"]
        in indents n ss

    ppstg n (THUNK omd e _) =
        let ss = [show omd ++ " THUNK( %"] ++
                 ppstg 7 e ++
                 ["% )"]
        in indents n ss

--BH    ppstg n (BLACKHOLE omd _) =
--BH        indents n [show omd ++ " BLACKHOLE"]

instance Show a => Ppstg [Obj a] where
    -- ppstg n defs = intercalate ["\n"] $ map (ppstg n) defs
    ppstg n defs = concatMap (ppstgdef n) defs

ppstgdef n o =
    let ss = [ oname o ++ " = %" ] ++
             ppstg 2 o
    in indents n ss
