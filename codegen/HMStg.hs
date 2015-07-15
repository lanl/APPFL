{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ParallelListComp      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}

module HMStg (
  hmstgdebug,
  hmstg
) where

import ADT
import CMap
import AST
import InfoTab
import BU
import SCC
import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad.State
import Data.List (intercalate)

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
hmstgdebug ::  [Obj InfoTab] -> IO()
--          ([Assumption], [(TyVar, Monotype)], [Constraint], String)


hmstg os0 = 
    let (os1,i1) = runState (dv os0) 0
        (as,cs,os2) = buNest Set.empty os1 :: (Set.Set Assumption,
                                               Set.Set Constraint,
                                               [Obj InfoTab])
        (subst, _) = runState (solve cs) i1
    in co Set.empty $ backSub subst os2

hmstgdebug os0 = 
    let (os1,i1) = runState (dv os0) 0
        (as,cs,os2) = buNest Set.empty os1 :: (Set.Set Assumption,
                                               Set.Set Constraint,
                                               [Obj InfoTab])
        (subst, _) = runState (solve cs) i1
    in putStrLn $ showObjs (co Set.empty $ backSub subst os2)
{-
hmstgdebug os0 = 
    let (os1,i1) = runState (dv os0) 0
        (as,cs,os2) = buNest Set.empty os1 :: (Set.Set Assumption,
                                               Set.Set Constraint,
                                               [Obj InfoTab])
        (subst, _) = runState (solve cs) i1
    in putStrLn $ show (Set.toList as) ++ show (Set.toList cs) ++ showObjs os2
-}

{-
    in (Set.toList as,
        Map.toList subst,
        apply subst $ Set.toList cs,
        "\n\n" ++ showObjs (backSub subst os2))
-}

-- distribute fresh type variables, monomorphic type variables, for BU in advance

errTyp = error "typ not defined"

getMono (Var v)  = freshMonoVar
getMono (LitI _) = return $ MPrim UBInt
getMono a        = error $ "HMStg.getMono: " ++ show a

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
    dv e@EAtom{ea} =
        do m <- getMono ea
           return $ setTyp m e

    dv e@EFCall{eas} = 
        do retMono <- freshMonoVar -- return type
           eas' <- mapM dv eas -- setTyp(s) of args
           let e' = e{eas = eas'}
           return $ setTyp retMono e'

    dv e@EPrimop{..} =
        let retMono = case eprimop of
                        Piadd -> MPrim UBInt
                        Pisub -> MPrim UBInt
                        Pimul -> MPrim UBInt
                        Pidiv -> MPrim UBInt
                        Pimod -> MPrim UBInt
                        Pimax -> MPrim UBInt
                        Pimin -> MPrim UBInt
                        Pieq  -> MPrim UBInt
                        Pile  -> MPrim UBInt
                        x -> error $ "HMStg.dv Eprimop " ++ show x
                        -- etc.

        in do eas' <- mapM dv eas -- setTyp(s) of Primop args
              let --ms = map getTyp eas' -- use those types to make MFun
                 -- m = foldr MFun retMono ms
                  e' = e{eas = eas'}
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
           return Alts{..}

instance DV (Alt InfoTab) (Alt InfoTab) where
    dv ADef{..} =
        do ae <- dv ae
           return ADef{..}
           
    dv a@ACon{amd,ac,ae} =
        do ae' <- dv ae
           -- stash type constructor TC b1 .. bn, bi fresh
           let TyCon boxed tcon tvs dcs = 
-- MODIFIED 6.30 - David----------------------------------------
                 luTCon ac $ cmap amd -- NEW
-- OLD                 gettTyConDefFromConstructor (dconMap amd) (tconMap amd) ac
           ntvs <- freshMonoVars $ length tvs
           let m = MCon boxed tcon ntvs

           return $ setTyp m a{ae = ae'}

instance DV (Obj InfoTab) (Obj InfoTab) where
    dv o@FUN{vs,e} =
        do bs <- freshMonoVars (length vs) -- one for each arg
           let m = MCon True "hack" bs  --hack, just hold them        
           e' <- dv e
           return $ setTyp m o{e=e'}  --what's the precedence, if it mattered?

    dv o@PAP{f,as} = -- identical to EFCall
        do retMono <- freshMonoVar -- return type
           ms <- mapM getMono as
           let m = foldr MFun retMono ms
           return $ setTyp m o

    dv o@CON{omd,c,as} = 
        -- stash type constructor TC b1 .. bn, bi fresh
        -- stash type info for as as well
        let TyCon boxed tcon tvs dcs =
-- MODIFIED 6.30 - David ----------------------------------------
              luTCon c $ cmap omd
-- OLD              getTyConDefFromConstructor (dconMap omd) (tconMap omd) c
        in do ntvs <- freshMonoVars $ length tvs
              asts <- mapM getMono as -- may be zero of them
              let hack = MFun (MCon boxed tcon ntvs) (MCon True "hack2" asts)
              return $ setTyp hack o

    dv o@THUNK{e} =
        do e' <- dv e
           return o{e = e'} -- don't need setTyp as bu is done on e

    dv o@BLACKHOLE{} =
        do typ <- freshMonoVar
           return $ setTyp typ o

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
        let (ass, _, eas') = unzip3 $ map (bu mtvs) eas
            ftyp = foldr MFun (typ $ emd e) $ map (typ . emd) eas
            fas = Set.singleton (ev, ftyp)
        in (Set.unions $ fas:ass,
            Set.empty,
            e{eas=eas'}) -- EFCall monotype set in dv

    bu mtvs e@EPrimop{emd = ITPrimop{typ}, eprimop, eas} =
        let (ass, _, eas') = unzip3 $ map (bu mtvs) eas
            as = Set.unions ass
            pts = case eprimop of
                    Piadd -> [MPrim UBInt, MPrim UBInt]
                    Pisub -> [MPrim UBInt, MPrim UBInt]
                    Pimul -> [MPrim UBInt, MPrim UBInt]
                    Pidiv -> [MPrim UBInt, MPrim UBInt]
                    Pimod -> [MPrim UBInt, MPrim UBInt]
                    Pimax -> [MPrim UBInt, MPrim UBInt]
                    Pimin -> [MPrim UBInt, MPrim UBInt]
                    Pieq  -> [MPrim UBInt, MPrim UBInt]
                    Pile  -> [MPrim UBInt, MPrim UBInt]
                    x -> error $ "HMStg.bu EPrimop " ++ show x
            cs = Set.fromList [EqC m1 m2 | (_,m1) <- Set.toList as | m2 <- pts]
        in (as, cs, e) -- EPrimop monotype set in dv

    bu mtvs e@ECase{ee,ealts} =
        let (asee, csee, ee')    = bu mtvs ee
            (asas, csas, ealts') = butAlts (getTyp ee') mtvs ealts
        in (Set.union asee asas,
            csee `Set.union` csas,
            setTyp (getTyp ealts') e{ee = ee', ealts = ealts'})

    bu mtvs e@ELet{edefs,ee} = 
        let (asdefs, csdefs, edefs') = buNest mtvs edefs
            (asee, csee, ee') = bu mtvs ee
            xts = map2 oname getTyp edefs'
            (asee', cseen) = buIn mtvs xts asee
        in (Set.union asdefs asee',
            csdefs `Set.union` csee `Set.union` cseen,
            setTyp (getTyp ee') e{edefs = edefs', ee = ee'})

    bu mtvs e = error $ "HMStg.bu Expr: " ++ show e

buNest :: Set.Set Monotype
       -> [Obj InfoTab]
       -> (Set.Set (String, Monotype), Set.Set Constraint, [Obj InfoTab])

buNest mtvs os = 
        -- get strongly connected components
        let fvss = map (Set.fromList . truefvs . omd) os
            onamel = map oname os
            localfvll = map Set.toList $ map (Set.intersection (Set.fromList onamel)) fvss
            sccnames = scc $ zip onamel localfvll -- sccnames is list of lists of onames
            nameobjm = Map.fromList $ zip onamel os
            sccobjs = map (map (lookupOrElse nameobjm)) sccnames
--        in error $ show sccobjs
        in foldr f (Set.empty, Set.empty, []) sccobjs
            where
              f defs (as, cs, ds) =
                  let (asdefs, csdefs, defs') = buRec mtvs defs
                      xts = map2 oname getTyp defs'
                      (as', ncs) = buIn mtvs xts as
                  in (Set.union asdefs as',
                      csdefs `Set.union` cs `Set.union` ncs,
                      defs'++ds)

--buRec discharges assumptions about the objects it defines
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
buIn mtvs xts as = 
    let xi = map fst xts
        ncs = Set.fromList [ ImpC t' mtvs t 
                             | (x,t) <- xts, (x',t') <- Set.toList as, x == x' ]
    in (foldr dropAss as xi, ncs)

lookupOrElse m k =
    case Map.lookup k m of
      Nothing -> error "HMStg this shouldn't happen"
      Just x  -> x

butAlts t0 mtvs e@Alts{alts} =
      let (ass, css, alts') = unzip3 $ map (butAlt t0 mtvs) alts
          (a1:as) = alts'
          ncs = Set.fromList [EqC (getTyp a1) (getTyp a') | a'<-as]
      in (foldr1 Set.union ass,
          foldr Set.union ncs css,
          setTyp (getTyp a1) e{alts = alts'}) -- any one will do

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
-- MODIFIED 6.30 - David ----------------------------------------
            luTCon ac $ cmap amd
-- OLD            getTyConDefFromConstructor (dconMap amd) (tconMap amd) ac
        -- get data constructor definition C [Monotype]
        -- [ms] = [ ms | DataCon b c ms <- dcs, c == ac ] -- ms :: [Monotype]
-- MODIFIED 7.1 - David ----------------------------------------
        ms = case [ ms | DataCon c ms <- dcs, c == ac] of
-- OLD        ms = case [ ms | DataCon b c ms <- dcs, c == ac ] of
               [ms] -> ms
               _ -> error $ "butAlt: not finding " ++ ac ++ " in " ++ show dcs ++
-- MODIFIED 6.30 - David ----------------------------------------
                    " for TyCon: " ++ tcon ++
                     "\nCMap:\n" ++ show (cmap amd)
-- OLD                     " ++ show (dconMap amd) ++ 
-- OLD                     "\ntconMap: " ++ show (tconMap amd)

        -- instantiate those Monotypes

        MCon boxed' c ntvs = getTyp e --stashed by dv
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

  bu mtvs o@PAP{f,as} =
      let typ = getTyp o
          (m,ms) = unfoldr typ
      in (Set.fromList $ (f,typ) : [ (v, t) | (Var v, t) <- zzip as ms ],
          Set.empty,
          setTyp m o)

  bu mtvs o@CON{omd,c,as} = 
      let
        TyCon boxed tcon tvs dcs = luTCon c $ cmap omd         
        [ms] = [ ms | DataCon c' ms <- dcs, c == c' ]
        -- instantiate those Monotypes
        MFun typ@(MCon _ tcon' ntvs) (MCon True "hack2" asts) = getTyp o        
        subst = Map.fromList $ zzip tvs ntvs
        ms' = apply subst ms
      in (Set.fromList [(v,m) | (Var v, m) <- zzip as ms'], --drop LitX cases
          Set.fromList [ EqC t m | (t,m) <- zzip asts ms'],
          setTyp typ o)

  bu mtvs o@THUNK{e} =
      let (as,cs,e') = bu mtvs e
      in (as, cs, setTyp (getTyp e') o{e = e'})

  bu mtvs o@BLACKHOLE{} =
      (Set.empty, Set.empty, o) -- typ as fresh var set in dv

-- m is rightmost element
unfoldr (MFun m1 m2) = let (m,ms) = unfoldr m2 in (m, m1:ms)
unfoldr m = (m,[])


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
             e@EPrimop{eas}     -> e{eas = map (backSub s) eas}
             e@EFCall{eas}      -> e{eas = map (backSub s) eas}
             EAtom{}            -> e'

instance BackSub (Alts InfoTab) where
    backSub s as@Alts{alts} = 
        setTyp (polyToMono $ apply s $ getTyp as) as{alts = backSub s alts}

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
        let m = getTyp o
            fvs = freevars m
            ovs = Set.difference fvs bvs
            o' = setCTyp (PPoly (Set.toList ovs) m) o
            bvs' = Set.union bvs ovs
        in case o' of
             o@FUN{e} -> o{e = co bvs' e}
             o@THUNK{e} -> o{e = co bvs' e}
             _ -> o'

instance CO (Expr InfoTab) where
    co bvs e =
        let m = getTyp e
            fvs = freevars m
            ovs = Set.difference fvs bvs
            e' = setCTyp (PPoly (Set.toList ovs) m) e
            bvs' = Set.union bvs ovs
        in case e' of
             e@ELet{edefs,ee}   -> e{edefs = co bvs' edefs, ee = co bvs' ee}
             e@ECase{ee, ealts} -> e{ee = co bvs' ee, ealts = co bvs' ealts}
             e@EPrimop{eas}     -> e{eas = map (co bvs') eas}
             e@EFCall{eas}      -> e{eas = map (co bvs') eas}
             EAtom{}            -> e'

instance CO (Alts InfoTab) where
    co bvs as@Alts{alts} = 
        let m = getTyp as
            fvs = freevars m
            ovs = Set.difference fvs bvs
            as' = setCTyp (PPoly (Set.toList ovs) m) as
            bvs' = Set.union bvs ovs
        in as'{alts = co bvs' alts}

instance CO (Alt InfoTab) where
    co bvs a =
        let m = getTyp a
            fvs = freevars m
            ovs = Set.difference fvs bvs
            a' = setCTyp (PPoly (Set.toList ovs) m) a
            bvs' = Set.union bvs ovs
        in a'{ae = co bvs' $ ae a}



-- utilities
map2 f g = map (\x -> (f x, g x))

-- sanity-checking zip
zzip [] [] = []
zzip (a:as) (b:bs) = (a,b) : zzip as bs -- changed to zzip here
zzip _ _ = error "zzip on lists of differing lengths"

class PP a where
    pp :: a -> String

-- unparser ****************************************************************
-- in the spirit of intercalate

precalate s [] = []
precalate s (s':ss) = s ++ s' ++ precalate s ss

dropspaces = dropWhile (==' ')

interpolate ('%':'%':s) = interpolate $ '%':s
interpolate ('%':'\n':'%':'%':s) = interpolate $ '%':'\n':'%':s
interpolate ('%':'\n':'%':s) = interpolate $ dropspaces s
interpolate ('%':'\n':s) = interpolate $ dropspaces s
interpolate ('\n':'%':s) = interpolate $ dropspaces s
interpolate (c:s) = c : interpolate s
interpolate [] = []

showObjs objs = interpolate $ intercalate "\n" $ ppstg 0 objs

indent n s@('%':_) = s
indent n s = (take n $ repeat ' ') ++ s

indents n ss = map (indent n) ss

instance Show InfoTab where
    show it = show (typ it)
--    show it = show (ctyp it)
--    show it = show (truefvs it)

-- instance Ppstg [Atom] where
showas as = intercalate " " $ map show as

showFVs vs = "[" ++ intercalate " " vs ++ "] "

class Ppstg a where ppstg :: Int -> a -> [String]

instance Show a => Ppstg (Expr a) where
    ppstg n (EAtom emd a) = 
        indents n [show emd ++ " " ++ show a]

    ppstg n (EFCall emd f as) = 
        indents n [show emd ++ " " ++ f ++ " " ++ showas as]

    ppstg n (EPrimop emd p as) = 
        indents n [show emd ++ " PRIMOP " ++ show p ++ " " ++ showas as]

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

    ppstg n (BLACKHOLE omd _) =
        indents n [show omd ++ " BLACKHOLE"]

instance Show a => Ppstg [Obj a] where
    -- ppstg n defs = intercalate ["\n"] $ map (ppstg n) defs
    ppstg n defs = concatMap (ppstgdef n) defs

ppstgdef n o =
    let ss = [ oname o ++ " = %" ] ++
             ppstg 2 o
    in indents n ss
