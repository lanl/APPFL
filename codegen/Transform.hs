{-# LANGUAGE
FlexibleInstances,
TypeSynonymInstances,
NamedFieldPuns #-}

module Transform
(
  setTypeSignatures,
  renameIDs,
  genConFunctions,
  desugarExpAps,
  boxNumLiterals,
) where

import Debug.Trace
import AST
import NewAST
import ADT
import qualified Data.Map as Map
import qualified Data.Set as Set
import PPrint
import Util (mapSnd, deleteAll, lookupOrElse)
import Data.Maybe (catMaybes)
import Data.Char (isUpper, isLower, isNumber)
import Data.List (isPrefixOf, nub)
import State


{--------------------------------------------------------------------
Setting user-defined type signatures in definitions that have them.
Could be expanded to set a type for the expression (and sub expressions)
in the definition.
This also sets boxity (where applicable) in those types based on the data
definitions and builtin Int# and Double#
---------------------------------------------------------------------}

setTypeSignatures :: [Defn] -> [Defn]
setTypeSignatures defs = 
  let (ds,os,ts) = partitionDefs defs
      cmap = buildCMap ds
      ts' = map (setBoxity cmap) ts
      tmap = buildTMap ts'
  in ds ++ ts' ++ map (setTypeSigs tmap) os

setBoxity cmap t@TDefn{mtyp} =
  let mtyp' = sb mtyp in t{mtyp = mtyp'}
  where sb m = case m of
          MCon _ c ms -> case Map.lookup c cmap of
            Just (MCon b _ _) -> MCon b c (map sb ms)
            _ -> error $ "unknown concrete type reference: " ++ c
          _ -> m
setBoxity cmap d = error $ "Transform.setBoxity: d = " ++ show (pprint d)        
          

buildCMap defs =
  let mfun (DDefn t@(MCon b c ms) _) = (c, t)
      mfun _ = error "bad monotype in DDefn"
  in Map.fromList $ map mfun defs

buildTMap :: [Defn] -> TypeMap
buildTMap defs =
  let (_, _, ts) = partitionDefs defs
  in Map.fromList $ map (\t -> (bnd t, mtyp t)) ts

type TypeMap = Map.Map Var Monotype

class SetTypeSigs a where
  setTypeSigs :: TypeMap -> a -> a

instance SetTypeSigs Defn where
  setTypeSigs tmap d = case d of
    ODefn{bnd, oexp} ->
      let oexp' = setTypeSigs tmap oexp
      in d {oexp = oexp',
            mmtype = Map.lookup bnd tmap}

    DDefn{} -> d
    TDefn{} -> d

instance SetTypeSigs Exp where
  setTypeSigs tmap e = case e of
    EAp{fexp, eexp} ->
      let fexp' = setTypeSigs tmap fexp
          eexp' = setTypeSigs tmap eexp
      in e {fexp = fexp',
            eexp = eexp'}

    EFn{eexp} ->
      let eexp'  = setTypeSigs tmap eexp
      in e {eexp = eexp'}

    ECs{eexp, cls} ->
      let eexp' = setTypeSigs tmap eexp
          cls'  = map (mapSnd (setTypeSigs tmap)) cls
      in e {eexp = eexp',
            cls  = cls'}

    ELt{eexp, defns} ->
      -- have to delete all new bindings from map. Type signatures bind only
      -- at the same "depth" in a program.
      -- Must also add new signatures, which are legal in let exprs
      let newBinds = map bnd defns
          tmap'    = Map.union (buildTMap defns) (deleteAll newBinds tmap)
          eexp'    = setTypeSigs tmap' eexp
          defns'   = map (setTypeSigs tmap') defns
      in e {eexp  = eexp',
            defns = defns'}

    EAt{} -> e


{-------------------------------------------------------------------------
Add a common prefix to all user-created bindings "u_" to make generating
variables in transformation simpler
-------------------------------------------------------------------------}

renameIDs defs =
  let (defs', _) = runState (rename Map.empty defs) Map.empty
  in defs'


ceeSubDict = [('#',"_h"),
              ('\'',"_a")]


renameIt :: Var -> State NameMap Var
renameIt var =
  do
    nmap <- get
    let i = case Map.lookup var nmap of
             Just x -> x
             Nothing -> 0
        var' = case ceeify var of    
                (True, v) -> dirtyPfx i v
                (False, v) -> cleanPfx i v
          where ceeify [] = (False, [])
                ceeify (x:xs) = case lookup x ceeSubDict of
                  Just s -> let (b', s') = ceeify xs
                            in (True, s++s')
                  Nothing -> let (b', s') = ceeify xs
                             in (b', x:s')
                cleanPfx i v = "uc" ++ show i ++ "_" ++ v
                dirtyPfx i v = "ud" ++ show i ++ "_" ++ v
    put $ Map.insert var (i+1) nmap
    return var'

    
isUserID = ("u" `isPrefixOf`)
           
getUserID c | isUserID c = tail $ dropWhile (isNumber) $ drop 2 c
            | otherwise = error $ "Transform.getUserID: c is not user ID - " ++ c
               
isGenID = not . isUserID


type NameMap = Map.Map Var Int
type ReplMap = Map.Map Var Var

class Rename a where
  rename :: ReplMap -> a -> State NameMap a

instance Rename [Defn] where
  rename env defs =
    do
      let (ds, os, ts) = partitionDefs defs
          obnds = nub $ map bnd os
      obnds' <- mapM renameIt obnds
      let env' = Map.union env $ Map.fromList $ zip obnds obnds'
          renamed = map (\o -> o{bnd = lookupOrElse (bnd o) env'}) os
      os' <- mapM (rename env') renamed
      ts' <- mapM (rename env') ts
      return $ ds ++ os' ++ ts'

instance Rename Defn where
  rename env d = case d of
    DDefn{} -> return d -- don't need to mess with datatype declarations
    
    TDefn{bnd} ->
      case Map.lookup bnd env of
       Just n -> return d{bnd = n}
       Nothing -> error $
                  "Transform.rename (Defn): Couldn't find TDefn bnd in env - " ++ bnd
    
    ODefn{bnd, oexp} -> 
      do -- 
        oexp' <- rename env oexp
        return d {oexp = oexp'}
        
instance Rename Exp where
  rename env e = case e of
    EFn{pats, eexp} ->
      do
        pats' <- mapM (rename env) pats
        let penv = Map.unions $ zipWith makePatBindMap pats pats'
            env' = Map.union penv env
        eexp' <- rename env' eexp
        return e {pats = pats',
                  eexp = eexp'}

    EAp{fexp, eexp} ->
      do
        fexp' <- rename env fexp
        eexp' <- rename env eexp
        return e {fexp = fexp',
                  eexp = eexp'}

    ELt{eexp, defns} ->
      do
        eexp' <- rename env eexp
        defns' <- rename env defns
        return e {eexp = eexp',
                  defns = defns'}

    ECs{eexp, cls} ->
      do
        eexp' <- rename env eexp
        cls' <- mapM (rename env) cls
        return e {eexp = eexp',
                  cls = cls'}

    EAt{atm = AtmVar v} ->
      let atm' =
            case Map.lookup v env of
             Just v' -> AtmVar v'
             Nothing -> error $
                        "Transform.rename: (Exp) why isn't atm in map? " ++
                        (show $ pprint e) ++
                        show env
      in return e {atm = atm'}
    EAt{} -> return e

instance Rename Clause where
  rename env (pat, exp) =
    do
      pat' <- rename env pat
      let penv = makePatBindMap pat pat'
          env' = Map.union penv env
      exp' <- rename env' exp
      return (pat', exp')

instance Rename Pattern where
  rename env pat = case pat of
    Match{npats} ->
      do
        npats' <- mapM (rename env) npats
        return pat{npats = npats'}
    Default{str} ->
      do
        str' <- renameIt str
        return pat{str = str'}


makePatBindMap (Match m ms) (Match n ns) =
  let maps = zipWith makePatBindMap ms ns
  in Map.unions (Map.singleton m n : maps)
makePatBindMap (Default m) (Default n) = Map.singleton m n
makePatBindMap m1 m2 = error "Transform.makePatBindMap (Exp): shouldn't happen" 
  

{-------------------------------------------------------------------------
Generate functions for data constructors (only those in use!)
-------------------------------------------------------------------------}

genConFunctions defs =
  let (ds,os,ts) = partitionDefs defs
      constrs = concatMap dcons ds
      dict = Map.fromList $ map (\DCon{dcon, mtyps} -> (dcon, length mtyps)) constrs
      (cons, defs') = gcfs Set.empty defs
  in makeConEFns (Set.toList cons) dict ++ defs'

makeConEFns cons dict =
  let mfun c = case Map.lookup c dict of
                Just 0 -> ODefn ("gfc_" ++ c) (EAt $ AtmCon c) Nothing
                Just l ->
                  let vars = ["gac_" ++ show i | i <- [1..l]]
                      pats = map Default vars
                      fexp = EAt $ AtmCon c
                      args = map (EAt . AtmVar) vars
                      oexp = foldl EAp fexp args
                  in ODefn ("gfc_" ++ c) oexp Nothing
                Nothing -> error $ "could not find Con to generate function: " ++ c
  in map mfun cons
 
         

type CGenSet = Set.Set Con

class GenConFuns a where
  gcfs :: CGenSet -> a -> (CGenSet, a)

instance (GenConFuns a) => GenConFuns [a] where
  gcfs cgs as =
    let (cgss, as') = unzip $ map (gcfs cgs) as
    in (Set.unions cgss, as')
  
instance GenConFuns Defn where
  gcfs cgs d = case d of
    DDefn{} -> (cgs,d)
    TDefn{} -> (cgs,d)
    ODefn{oexp} ->
      let (cgs', oexp') = gcfs cgs oexp
      in (cgs', d {oexp = oexp'})

instance GenConFuns Exp where
  gcfs cgs e = case e of
    EAt{atm} -> case atm of
      AtmCon c -> let c' = "gfc_" ++ c
                  in (Set.insert c cgs, e{atm = AtmVar c'})
      _ -> (cgs, e)

    EAp{fexp, eexp} ->
      let (cgs', fexp') = gcfs cgs fexp
          (cgs'', eexp') = gcfs cgs' eexp
      in (cgs'', e {fexp = fexp',
                    eexp = eexp'})

    EFn{eexp} ->
      let (cgs', eexp') = gcfs cgs eexp
      in (cgs', e {eexp = eexp'})

    ELt{eexp, defns} ->
      let (cgs', eexp') = gcfs cgs eexp
          (cgs'', defns') = gcfs cgs' defns
      in (cgs'', e{eexp = eexp',
                   defns = defns'})

    ECs{eexp, cls} ->
      let (cgs', eexp') = gcfs cgs eexp
          (cgs'', cls') = gcfs cgs' cls
      in (cgs'', e {eexp = eexp',
                    cls = cls'})

instance GenConFuns Clause where
  gcfs cgs (pat, exp) =
    let (cgs', exp') = gcfs cgs exp
    in (cgs', (pat, exp'))
                         

{--------------------------------------------------------------------------
Convert expression application to atomic function application
e.g. e1 e2 e3 -> let f = e1; a1 = e2; a2 = e3; in f a1 a2
--------------------------------------------------------------------------}

desugarExpAps defs =
  let (defs', _) = runState (mapM dsgAps defs) (0,0)
  in defs'

genFunVar :: State (Int,Int) Var
genFunVar =
  do (fi,ai) <- get
     put (fi+1,ai)
     return ("gf_" ++ show fi)

genArgVar :: State (Int, Int) Var
genArgVar =
  do (fi,ai) <- get
     put (fi,ai+1)
     return ("ga_" ++ show ai)

genArgVars :: Int -> State (Int, Int) [Var]
genArgVars i = case i of
  0 -> return []
  _ -> do
    a <- genArgVar
    as <- genArgVars (i-1)
    return (a:as)
     
class DesugarAps a where
  dsgAps :: a -> State (Int,Int) a

instance DesugarAps Defn where
  dsgAps d = case d of
    DDefn{} -> return d
    TDefn{} -> return d
    ODefn{oexp} ->
      do
        oexp' <- dsgAps oexp
        return $ d{oexp = oexp'}

instance DesugarAps Exp where
  dsgAps e = case e of
    EFn{eexp} ->
      do
        eexp' <- dsgAps eexp
        return $ e {eexp = eexp'}
        
    ELt{eexp, defns} ->
      do
        eexp' <- dsgAps eexp
        defns' <- mapM dsgAps defns
        return $ e {eexp = eexp',
                    defns = defns'}

    ECs{eexp, cls} ->
      do
        eexp' <- dsgAps eexp
        cls' <- mapM dsgAps cls
        return $ e {eexp = eexp',
                    cls = cls'}

    EAt{} -> return e

    -- the ugly one
    EAp{} ->
      do
        -- unfold and desugar expressions in application
        let (fe, es) = unfoldEAp e
        fe' <- dsgAps fe
        es' <- mapM dsgAps es

        -- generate assoc of EAt to Maybe Defn
        let mfun e =
              case e of
               EAt{} -> return (e, Nothing);
               _ ->
                 do v <- genArgVar;
                    return (EAt (AtmVar v), Just $ ODefn{bnd = v, oexp = e, mmtype = Nothing})
        tups <- mapM mfun es'
        let (args, mdefs) = unzip tups
            defs = catMaybes mdefs

        -- ensure function being applied is in atomic form, and bound to Defn
        -- if necessary
        (f, truDefs) <-
          case fe' of
           EAt(AtmVar _) -> return (fe',defs)
           EAt(AtmCon _) -> return (fe',defs)
           EAt(AtmOp _)  -> return (fe',defs)
           EAt{} -> error $ "bad EAt in Transform.dsgAps: " ++ show (pprint fe')
           _ -> do
             v <- genFunVar
             return (EAt $ AtmVar v, ODefn {bnd = v, oexp = fe', mmtype = Nothing}:defs)

        -- build the expression (if defs generated, make let, otherwise, just expr)
        return $ let exp = foldl EAp f args in
                  case truDefs of
                   [] -> exp
                   _  -> ELt{eexp = exp,
                             defns = truDefs}

instance DesugarAps Clause where
  dsgAps (pat, exp) =
    do
      exp' <- dsgAps exp
      return (pat, exp')



{---------------------------------------------------------------------------------
Desugar numeric literals to Boxed Int/Doubles
This transformation adds its own datatype definitions for boxed Ints and Doubles
as well as functions that call to them in the same convention as that used when
generating functions for constructors used by a program.  This keeps the transform
from being reliant on any previous transform while still compliant with the naming
convention used to generate the abstract STG
-----------------------------------------------------------------------------------}


intC = DDefn { mtyp = MCon True "GC_Int" [],
               dcons = [DCon {dcon = "GDC_I",
                              mtyps = [MPrim UBInt] }] }
doubC = DDefn { mtyp = MCon True "GC_Double" [],
                dcons = [DCon {dcon = "GDC_D",
                               mtyps = [MPrim UBDouble] }] }
intFDef = ODefn {bnd = "gfc_GDC_I",
                 oexp = EFn { pats = [Default "gac_1"],
                              eexp = EAp {fexp = EAt $ AtmVar "GDC_I",
                                          eexp = EAt $ AtmVar "gac_1"} },
                 mmtype = Just $ MFun (MPrim UBInt) (mtyp intC)}

doubFDef = ODefn {bnd = "gfc_GDC_D",
                  oexp = EFn { pats = [Default "gac_1"],
                               eexp = EAp {fexp = EAt $ AtmVar "GDC_D",
                                           eexp = EAt $ AtmVar "gac_1"} },
                  mmtype = Just $ MFun (MPrim UBDouble) (mtyp doubC)}

callIntF ea = EAp {fexp = EAt $ AtmVar "gfc_GDC_I",
                   eexp = ea}

callDoubF ea = EAp {fexp = EAt $ AtmVar "gfc_GDC_D",
                    eexp = ea}

isInteger s = all isNumber s
isDouble s | '.' `elem` s = let (w,(p:ps)) = break (== '.') s
                            in isInteger w && isInteger ps
           | otherwise = False

boxNumLiterals :: [Defn] -> [Defn]                
boxNumLiterals = boxnums

class BoxNums a where
  boxnums :: a -> a

instance (BoxNums a) => BoxNums [a] where
  boxnums = map boxnums

instance BoxNums Defn where
  boxnums d = case d of
    DDefn{} -> d
    TDefn{} -> d
    ODefn{oexp} -> d{oexp = boxnums oexp}

instance BoxNums Exp where
  boxnums e = case e of
    EAt{atm} -> case atm of
                 LBInt{} -> callIntF e
                 LBDbl{} -> callDoubF e
                 _ -> e

    EAp{fexp,eexp} -> e {fexp = boxnums fexp,
                         eexp = boxnums eexp}

    EFn{pats,eexp} -> e {pats = boxnums pats,
                         eexp = boxnums eexp}

    ECs{eexp,cls} -> e {eexp = boxnums eexp,
                        cls = boxnums cls}

    ELt{eexp, defns} -> e {eexp = boxnums eexp,
                           defns = boxnums defns}

instance BoxNums Clause where
  boxnums (pat, exp) = (boxnums pat, boxnums exp)

instance BoxNums Pattern where
  boxnums pat = case pat of
    Default{} -> pat
    Match{str, npats} ->
      let pat' = pat {npats = boxnums npats}
      in case () of
          _ | isInteger str -> Match {str = "GDC_I",
                                      npats = [pat']}
            | isDouble str -> Match {str = "GDC_D",
                                     npats = [pat']}
            | otherwise -> pat'
        
