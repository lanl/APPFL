{-# LANGUAGE
FlexibleInstances,
TypeSynonymInstances,
NamedFieldPuns #-}

module MHS.Transform
(
  setTypeSignatures,
  renameIDs,
  genFunctions,
  simplifyExps,
  boxNumLiterals,
  simplifyPats,
  simpleCase
) where

import Debug.Trace
import MHS.AST
import MHS.Tokenizer (primopTable)
import qualified Data.Map as Map
import qualified Data.Set as Set
import PPrint
import Util (mapSnd, deleteAll, lookupOrElse, groupAllBy)
import Data.Maybe (catMaybes)
import Data.Char (isUpper, isLower, isNumber)
import Data.List (isPrefixOf, nub, partition, groupBy, (\\), find)
import Data.Tuple (swap)
import State
import Data.Function (on)
import Control.Monad (foldM)

{--------------------------------------------------------------------
Setting user-defined type signatures in definitions that have them.
Could be expanded to set a type for the expression (and sub expressions)
in the definition.
This also sets boxity (where applicable) in those types based on the data
definitions and builtin Int# and Double#
---------------------------------------------------------------------}


-- Use the TDefns built from type signatures in a .mhs program and
-- associate them with the objects they correspond to
-- Currently there is no checking for signatures without accompanying
-- bindings.
-- This also sets the "boxity" of the types in both type signatures
-- and in constructors (Constrs) as defined by the datatype
-- declarations in the list of Defns
setTypeSignatures :: [Defn] -> [Defn]
setTypeSignatures defs = 
  let defs'= map (setBoxity cmap) defs
      (ds,os,ts) = partitionDefs defs'
      cmap = buildCMap ds
      tmap = buildTMap ts
  in ds ++ ts ++  map (setTypeSigs tmap) os


-- set the boxity of Monotypes in Constrs and TDefns
-- given a map from Con to datatype-defining MCons
setBoxity :: Map.Map Con Monotype -> Defn -> Defn
setBoxity cmap def =
  case def of
   ODefn{} -> def
   TDefn{mtyp} ->
     let sb m = case m of
                  MCon _ c ms -> case Map.lookup c cmap of
                    Just (MCon b _ _) -> MCon b c (map sb ms)
                    _ -> error $ "unknown concrete type reference: " ++ c
                  _ -> m
     in def{mtyp = sb mtyp}
   DDefn{dcons} ->
     let dcons' = map mfun2 dcons
         mfun2 d@DCon{mtyps} = d{mtyps = map mfun mtyps}
         mfun (MCon _ c ms) = let ms' = map mfun ms
                                  bxt = case c of
                                          "Int_h" -> False
                                          "Double_h" -> False
                                          _ ->
                                              case Map.lookup c cmap of
                                                Just (MCon b _ _) -> b
                                                _ -> error $ "cmap lookup failed for " ++ c
                              in MCon bxt c ms'
         mfun (MFun m1 m2) = MFun (mfun m1) (mfun m2)
         mfun m = m
     in def{dcons = dcons'}
          

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
Rename all user identifiers uniquely, adding common prefixes to make
generating variables in other transforms easier.
-------------------------------------------------------------------------}

renameIDs defs =
  let rmap = Map.singleton "main" "main"
      nmap = Map.singleton "main" 0
      (defs', _) = runState (rename rmap defs) nmap
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
        defns' <- rename env defns
        let defenv = Map.fromList $ zip (map bnd defns) (map bnd defns')
            env' = Map.union defenv env
        eexp' <- rename env' eexp
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

genFunctions defs =
  let (ds,os,ts) = partitionDefs defs
      constrs = concatMap dcons ds
      dict = Map.fromList $ map (\DCon{dcon, mtyps} -> (dcon, length mtyps)) constrs
      (cons, pops, defs') = genfuns defs
      conLs = Set.toList cons
      popLs = Set.toList pops
      defns = makefuns popLs conLs dict
  in defns ++ defs'

primDict = map swap primopTab


getNum = do
  i <- get
  put (i+1)
  return i

getNums n =
  case n of
   0 -> return []
   _ -> do
     i <- getNum
     is <- getNums (n - 1)
     return (i:is)
    

makefuns :: [Primop] -> [Con] -> Map.Map Con Int -> [Defn]
makefuns pops cons dict =
  
  let mfun1 c = case Map.lookup c dict of
                 Nothing -> error $ "could not find Con to generate function: " ++ c
                 Just 0 -> return $ ODefn ("gfc_" ++ c) (EAt $ AtmCon c) Nothing
                 Just l ->
                   do
                     nums <- getNums l
                     let vars = zipWith (++) (repeat "gac_") (map show nums)
                         pats = map Default vars
                         fexp = EAt $ AtmCon c
                         args = map (EAt . AtmVar) vars
                         eexp = foldl EAp fexp args
                         oexp = EFn pats eexp
                     return $ ODefn ("gfc_" ++ c) oexp Nothing

      mfun2 p = case lookup p primDict of
                 Nothing -> error $ "Couldn't find primop in table: " ++ show (pprint p)
                 Just op -> 
                   do
                     nums <- getNums (primArity p)
                     let vars = zipWith (++) (repeat "gap_") (map show nums)
                         pats = map Default vars
                         fexp = EAt $ AtmOp p
                         args = map (EAt . AtmVar) vars
                         eexp = foldl EAp fexp args
                         oexp = EFn pats eexp        
                     return $ ODefn ("gfp_" ++ op) oexp Nothing
      (cdefs,_) = runState (mapM mfun1 cons) 0
      (pdefs,_) = runState (mapM mfun2 pops) 0
  in cdefs ++ pdefs
 
{-
tryCon :: Map.Map Con Int -> Exp -> State Int (Maybe Exp)
tryCon dict e = case e of
  EAp{fexp, eexp} -> 
    let (f, args) = unfoldEAp e
    in case f of
        EAt{atm = AtmCon c} ->
          case Map.lookup c dict of
           i | i == length args ->
                 
           
          _
        
  -- Don't want constructors with 0 arity like Nil, True or False
  -- that would create many let-bound instances of the same
  -- constructor, as opposed to one at top level
  _ -> Nothing
-}

type CGenSet = Set.Set Con
type PGenSet = Set.Set Primop

class GenFuns a where
  genfuns ::  a -> (CGenSet, PGenSet, a)

instance (GenFuns a) => GenFuns [a] where
  genfuns as =
    let (cgss, pgss, as') = unzip3 $ map genfuns as
    in (Set.unions cgss, Set.unions pgss, as')
  
instance GenFuns Defn where
  genfuns d = case d of
    DDefn{} -> (Set.empty, Set.empty, d)
    TDefn{} -> (Set.empty, Set.empty, d)
    ODefn{oexp} ->
      let (cgs, pgs, oexp') = genfuns oexp
      in (cgs, pgs, d {oexp = oexp'})

instance GenFuns Exp where
  genfuns e = case e of
    EAt{atm} -> case atm of
      AtmCon c -> let v = "gfc_" ++ c
                  in (Set.singleton c, Set.empty, e{atm = AtmVar v})
      AtmOp o -> let op = case lookup o primDict of
                       Just n -> n
                       Nothing -> error $ "Couldn't find primop in table: " ++ show (pprint o)
                     v = "gfp_" ++ op
                 in (Set.empty, Set.singleton o, e{atm = AtmVar v})      
      _ -> (Set.empty, Set.empty, e)

    EAp{fexp, eexp} -> 
      let (cgs1, pgs1, fexp') = genfuns fexp
          (cgs2, pgs2, eexp') = genfuns eexp
      in (Set.union cgs2 cgs1,
          Set.union pgs2 pgs1,
          e {fexp = fexp',
             eexp = eexp'})

    EFn{eexp} ->
      let (cgs, pgs, eexp') = genfuns eexp
      in (cgs, pgs, e {eexp = eexp'})

    ELt{eexp, defns} ->
      let (cgs1, pgs1, eexp') = genfuns eexp
          (cgs2, pgs2, defns') = genfuns defns
      in (Set.union cgs2 cgs1,
          Set.union pgs2 pgs1,
          e{eexp = eexp',
            defns = defns'})

    ECs{eexp, cls} ->
      let (cgs1, pgs1, eexp') = genfuns eexp
          (cgs2, pgs2, cls') = genfuns cls
      in (Set.union cgs2 cgs1,
          Set.union pgs2 pgs1,
          e {eexp = eexp',
             cls = cls'})

instance GenFuns Clause where
  genfuns (pat, exp) =
    let (cgs, pgs, exp') = genfuns exp
    in (cgs, pgs, (pat, exp'))
                         

{--------------------------------------------------------------------------
Convert expression application to atomic function application
e.g. e1 e2 e3 -> let f = e1; a1 = e2; a2 = e3; in f a1 a2
--------------------------------------------------------------------------}

simplifyExps defs =
  let (defs', _) = runState (mapM smplExp defs) (0,0)
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
     
class SimpleExp a where
  smplExp :: a -> State (Int,Int) a

instance SimpleExp Defn where
  smplExp d = case d of
    DDefn{} -> return d
    TDefn{} -> return d
    ODefn{oexp} ->
      do
        oexp' <- smplExp oexp
        return $ d{oexp = oexp'}

instance SimpleExp Exp where
  smplExp e = case e of
    EFn{eexp} ->
      do
        eexp' <- smplExp eexp
        return $ e {eexp = eexp'}
        
    ELt{eexp, defns} ->
      do
        eexp' <- smplExp eexp
        defns' <- mapM smplExp defns
        return $ e {eexp = eexp',
                    defns = defns'}

    ECs{eexp, cls} ->
      do
        eexp' <- smplExp eexp
        cls' <- mapM smplExp cls
        return $ e {eexp = eexp',
                    cls = cls'}

    EAt{} -> return e

    -- the ugly one
    EAp{} ->
      do
        -- unfold and desugar expressions in application
        let (fe, es) = unfoldEAp e
        fe' <- smplExp fe
        es' <- mapM smplExp es

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
           EAt{} -> error $ "bad EAt in Transform.smplExp: " ++ show (pprint fe')
           _ -> do
             v <- genFunVar
             return (EAt $ AtmVar v, ODefn {bnd = v, oexp = fe', mmtype = Nothing}:defs)

        -- build the expression (if defs generated, make let, otherwise, just expr)
        return $ let exp = foldl EAp f args in
                  case truDefs of
                   [] -> exp
                   _  -> ELt{eexp = exp,
                             defns = truDefs}

instance SimpleExp Clause where
  smplExp (pat, exp) =
    do
      exp' <- smplExp exp
      return (pat, exp')



{---------------------------------------------------------------------------------
Desugar numeric literals to Boxed Int/Doubles

This transformation adds its own datatype definitions for boxed Ints
and Doubles as well as functions that call to them in the same
convention as that used when generating functions for constructors
used by a program.  This keeps the transform from being reliant on any
previous transform while still compliant with the naming convention
used to generate the abstract STG

Note to future David (from past David) so you don't panic:

This does not modify unboxed literals. It does create top level boxed
objects for each unique literal number.  Unboxed literals do not need
to be processed specially, or, at least not here.  Their behavior in
MHS should be identical to that found in MagicHash programs (i.e. no
recursive bindings, including at the top level).
-----------------------------------------------------------------------------------}



makeInt i = let n = iname i
                f = EAt $ AtmCon "I#"
                e = EAt $ LUBInt i
            in ODefn n (EAp f e) (Just $ mtyp intCon)

makeDbl d = let n = dname d
                f = EAt $ AtmCon "D#"
                e = EAt $ LUBDbl d
            in ODefn n (EAp f e) (Just $ mtyp dblCon)


isInteger s = all isNumber s
isDouble s | '.' `elem` s = let (w,(p:ps)) = break (== '.') s
                            in isInteger w && isInteger ps
           | otherwise = False

boxNumLiterals :: [Defn] -> [Defn]                
boxNumLiterals defs =
  let ((is,ds), defs') = boxnums defs
      idefs = map makeInt $ Set.toList is
      ddefs = map makeDbl $ Set.toList ds
  in idefs ++ ddefs ++ defs'

type NumSet = (Set.Set Int, Set.Set Double)

numUnion (is1,ds1) (is2,ds2) =
  (Set.union is1 is2, Set.union ds1 ds2)
numUnions = foldl1 numUnion
numEmpty = (Set.empty, Set.empty)
iname i = "gvi_" ++ show i
dname d = "gvd_" ++ show d

class BoxNums a where
  boxnums ::  a -> (NumSet, a)

instance (BoxNums a) => BoxNums [a] where
  boxnums as =
    let (nset, as') = unzip $ map boxnums as
    in (numUnions nset, as')
        

instance BoxNums Defn where
  boxnums d = case d of
    DDefn{} -> (numEmpty, d)
    TDefn{} -> (numEmpty, d)
    ODefn{oexp} ->
      let (nset, oexp') = boxnums oexp
      in (nset, d{oexp = oexp'})
         
instance BoxNums Exp where
  boxnums e = case e of
    EAt{atm} -> case atm of
                 LBInt i -> ((Set.singleton i, Set.empty), e{atm = AtmVar $ iname i})
                 LBDbl d -> ((Set.empty, Set.singleton d), e{atm = AtmVar $ dname d})
                 _ -> ((Set.empty, Set.empty), e)

    EAp{fexp,eexp} ->
      let (nset1, fexp') = boxnums fexp
          (nset2, eexp') = boxnums eexp
      in (numUnion nset1 nset2, e {fexp = fexp',
                                   eexp = eexp'})

    EFn{pats,eexp} ->
      let (nset1, pats') = boxnums pats
          (nset2, eexp') = boxnums eexp
      in (numUnion nset1 nset2, e{pats = pats',
                                  eexp = eexp'})

    ECs{eexp,cls} ->
      let (nset1, eexp') = boxnums eexp
          (nset2, cls')  = boxnums cls
      in (numUnion nset1 nset2, e{eexp = eexp',
                                  cls = cls'})

    ELt{eexp, defns} ->
      let (nset1, eexp') = boxnums eexp
          (nset2, defns') = boxnums defns
      in (numUnion nset1 nset2, e{eexp = eexp',
                                  defns = defns'})

instance BoxNums Clause where
  boxnums (pat, exp) =
    let (nset1, pat') = boxnums pat
        (nset2, exp') = boxnums exp
    in (numUnion nset1 nset2, (pat', exp'))

instance BoxNums Pattern where
  boxnums pat =
    case pat of
     Default{} -> (numEmpty, pat)
     Match{str, npats} ->
       let (_,npats') = boxnums npats
           numstr = str ++ "#"
       in case () of
           _ | isInteger str ->
               (numEmpty, Match {str = "I#", npats = [Match numstr npats']})
             | isDouble str -> 
               (numEmpty, Match {str = "D#", npats = [Match numstr npats']})
             | otherwise -> (numEmpty, pat {npats = npats'})
        

{---------------------------------------------------------------------

---------------------------------------------------------------------}

simplifyPats :: [Defn] -> [Defn]
simplifyPats defs =
  let (ds,os,ts) = partitionDefs defs
      cons = concatMap dcons ds
      (os',_) = runState (dsgpats cons os) 0
  in ts ++ ds ++ os'
     
type Equation = ([Pattern], Exp)

instance Unparse Equation where
  unparse (pats,exp) =
    hsep (map unparse pats) <+> arw <+> unparse exp

instance Unparse [Equation] where
  unparse = vcat . map unparse

patBinds :: Pattern -> [Var]
patBinds Default{str} = [str]
patBinds Match{str, npats} = str : concatMap patBinds npats

lookupOrID a = maybe a id . Map.lookup a    

caseVar =
  do
    i <- get
    put (i+1)
    return ("gvc_" ++ show i)

caseVars n =
  case n of
   0 -> return []
   _ -> do
     v <- caseVar
     vs <- caseVars (n-1)
     return (v:vs)
   

-- in a list of pattern lists, check if each list has a
-- Default pattern at the head
allVars :: [[Pattern]] -> Bool
allVars pats = all isDefaultPat $ map head pats

allCons :: [[Pattern]] -> Bool
allCons pats = all (not . isDefaultPat) $ map head pats

-- filter equations that are hidden by another
rmHidden = id
rmHidden' :: [Equation] -> [Equation]
rmHidden' = foldr (\(ps1,_) pps ->
                   filter (\(ps2,_) -> 
                            and $ zipWith hides ps1 ps2) pps) []
  where hides Default{} _ = True
        hides _ Default{} = False
        hides (Match c1 ps1) (Match c2 ps2) = c1 == c2 &&
                                              and (zipWith hides ps1 ps2)


testEQs =
  [([Match "Cons" [Default "a", Match "I#" [Match "1#"[]]], Match "X" []], EAt $ AtmVar "a"),
   ([Match "Cons" [Default "a", Match "Cons" [Default "b", Default "c"]], Default "x"], EAt $ AtmVar "b"),
   ([Default "a", Match "Cons" [Default "b", Match "Nil" []]], EAt $ AtmVar "ABBA rules"),
   ([Match "X" [], Default "xx"], EAt $ AtmVar "xx"),
   ([Match "Nil" [], Match "Nil" []], EAt $ AtmVar "dunno"),
   ([Match "Cons" [Default "a", Default "b"], Default "l"], EAt $ AtmVar "Family matters"),
   ([Default "x", Match "Nil" []], EAt $ AtmVar "whee"),
   ([Default "x", Default "y"], EAt $ AtmVar "whoo")]

oddFn = ODefn "odd"
         (EFn [Default "x"]
          (ECs (EAt $ AtmVar "x")
           [(Match "I#" [Match "1#" []], EAt $ AtmVar "true"),
            (Default "x", EAt $ AtmVar "false")]))
         Nothing
         

testFuns =
  let efns = map (uncurry EFn) testEQs
      fns = map (\x -> ODefn "fun" x Nothing) efns
      lcon = DDefn (MCon True "List" [MVar "a"])
              [DCon "X" [] ["X","Nil","Cons"],
               DCon "Nil" [] ["X","Nil","Cons"],
               DCon "Cons" [MVar "a", MCon True "List" [MVar "a"]] ["X","Nil","Cons"]]
  in intCon : lcon : oddFn : fns
     

--Match "Cons" [Default "b", Default "c"]
match :: [Constr] -> [Var] -> [Equation] -> Exp -> State Int Exp
match _ [] [] dflt = return dflt -- I think

-- empty rule: No variables to match on
-- first expression in equations is the result
match _ [] ((pats , exp):eqs) dflt = return exp
match cons (v:vs) eqs dflt =
  let (patsLs, exps) = unzip eqs
  in case  () of
      _ | 1 /= length (nub (length (v:vs): map length patsLs)) ->
          error $ "malformed patterns detected\n" ++ show (pprint eqs)
      _ | allVars patsLs ->

      -- Variable rule:
      -- binding of scrutinee to variable i.e. Default Pattern
      -- If all the pattern lists start with a variable match, no need
      -- to make a case expression, simply substitute scrutinee variable
      -- in place of all the pattern-bound variables in the accompanying
      -- expressions
          let vars = map (str.head) patsLs
              -- map to use when renaming vars in expressions
              rmap = Map.fromList $ zip vars (repeat v)
              exps' = map (subnames rmap) exps
          in do
             exp <- match cons vs (zip (map tail patsLs) exps') dflt
             return exp

     -- Constructor rule:
     -- Matching specific constructors i.e. Match Pattern
     -- If all the pattern lists start with a constructor match,
     -- the case expression is simple to generate:
        | allCons patsLs ->
          let lst = (Default v, dflt)
              f [] = return []
              f eqs@((ps,_):_) =
                let
                  -- need Constructor name and arity of first top-level pattern
                  (Match c npats) = head ps

                  -- select all equations starting with the same constructor
                  (ms, rest) = partition ((== c) . str . head . fst) eqs
                  
                  -- remove Constructor from matches, add nested patterns
                  -- to remainder in each equation
                  ms' = map (\(Match _ nps : xs, exp) -> (nps ++ xs, exp)) ms

                in do
                  nvs <- caseVars (length npats)
                  let npats' = map Default nvs
                      -- new *simple* pattern to match on
                      pat = Match c npats'
                  -- build remainder of clauses
                  clss <- f rest                  
                  -- build expression for Constructor
                  exp <- match cons (nvs++vs) ms' dflt                  
                  -- add this clause to list and return
                  return $ (pat, exp):clss
          in do
            cls <- f eqs
            

            -- checking case exhaustion
            let cls' = exhaustClauses cons cls lst
                                   
            return $ supercompact [] $ ECs (EAt $ AtmVar v) $ cls'

      -- Mixture rule:
      -- There are both constructor and variable matches in list
      -- of equations:
      -- fold the "bottom" equations up
        | otherwise ->
          let eqs' = rmHidden eqs
              foldy [] = return dflt
              foldy (qs:qqs) =
                do
                  df <- foldy qqs
                  match cons (v:vs) qs df
              qs = groupBy ((==) `on` (isDefaultPat.head.fst)) eqs'
          in foldy qs

exhaustClauses cons cls fallThru =
  let (dfs,ms) = partition isDefaultPat $ map fst cls
  in case map str ms of
      c:cs | null dfs ->
               case find ((== c) . dcon) cons of
                Nothing | not $ isBoxedNum c ->
                            error $ "unknown constructor in pattern: " ++ c
                        | otherwise -> cls ++ [fallThru]
                                       
                Just DCon{cons} ->
                  case cons \\ (c:cs) of
                   [] -> cls
                   _ -> cls ++ [fallThru]
      _ -> cls

compact :: Exp -> Exp
compact e = case e of
  ECs e1 cs ->
    let mfun (p,ex) = case p of
          Default{str} -> case liftClauses [e1, EAt $ AtmVar str] ex of
            Just cs -> cs
            Nothing -> [(p,ex)]
          _ -> [(p,ex)]
        clauses = concatMap mfun cs
        cs' = rmOverlaps clauses
    in ECs e1 cs'
  _ -> e

rmOverlaps :: [Clause] -> [Clause]
rmOverlaps cls =
  foldr (\(p,ex) cs -> (p,ex):filter (not . (p `hidesPat`) . fst) cs) [] cls

supercompact :: [(Var, Pattern)] -> Exp -> Exp
supercompact env e = case e of
  ECs{eexp = EAt (AtmVar v), cls} ->
    case lookup v env of
     Just p1 -> let (ECs ex cs) = compact e
                    cs' = filter (\(p2,e)-> p1 `hidesPat` p2) cs
                in if null cs' then noExhaust
                   else snd $ head cs'
                        
     Nothing -> let (ECs ex cs) = compact e
                    mfun (p,e) = case () of
                      _ | isDefaultPat p -> (p, supercompact env e)
                        | otherwise -> (p, supercompact ((v,p):env) e)
                    cs' = map mfun cs
                in ECs ex cs'
  _ -> e
  

noExhaust = EAt $ AtmVar "noExhaust"
             
liftClauses :: [Exp] -> Exp -> Maybe [Clause]
liftClauses exs e = case e of
  ECs{eexp, cls} | eexp `elem` exs -> Just cls
  _ -> Nothing

simpleCase :: Exp -> Bool  
simpleCase e = case e of
  ECs{cls} ->
    let (_,ms) = partition isDefaultPat $ map fst cls
        (_,ms') = partition isDefaultPat $ concatMap npats ms
    in null ms' -- Defaults and Matches with only defaults nested are simple
  _ -> False

       
class DSGPats a where
  dsgpats :: [Constr] -> a -> State Int a
  
instance DSGPats [Defn] where
  dsgpats cons defs =
    let (ds, os, ts) = partitionDefs defs
        (fns, os') = partition (isEFn . oexp) os
        fgrps = groupAllBy ((==) `on` bnd) fns
        mfun (o:os) = (o,
                       length $ pats $ oexp o,
                       map (\f -> (pats $ oexp f, eexp $ oexp f)) (o:os))
        mfun _ = error "trying to build trips from empty objects"
        trips = map mfun fgrps
        tripFun (obj, arity, equations) =
          do
            vs <- caseVars arity
            eexp <- match cons vs equations noExhaust
            let oexp = EFn (map Default vs) eexp
            return $ obj{oexp = oexp}
    in
     do
       newFns <- mapM tripFun trips
       newOs <- mapM (dsgpats cons) (newFns ++ os')
       return (ts ++ ds ++ newOs)
       

instance DSGPats Defn where
  dsgpats cons d = case d of
    DDefn{} -> return d
    TDefn{} -> return d
    ODefn{oexp} ->
      do
        oexp' <- dsgpats cons oexp
        return d{oexp = oexp'}

instance DSGPats Exp where
  dsgpats cons e = case e of
    EAt{} -> return e
    ELt{eexp, defns} ->
      do
        defns' <- dsgpats cons defns
        eexp' <- dsgpats cons eexp
        return e{eexp = eexp',
                 defns = defns'}

    ECs{eexp, cls}
      | simpleCase e ->
          do
            cls' <- mapM (dsgpats cons) cls
            eexp' <- dsgpats cons eexp
            return e{eexp = eexp',
                     cls = cls'}
      | otherwise ->
          do
            cls' <- mapM (dsgpats cons) cls        
            eexp' <- dsgpats cons eexp
            v <- caseVar
            let eqs = map (\(p,e) -> ([p],e)) cls'
            -- guarding against simpleCase ensures this
            -- match never fails
            ECs _ cs <- match cons [v] eqs noExhaust
            return $ ECs eexp' cs

    EAp{fexp, eexp} ->
      do
        f' <- dsgpats cons fexp
        e' <- dsgpats cons eexp
        return e{fexp = f',
                 eexp = e'}

    EFn{pats, eexp}
      | all isDefaultPat pats ->
        do
          eexp' <- dsgpats cons eexp
          return $ e{eexp = eexp'}
      | otherwise ->
        do
          ex <- dsgpats cons eexp
          vs <- caseVars (length pats)
          eexp' <- match cons vs [(pats, ex)] noExhaust
          let pats' = map Default vs
          return e {pats = pats',
                    eexp = eexp'}

instance DSGPats Clause where
  dsgpats cons (p, e) =
    do
      e' <- dsgpats cons e
      return (p, e)
    

class SubNames a where
  subnames :: ReplMap -> a -> a

instance SubNames Defn where
  subnames rmap d = case d of
    ODefn{bnd, oexp} ->
      let rmap' = Map.delete bnd rmap -- occasionally redundant
          oexp' = subnames rmap' oexp
      in d { oexp = oexp' }

    _ -> d -- don't mess with other definitions

instance SubNames Exp where
  subnames rmap e = case e of
    EAp {fexp, eexp} ->
      let fexp' = subnames rmap fexp
          eexp' = subnames rmap eexp
      in e {fexp = fexp',
            eexp = eexp'}

    EFn {pats, eexp} ->
      let rmap' = deleteAll (concatMap patBinds pats) rmap
          eexp' = subnames rmap' eexp
      in e {eexp = eexp'}

    ECs {eexp, cls} ->
      let eexp' = subnames rmap eexp
          cls' = map (subnames rmap) cls
      in e {eexp = eexp',
            cls = cls'}

    ELt {eexp, defns} ->
      let rmap' = deleteAll (map bnd defns) rmap
          eexp' = subnames rmap' eexp
          defns' = map (subnames rmap') defns
      in e {eexp = eexp',
            defns = defns'}

    EAt {atm} ->
      case atm of
       AtmVar v -> let v' = lookupOrID v rmap
                   in e {atm = AtmVar v'}
       _ -> e

instance SubNames Clause where
  subnames rmap (pat,exp) =
    let rmap' = deleteAll (patBinds pat) rmap
        exp' = subnames rmap' exp
    in (pat, exp')

instance SubNames Equation where
  subnames rmap eqs = undefined
