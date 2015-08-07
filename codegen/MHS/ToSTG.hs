{-# LANGUAGE
NamedFieldPuns,
TypeSynonymInstances,
FlexibleInstances,
MultiParamTypeClasses#-}

module MHS.ToSTG
(
  makeSTG
) where

import MHS.Transform
import MHS.AST
import Analysis (defAlt)
import BU (Assumption, Assumptions)
import PPrint
import State
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (isPrefixOf, partition)
import Data.Function (on)
import Debug.Trace

makeSTG :: [Defn] -> ([TyCon], [Obj ()], Assumptions)
makeSTG defs =
  let (ds, os, _) = partitionDefs defs
      dcmap = makeDCMap ds
      tycons = map stgTycon ds
      (objs,_) = runState (mapM (stgify dcmap) os) (0,0)
      assums = makeAssumptions os
  in (tycons, objs, assums)



class Assume a where
  assume :: a -> State (Int, Assumptions) ()

insertAS :: Assumption -> State (Int, Assumptions) ()
insertAS (v,m) =
  do
    mtyp <- newMtyp m Map.empty
    (i,ass) <- get
    let ass' = Set.insert (v,mtyp) ass
    put (i,ass')

monoVar :: State (Int, Assumptions) Var
monoVar =
  do
    (i,ass) <- get
    put (i+1, ass)
    return $ "tsv_" ++ show i

newMtyp :: Monotype -> Map.Map Var Var -> State (Int, Assumptions) Monotype
newMtyp m dict =
  let foldy dict [] = return []
      foldy dict (m:ms) =
        do
          m' <- newMtyp m dict
          let newDict = makeDict m' m
          ms' <- foldy newDict ms
          return $ m':ms'

      makeDict (MVar v1) (MVar v2) = Map.singleton v1 v2
      makeDict (MFun m1 m2) (MFun n1 n2) = Map.union
                                           (makeDict m1 n1)
                                           (makeDict m2 n2)
      makeDict (MCon _ c1 ms) (MCon _ c2 ns) =
        Map.unions (Map.singleton c1 c2 : zipWith makeDict ms ns)
      makeDict _ _ = Map.empty
        
  in case m of
      MCon b c ms ->
        do
          ms' <- foldy dict ms
          return $ MCon b c ms'
       
      MVar v ->
        do
          v' <- monoVar
          return $ MVar v'
          
      MFun m1 m2 ->
        do
          [m1',m2'] <- foldy dict [m1,m2]
          return $ MFun m1' m2'
          
      m -> return m


makeAssumptions :: [Defn] -> Assumptions
makeAssumptions defs =
  let (_,(_,as)) = runState (mapM_ assume defs) (0, Set.empty)
  in as

instance Assume Defn where
  assume def = case def of
    ODefn{bnd,oexp,mmtype} ->
      case mmtype of
       Nothing -> assume oexp
       Just m ->
         do
           assume oexp
           insertAS (bnd,m)
               
    _ -> error $ unlines
         ["ToSTG.stgAssum given non ODefn arg:",
          show $ pprint def]

instance Assume Exp where
  assume e = case e of
    EAt{} -> return ()
    EAp{fexp,eexp} -> assume fexp >> assume eexp
    ECs{eexp,cls} -> assume eexp >> mapM_ assume cls
    ELt{eexp,defns} -> assume eexp >> mapM_ assume defns
    EFn{eexp} -> assume eexp

instance Assume Clause where
  assume (_,e) = assume e

makeDCMap :: [Defn] -> DCMap
makeDCMap defs =
  case stgbl defs of
  Just e -> error $ show e
  Nothing ->
    let dcs = concatMap dcons defs
        mfun d = case d of
                  DCon{dcon} -> (dcon,d)
    in Map.fromList $ map mfun dcs


stgTycon :: Defn -> TyCon
stgTycon ddef =
  case stgbl ddef of
   Just e -> error $ show e
   Nothing ->
     case ddef of
      DDefn{mtyp,dcons} ->
        let (MCon b c vs) = mtyp
            vars = map (\(MVar v) -> v) vs
            dcs = map (\DCon{dcon, mtyps} -> DataCon dcon mtyps) dcons
        in TyCon b c vars dcs
      _ -> error $ unlines
           ["ToSTG.stgTycon passed non DDefn argument",
            show $ pprint ddef]
                    
                                        
      
addErr (Just d1) (Just d2) = Just (d1 $+$ d2)
addErr (Just d1) _ = Just d1
addErr _ x = x
addErrs = foldr addErr Nothing
isErr = maybe False (const True)
fromErr = maybe empty id

mkErr b err = if b then Just err else Nothing
mkErrs tups = addErrs $ map (uncurry mkErr) tups

isUniqueBy _ [] = True
isUniqueBy f (x:xs) = not (any (f x) xs) &&
                      isUniqueBy f xs
              
class STGable a where
  stgbl :: a -> Maybe Doc

instance STGable [Defn] where
  stgbl defs =
    let (ds, os, ts) = partitionDefs defs
        dcerr d1 d2 = 
          let (DDefn (MCon _ c1 _) dcs1) = d1
              (DDefn (MCon _ c2 _) dcs2) = d2
              e1 = mkErr (c1 == c2 )
                   (text "Overlapping datatype definitions:" <+> text c1 <+> text c2)
              e2 = mkErr (not (isUniqueBy ((==) `on` dcon) (dcs1 ++ dcs2)))
                   (text "Overlapping data constructors")
          in addErr e1 e2

        dcerrs (x:y:xs) = addErr (dcerr x y) (dcerrs (y:xs))
        dcerrs xs = Nothing
        dser1 = dcerrs ds
        dser2 = addErrs $ map stgbl ds
        oser = addErrs $ map stgbl os
    in mkErrs
       [(not $ null ts,
         text "Don't try to stgify Type Signatures"),
        (isErr dser1,
         fromErr dser1),
        (not $ isUniqueBy ((==) `on` bnd) os,
         text "Overlapping object definitions:" $+$
         hsep (punctuate comma $ map (text.bnd) os)),
        (isErr dser2,
         fromErr dser2),
        (isErr oser,
         fromErr oser)]

instance STGable Defn where
  stgbl d = case d of
    TDefn{} -> Just $ text "don't try to stgify a type signature!"

    ODefn{bnd, oexp} ->
      let exerr = stgbl oexp
          oerr = mkErr (isErr exerr)
                 (text "Can't translate object" <+> text bnd <+> text "into STG:")
      in addErr oerr exerr
         
    DDefn{mtyp, dcons} -> Nothing -- short of duplicates, DDefns should be fine

instance STGable Exp where
  stgbl e = case e of
    EAt{} -> Nothing
    ELt{eexp, defns} ->
      let
        exerr = stgbl eexp
        derr = stgbl defns
      in addErr exerr derr
    ECs{eexp, cls} ->
      let
        cerr = mkErr (not $ simpleCase e)
               (text "case expression patterns not simplified:" $+$ pprint cls)
        exerr = stgbl eexp
        clserr = addErrs $ map stgbl cls
      in addErrs [cerr, exerr, clserr]

    EFn{pats, eexp} ->
      let fnerr = mkErr (not $ all isDefaultPat pats)
                  (text "function patterns not simplified:" $+$ pprint pats)
          exerr = stgbl eexp
      in addErr fnerr exerr

    EAp{} ->
      let (f,args) = unfoldEAp e
          ferr = mkErr (not $ isEAt f)
                 (text "function applied is not in atomic form:" $+$ pprint f)
          argerr = mkErr (not $ all isEAt args)
                   (text "function arguments are not in atomic form:" $+$ pprint args)
      in addErr ferr argerr


instance STGable Clause where
  stgbl (p,e) = addErr (stgbl p) (stgbl e)

instance STGable Pattern where
  stgbl p = case p of
    Default{} -> Nothing
    Match{npats} -> mkErr (not $ all isDefaultPat npats)
                    (text "Non-simple pattern found:" <+> pprint p)

  
class ToSTG a b where
  stgify :: DCMap -> a -> STGState b

type DCMap = Map.Map Con Constr
type STGState a = State (Int,Int) (a ())

letVar :: State (Int,Int) Var
letVar =
  do
    (i,j) <- get
    put (i+1, j)
    return $ letvarPfx ++ show i

altsVar :: State (Int,Int) Var
altsVar =
  do
    (i,j) <- get
    put (i, j+1)
    return $ altsPfx ++ show j    

altsPfx = "gal_"  
letvarPfx = "glv_"
gcfunPfx = "gfc_"
gpfunPfx = "gfp_"

conCall = (gcfunPfx `isPrefixOf`)
conAr dcmap str =
  let s = drop (length gcfunPfx) str
      Just DCon{mtyps} = Map.lookup s dcmap
  in length mtyps

primCall = (gpfunPfx `isPrefixOf`)

primAr :: Var -> Int
primAr str =
  let s = drop (length gpfunPfx) str
      Just op = lookup s primopTab
  in primArity op


makeObj :: DCMap -> Exp -> Var -> STGState Obj
makeObj dcmap e name =
  case e of
    EAp{fexp,eexp} ->
      let (f,args) = unfoldEAp e
          EAt{atm} = f
      in case atm of
          AtmCon c -> conit c args
          AtmVar v -> thunk            
          AtmOp p -> thunk
          a -> error $ unlines
               ["attempt to apply literal as function",
                show $ pprint e]

    EAt{atm} -> case atm of
      AtmCon c -> conit c []
      AtmVar v -> thunk
      LUBInt _ -> error "illegal recursive binding of unboxed int"
      LUBDbl _ -> error "illegal recursive binding of unboxed double"
      _ -> error "ToStg.makeObj: shouldn't happen"
      
    EFn{pats,eexp} ->
      do
        let vs = map str pats
        expr <- stgify dcmap eexp
        return $ FUN () vs expr name

    _ -> thunk
  where
    conit :: Con -> [Exp] -> STGState Obj
    conit c args =
      case Map.lookup c dcmap of
       Just DCon{mtyps}
         | length mtyps == length args ->
             do
               eas <- mapM (stgify dcmap) args
               let as = map ea eas
               return $ CON () c as name
               
         | otherwise ->
             error $ unlines
             ["ToStg.makeObj.conOrThunk:",
              "why is there a definition with a partially applied Con still?"]
            
       Nothing -> error $ "can't find con in map: " ++ c
    thunk = do 
      expr <- stgify dcmap e
      return $ THUNK () expr name
    

instance ToSTG Defn Obj where
  stgify dcmap d = case stgbl d of
    Just e -> error $ show e
    Nothing ->
      case d of      
       ODefn{bnd,oexp} -> makeObj dcmap oexp bnd
       _ -> error "Why is stgify given non-ODefn?"
      

letCon :: DCMap
       -> Con -- name of the CON to be applied
       -> [Exp] -- Exp args to the CON
       -> STGState Expr -- Return ELet
letCon dcmap conName args =
  do
    eas <- mapM (stgify dcmap) args
    letv <- letVar
    let as = map ea eas
        con = CON () conName as letv
        expr = EAtom () $ Var letv
    return $ ELet () [con] expr

instance ToSTG Exp Expr where
  stgify dcmap e = case stgbl e of
    Just e -> error $ show e
    Nothing ->
      case e of
       EAt{atm} -> case atm of
         LUBInt i -> return $ EAtom () $ LitI i
         LUBDbl d -> return $ EAtom () $ LitD d
         AtmVar v -> return $ EAtom () $ Var v
         _ -> error $ unlines
              ["EAt in stgify given non var, non UB lit arg:",
               show $ pprint e]

       EAp{fexp, eexp} ->
         let (f, args) = unfoldEAp e
         in case f of
             EAt{atm = AtmVar v}
               | conCall v &&
                 conAr dcmap v == length args ->
                   let conName = drop (length gcfunPfx) v
                   in letCon dcmap conName args
                            
               | primCall v &&
                 length args == primAr v ->
                   do
                     let v' = drop (length gpfunPfx) v
                         Just op = lookup v' primopTab
                     eas <- mapM (stgify dcmap) args
                     return $ EPrimop () op eas
                         
               | otherwise ->
                   do
                     eas <- mapM (stgify dcmap) args
                     return $ EFCall () v eas

            -- previous transforms guarantee that this is only
            -- a result of generated functions, so we can safely
            -- make let-bound CON expression
             EAt{atm = AtmCon c} -> letCon dcmap c args              
                       
             -- previous transforms guarantee that this is only
             -- a result of generated functions.
             EAt{atm = AtmOp o} ->
               do
                 eas <- mapM (stgify dcmap) args
                 return $ EPrimop () o eas
                 
             _ -> error $ unlines
                  ["EAp in stgify not simplified:",
                   show $ pprint e]

       EFn{pats, eexp} ->
         error $ unlines
         ["EFn passed to stgify.  Should only occur at top level",
          "expression of a recursive binding:",
          show $ pprint e]

       ECs{eexp, cls} ->
         do
           expr <- stgify dcmap eexp
           ealts <- stgify dcmap cls
           return $ ECase () expr ealts

       ELt{eexp, defns} ->
         do
           let (_, defs) = partition isTDefn defns
           odefs <- mapM (stgify dcmap) defs
           expr <- stgify dcmap eexp
           return $ ELet () odefs expr

instance ToSTG [Clause] Alts where
  stgify dcmap cls = case addErrs $ map stgbl cls of
    Just e -> error $ show e
    Nothing -> 
      do
        aname <- altsVar
        alts <- mapM (stgify dcmap) cls
        return $ Alts () alts aname

instance ToSTG Clause Alt where
  stgify dcmap (pat, exp) = case stgbl (pat,exp) of
    Just e -> error $ show e
    Nothing ->
      case exp of
       EAt{atm = AtmVar "noExhaust"} ->
         let var = str pat
             exp = EFCall{emd = error "emd not set stg_case_not_exhaustive",
                         ev = "stg_case_not_exhaustive",
                         eas = [EAtom {emd = error "emd not set stg_case_not_exhaustive",
                                       ea = Var var}]}
         in return $ ADef {amd = error "amd not set stg_case_not_exhaustive",
                           av = var,
                           ae = exp}

                               
       _ ->
         do
           expr <- stgify dcmap exp
           return $       
             case pat of
              Default{str} ->
                ADef () str expr
              Match s npats ->
                let con = if isBoxedNum s
                          then init s else s
                in ACon () con (map str npats) expr

         
                
