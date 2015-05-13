{-# LANGUAGE FlexibleInstances #-}

module ADT (
  Def(..),
  Boxedness(..),
  TyCon(..),
  DataCon(..),
  Atype(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  Boxedtype(..),
  Unboxedtype(..),
  TyConParam(..),
  TyConMap,
  DataConParam(..),
  DataConMap,
  ConMaps,
  onObjs,
  getObjs,
  getDatas,
  splitDefs,
  unsplitDefs,
  updatedata
) where

import AST

import Data.Maybe
import Control.Monad.State
import qualified Data.Map as Map

{-
  Algebraic Datatypes:
    
  Ref:  Unboxed Values as First-Class Citizens
  
  data Def (pg 6):
  data \Chi \alpha_1 .. \alpha_t =
  c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1
  
  \Chi -- type constructor
  \c_x -- data constructor
  
  and unboxed version on pg 26
  data unboxed \Chi# \alpha_1 .. \alpha_t =
  c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1
  
  Polytype      \sigma ::=  \forall \alpha . \sigma | \tau

  Monotype      \tau   ::=  \pi | \nu

  Boxed type    \pi    ::=  \alpha                   Type variable
                        |   \tau -> \tau             Function type
                        |   \Chi \pi_1 ... \pi_n   Parameterized boxed data type

  Unboxed type \nu     ::=  Int#
                        |   Double#
                        |   Bool#
                        |   \Chi# \pi_1 ... \pi_n   Parameterized boxed data type

-}


data Def a = ObjDef (Obj a)
             | DataDef TyCon
               deriving(Eq,Show)

data Boxedness = Boxed | Unboxed deriving(Eq,Show)
 
-- Boxed: data \Chi \alpha_1 .. \alpha_t =
-- c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1  
-- Unboxed: data unboxed \Chi# \alpha_1 .. \alpha_t =
-- c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1   
data TyCon = TyCon Boxedness Con [TyVar] [DataCon]
             deriving(Eq,Show)
             
-- Boxed True: c_x \tau_x1 .. \tau_xa_1 
-- Boxed False: c_x# \tau_x1 .. \tau_xa_1  
data DataCon = DataCon Boxedness Con [Monotype]
               deriving(Eq,Show)

-- \alpha                     
type TyVar = String

data Polytype = PPoly [TyVar] Monotype  -- uncurried forall
              | PMono Monotype
                deriving(Eq,Show)
                
-- \tau w/ addition of Atype (see below)
data Monotype = Mono Atype
              | MBoxed Boxedtype
              | MUnboxed Unboxedtype
                deriving(Eq,Show)

-- Atype is used on first pass,it is then converted to Boxed/Unboxed   
-- this is the first def of \tau from pg 6 of the paper
-- also atype from haskell 2010 standard
-- https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2
data Atype = ATyVar TyVar
           | AInt
           | ADouble
           | AFun Atype Atype
           | ATyCon Con [Atype]
             deriving(Eq,Show)              

-- \pi
data Boxedtype = BTyVar TyVar
               | BFun Monotype Monotype
               | BTyCon Con [Boxedtype]  
                 deriving(Eq,Show)
-- \nu
data Unboxedtype = UInt 
                 | UDouble
                 | UBool
                 | UTyCon Con [Boxedtype] 
                   deriving(Eq,Show)

-- Type constr name to arity, tag, boxed/unboxed
data TyConParam = TyConParam {tarity :: Int, 
                              ttag :: Int, 
                              tboxed :: Boxedness}
                  deriving(Eq,Show)

type TyConMap = Map.Map String TyConParam

-- Data constr name to (arity, tag, boxedness, type constr name)
data DataConParam = DataConParam {darity :: Int, 
                                  dtag :: Int, 
                                  dboxed :: Boxedness, 
                                  dtycon :: Con} 
                    deriving(Eq,Show)

type DataConMap = Map.Map String DataConParam

type ConMaps = (TyConMap, DataConMap)
         
 -- starting tycon map
tyconmap :: TyConMap
tyconmap = Map.insert "Bool#" (TyConParam 0 0 Unboxed)
         $ Map.insert "Int#"  (TyConParam 0 1 Unboxed)
         $ Map.insert "Bool"  (TyConParam 0 2 Boxed) -- data Bool = B Bool#
         $ Map.insert "Int"   (TyConParam 0 3 Boxed) -- data Int = I Int#
         $ Map.insert "List"  (TyConParam 1 4 Boxed) -- data List a = Nil | Cons a (List a)
           Map.empty

-- starting datacon map
-- these tags must match what is in stg_header.h
dataconmap :: DataConMap
dataconmap = Map.insert "False_h" (DataConParam 0 0 Unboxed "Bool#") 
           $ Map.insert "True_h"  (DataConParam 0 1 Unboxed "Bool#") 
           $ Map.insert "B"     (DataConParam 1 2 Boxed "Bool") 
           $ Map.insert "I"     (DataConParam 1 3 Boxed "Int") 
           $ Map.insert "Unit"  (DataConParam 0 4 Boxed "Unit")
           $ Map.insert "Nil"   (DataConParam 0 5 Boxed "List") 
           $ Map.insert "Cons"  (DataConParam 2 6 Boxed "List")
             Map.empty

buildconmaps :: [Def a] -> ConMaps
buildconmaps objs = execState (build [] objs) 
                    (tyconmap, dataconmap)
                    
updatedata :: [Def a] -> ([Def a], ConMaps)
updatedata inp = (update (fst conmaps) inp, conmaps)
                 where conmaps = buildconmaps inp

class BuildConMaps t where build :: String -> t -> State ConMaps()
                                                    
instance BuildConMaps a => BuildConMaps [a] where
  build = mapM_ . build

instance BuildConMaps (Def a) where
  build _ (DataDef tycon) = build [] tycon
  build _ _ = return ()

instance BuildConMaps TyCon where
  build _ (TyCon boxed con tyvars dcons) 
    = tyconinsert con (length tyvars) boxed >> 
      build con dcons
     
instance BuildConMaps DataCon where
  build tycon (DataCon boxed con mts) = 
    dataconinsert con (length mts) boxed tycon
  
dataconinsert :: String -> Int -> Boxedness -> Con -> State ConMaps ()
dataconinsert con arity boxed tycon
  = do 
      (tmap, dmap) <- get
      put (tmap, Map.insert con DataConParam{darity = arity, 
                                             dtag = Map.size dmap, 
                                             dboxed = boxed, 
                                             dtycon = tycon} 
                                             dmap)
  
tyconinsert :: String -> Int -> Boxedness -> State ConMaps ()
tyconinsert con arity boxed
  = do 
      (tmap, dmap) <- get
      put (Map.insert con TyConParam{tarity = arity, 
                                     ttag = Map.size tmap, 
                                     tboxed = boxed}
                                     tmap, dmap)        
                   

-- update Atype -> Boxed/Unboxed 
class Update t where update :: TyConMap -> t -> t              

instance Update a => Update [a] where
  update = map . update

instance Update (Def a) where
  update m (DataDef tycon) = DataDef(update m tycon)
  update _ x  = x

instance Update TyCon where
  update m (TyCon boxed c tvs dcs) = 
    TyCon boxed c tvs (update m dcs)
 
instance Update DataCon where 
  update m (DataCon boxed c mts) = 
    DataCon boxed c (update m mts)

instance Update Monotype where
  update m (Mono ty) = if isboxed m ty == Boxed
                       then MBoxed (makeboxed m ty) 
                       else MUnboxed (makeunboxed m ty)
  update _ ty = ty -- type is already boxed/unboxed, shoud never happen

makeboxed :: TyConMap -> Atype -> Boxedtype
makeboxed _ (ATyVar x) = BTyVar x
makeboxed _ (AInt) = error "can't convert Int# to boxed"
makeboxed _ (ADouble) = error "can't convert Double# to boxed"
makeboxed m (AFun x y) = BFun (update m (Mono x)) (update m (Mono y))
makeboxed m (ATyCon c xs) = BTyCon c (map (makeboxed m) xs)

makeunboxed :: TyConMap -> Atype -> Unboxedtype
makeunboxed _ (ATyVar a) = error ("can't convert " ++ a ++ " to unboxed")
makeunboxed _ (AInt) = UInt
makeunboxed _ (ADouble) = UDouble
makeunboxed _ (AFun x y) = error ("can't convert function "
                         ++ show x ++ " -> " ++ show y ++ " to unboxed")
makeunboxed m (ATyCon c xs) = UTyCon c (map (makeboxed m) xs)
  
isboxed :: TyConMap -> Atype -> Boxedness  
isboxed _ (ATyVar _) = Boxed
isboxed _ (AInt) = Unboxed
isboxed _ (ADouble) = Unboxed
isboxed _ (AFun _ _) = Boxed
isboxed m (ATyCon c _) = let lookup = Map.lookup c m
                           in if (isNothing lookup) then 
                                error ("unknown type constructor " ++ c ++ " used")
                              else (\(TyConParam{tboxed=b}) -> b) (fromJust lookup)

-- helper functions

-- take a function on Objs and apply to Defs
onObjs :: ([Obj a] -> [Obj b]) -> [Def a] -> [Def b]
onObjs f ds = let (ts, os) = splitDefs ds
              in unsplitDefs (ts, f os)
                
splitDefs :: [Def a] -> ([TyCon], [Obj a])
splitDefs d = (getDatas d, getObjs d)

unsplitDefs :: ([TyCon], [Obj a]) -> [Def a]
unsplitDefs (ts,os) = map DataDef ts ++ map ObjDef os

getObjs :: [Def a] -> [Obj a]
getObjs = concatMap getObj
          where getObj (ObjDef o) = [o]
                getObj (DataDef _) = []

getDatas :: [Def a] -> [TyCon]
getDatas = concatMap getData
           where getData (ObjDef _) = []
                 getData (DataDef t) = [t]
