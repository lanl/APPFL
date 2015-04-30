{-# LANGUAGE FlexibleInstances #-}

module ADT (
  ObjData(..),
  Boxedness(..),
  TyCon(..),
  DataCon(..),
  Atype(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  Boxedtype(..),
  Unboxedtype(..),
  buildconmaps,
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
                        |   \Chi \tau_1 ... \tau_n   Parameterized boxed data type, SPJ says \pi

  Unboxed type \nu     ::=  Int#
                        |   Double#
                        |   \Chi# \tau_1 ... \tau_n   Parameterized unboxed data type, SPJ says \pi

-}


data ObjData a = ODObj (Obj a)
             | ODData TyCon
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

data Polytype = PPoly TyVar Polytype  -- curried forall
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
           | AInt Int
           | ADouble Double
           | AFun Atype Atype
           | ATyCon Con [Atype]
             deriving(Eq,Show)              

-- \pi
data Boxedtype = BTyVar TyVar
               | BFun Monotype Monotype
               | BTyCon Con [Boxedtype]  
                 deriving(Eq,Show)
-- \nu
data Unboxedtype = UInt Int
                 | UDouble Double
                 | UTyCon Con [Boxedtype] 
                   deriving(Eq,Show)

-- Type constr name to arity, tag, boxed/unboxed
type TyConMap = Map.Map String (Int, Int, Boxedness)
-- Data constr name to (arity, tag, boxedness, type constr name) 
type DataConMap = Map.Map String (Int, Int, Boxedness, Con)
type ConMaps = (TyConMap, DataConMap)
         
 -- starting tycon map
tyconmap :: TyConMap
tyconmap = Map.insert "Bool#" (0,0, Unboxed) 
               Map.empty

-- starting datacon map
dataconmap :: DataConMap
dataconmap = Map.insert "True" (0, 1, Unboxed, "Bool#") 
           $ Map.insert "False" (0, 0, Unboxed, "Bool#") 
             Map.empty

buildconmaps :: [ObjData a] -> ConMaps
buildconmaps objs = execState (build [] objs) 
                    (tyconmap, dataconmap)

class BuildConMaps t where build :: String -> t -> State ConMaps()
                                                    
instance BuildConMaps a => BuildConMaps [a] where
  build = mapM_ . build

instance BuildConMaps (ObjData a) where
  build _ (ODData tycon) = build [] tycon
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
      put (tmap, Map.insert con (arity, Map.size dmap, boxed, tycon) dmap)
  
tyconinsert :: String -> Int -> Boxedness -> State ConMaps ()
tyconinsert con arity boxed
  = do 
      (tmap, dmap) <- get
      put (Map.insert con (arity, Map.size tmap, boxed) tmap, dmap)        
                   

-- update Atype -> Boxed/Unboxed 
class Update t where update :: TyConMap -> t -> t              

instance Update a => Update [a] where
  update = map . update

instance Update (ObjData a) where
  update m (ODData tycon) = ODData(update m tycon)
  update _ x  = x

instance Update TyCon where
  update m (TyCon boxed c tvs dcs) = 
    TyCon boxed c tvs (update m dcs)
 
instance Update DataCon where 
  update m (DataCon boxed c mts) = 
    DataCon boxed c (update m mts)

instance Update Monotype where
  update m (Mono ty) = if (isboxed m ty == Boxed)
                       then MBoxed (makeboxed ty) 
                       else MUnboxed (makeunboxed ty)
  update _ _ = error "non atype"

makeboxed :: Atype -> Boxedtype
makeboxed (ATyVar x) = BTyVar x
makeboxed (AInt _) = error "can't convert to boxed"
makeboxed (ADouble _) = error "can't convert to boxed"
makeboxed (AFun x y) = error "no fun types yet"
makeboxed (ATyCon c xs) = BTyCon c (map makeboxed xs)

makeunboxed :: Atype -> Unboxedtype
makeunboxed (ATyVar _) = error "can't convert to unboxed"
makeunboxed (AInt x) = UInt x
makeunboxed (ADouble x) = UDouble x
makeunboxed (AFun _ _) = error "can't convert to unboxed"
makeunboxed (ATyCon c xs) = UTyCon c (map makeboxed xs)
  
isboxed :: TyConMap -> Atype -> Boxedness  
isboxed _ (ATyVar _) = Boxed
isboxed _ (AInt _) = Unboxed
isboxed _ (ADouble _) = Unboxed
isboxed _ (AFun _ _) = Boxed
isboxed m (ATyCon c _) = let lookup = Map.lookup c m
                           in if (isNothing lookup) then Boxed --default boxed for now
                              else (\(a,t,b) -> b) (fromJust lookup)

updatedata :: [ObjData a] -> ([ObjData a], ConMaps)
updatedata inp = (update (fst conmaps) inp, conmaps)
                 where conmaps = buildconmaps inp
