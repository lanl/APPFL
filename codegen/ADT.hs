{-# LANGUAGE FlexibleInstances #-}

module ADT (
  ObjData(..),
  TyCon(..),
  BoxedTyCon(..),
  UnboxedTyCon(..),
  DataCon(..),
  BoxedDataCon(..),
  UnboxedDataCon(..),
  Simpletype(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  Boxedtype(..),
  Unboxedtype(..),
  addtymap,
) where

import AST

import Control.Monad.State
import qualified Data.Map as Map

-- TyCon name to arity, boxed/unboxed
type TyConMap = Map.Map String (Int, Bool)

{-
  Algebraic Datatypes:
    
  Ref:  Unboxed Values as First-Class Citizens
  
  data Def:
  data \Chi \alpha_1 .. \alpha_t =
  c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1
  
  Polytype      \sigma ::=  \forall \alpha . \sigma | \tau

  Monotype      \tau   ::=  \pi | \nu

  Boxed type    \pi    ::=  \alpha                   Type variable
                        |   \tau -> \tau             Function type
                        |   \Chi \tau_1 ... \tau_n   Parameterized boxed data type, SPJ says \pi

  Unboxed type \nu     ::=  Int#
                        |   Double#
                        |   \Chi# \tau_1 ... \tau_n   Parameterized unboxed data type, SPJ says \pi

-}


data ObjData = ODObj (Obj ())
             | ODData TyCon
               deriving(Eq,Show)
 
data TyCon = TyBoxed BoxedTyCon
           | TyUnboxed UnboxedTyCon
             deriving(Eq,Show)
           
data BoxedTyCon = BoxedTyCon Con [TyVar] [DataCon]
                  deriving(Eq,Show)
               
data UnboxedTyCon = UnboxedTyCon Con [TyVar] [DataCon]
                    deriving(Eq,Show)

data DataCon = DBoxed BoxedDataCon
             | DUnboxed UnboxedDataCon
               deriving(Eq,Show)

data BoxedDataCon = BoxedDataCon Con [Simpletype]
                    deriving(Eq,Show)
                    
data UnboxedDataCon = UnboxedDataCon Con [Simpletype]
                      deriving(Eq,Show)
                      
data Simpletype = STyVar TyVar
                | SInt Int
                | SDouble Double
                | SFun Simpletype Simpletype
                | STyCon Con [Simpletype]
                  deriving(Eq,Show)

type TyVar = String

data Polytype = PPoly TyVar Polytype  -- curried forall
              | PMono Monotype
                deriving(Eq,Show)

data Monotype = MBoxed Boxedtype
              | MUnboxed Unboxedtype
                deriving(Eq,Show)

data Boxedtype = BTyVar TyVar
               | BFun Monotype Monotype
               | BTyCon Con [Boxedtype]  
                 deriving(Eq,Show)

data Unboxedtype = UInt Int
                 | UDouble Double
                 | UTyCon Con [Boxedtype] 
                   deriving(Eq,Show)

addtymap :: [ObjData] -> ([ObjData], TyConMap)
addtymap inp = (inp, buildtymap inp)

buildtymap :: [ObjData] -> TyConMap
buildtymap objs = execState (build objs) Map.empty

class BuildConMap t where build :: t -> State TyConMap ()

instance BuildConMap [ObjData] where
  build (o:os) =  build o >> build os 
  build [] = return ()

instance BuildConMap ObjData where
  build (ODData tycon) = insert tycon
  build _ = return ()

insert :: TyCon -> State TyConMap ()
insert (TyBoxed (BoxedTyCon con tyvars _))
  = do
      cmap <- get
      put $ Map.insert con (length tyvars, True) cmap
insert (TyUnboxed (UnboxedTyCon con tyvars _)) 
 = do
      cmap <- get
      put $ Map.insert con (length tyvars, False) cmap

{-
pobjdata :: ObjData -> ObjData
pobjdata (ODData tycon) = ODData (ptycon tycon)
pobjdata x = x

ptycon :: TyCon -> TyCon
ptycon x = x
-}


{-           
monop :: Parser Token Monotype
monop = (boxedp `usingp` MBoxed) `altp` 
        (unboxedp `usingp` MUnboxed)

boxedp :: Parser Token Boxedtype
boxedp = (varp `usingp` BTyVar)  `altp` bconstrp --`altp` bfunp

bconstrp :: Parser Token Boxedtype
bconstrp = conp `thenp` manyp boxedp `usingp` uncurry BTyCon

bfunp :: Parser Token Boxedtype
bfunp = error "function type not supported"

unboxedp :: Parser Token Unboxedtype
unboxedp = (nump `usingp` UInt) `altp` ubconstrp

ubconstrp :: Parser Token Unboxedtype
ubconstrp = conp `thenp` manyp boxedp `usingp` uncurry UTyCon
-}

