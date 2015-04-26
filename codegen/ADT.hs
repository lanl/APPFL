{-# LANGUAGE FlexibleInstances #-}

module ADT (
  ObjData(..),
  TyCon(..),
  BoxedTyCon(..),
  UnboxedTyCon(..),
  DataCon(..),
  BoxedDataCon(..),
  UnboxedDataCon(..),
  Atype(..),
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
  
  data Def (pg 6):
  data \Chi \alpha_1 .. \alpha_t =
  c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1
  
  \Chi -- type constructor
  \c_x -- data constructor
  
  and unboxed version on pg 26
  data \Chi# \alpha_1 .. \alpha_t =
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


data ObjData = ODObj (Obj ())
             | ODData TyCon
               deriving(Eq,Show)
 
data TyCon = TyBoxed BoxedTyCon
           | TyUnboxed UnboxedTyCon
             deriving(Eq,Show)
             
-- data \Chi \alpha_1 .. \alpha_t =
-- c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1           
data BoxedTyCon = BoxedTyCon Con [TyVar] [DataCon]
                  deriving(Eq,Show)
               
-- data \Chi# \alpha_1 .. \alpha_t =
-- c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1   
data UnboxedTyCon = UnboxedTyCon Con [TyVar] [DataCon]
                    deriving(Eq,Show)

data DataCon = DBoxed BoxedDataCon
             | DUnboxed UnboxedDataCon
               deriving(Eq,Show)
-- c_x \tau_x1 .. \tau_xa_1 
data BoxedDataCon = BoxedDataCon Con [Monotype]
                    deriving(Eq,Show)
          
-- c_x# \tau_x1 .. \tau_xa_1                     
data UnboxedDataCon = UnboxedDataCon Con [Monotype]
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
                   
-- update Atype -> Boxed/Unboxed 

class Update t where update :: t -> TyConMap -> t              
                  
instance Update [ObjData] where
  update (o:os) m =  (update o m) : (update os m)
  update [] _ = []

instance Update ObjData where
  update (ODData tycon) m = ODData(update tycon m)
  update x _ = x

instance Update TyCon where
  update (TyBoxed (BoxedTyCon c tvs dcs)) m = 
    TyBoxed (BoxedTyCon c tvs (update dcs m))
  update (TyUnboxed (UnboxedTyCon c tvs dcs)) m =
    TyUnboxed (UnboxedTyCon c tvs (update dcs m))

instance Update [DataCon] where
  update (dc:dcs) m =  (update dc m) : (update dcs m)
  update [] _ = []   

instance Update DataCon where 
  update (DBoxed (BoxedDataCon c mts)) m = 
    DBoxed (BoxedDataCon c (update mts m))
  update (DUnboxed (UnboxedDataCon c mts)) m = 
    DUnboxed (UnboxedDataCon c (update mts m))

instance Update [Monotype] where
  update (mt:mts) m =  (update mt m) : (update mts m)
  update [] _ = []

instance Update Monotype where
  update (Mono ty) m = error "wip"
  update _ _ = error "non atype"
  
  
addtymap :: [ObjData] -> ([ObjData], TyConMap)
addtymap inp = (inp, buildtymap inp)

buildtymap :: [ObjData] -> TyConMap
buildtymap objs = execState (build objs) Map.empty

class BuildConMap t where build :: t -> State TyConMap ()

instance BuildConMap [ObjData] where
  build (o:os) =  build o >> build os 
  build [] = return ()

instance BuildConMap ObjData where
  build (ODData tycon) = build tycon
  build _ = return ()

instance BuildConMap TyCon where
  build (TyBoxed (BoxedTyCon con tyvars _)) 
    = insert con (length tyvars) True
  build (TyUnboxed (UnboxedTyCon con tyvars _)) 
    = insert con (length tyvars) False
  
insert :: String -> Int -> Bool -> State TyConMap ()
insert con arity boxed
  = do
      cmap <- get
      put $ Map.insert con (arity, boxed) cmap



