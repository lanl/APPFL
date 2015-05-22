{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ADTnew (
  Def(..),
  TyCon(..),
  DataCon(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  TyConParam(..),
  TyConMap,
  DataConParam(..),
  DataConMap,
  ConMaps
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
  
  Polytype      \sigma ::=  \forall \alpha . \sigma 
                        |   \tau

  Monotype      \tau   ::=  \pi | \nu

  Boxed type    \pi    ::=  \alpha                   Type variable
                        |   \tau -> \tau             Function type
                        |   \Chi \pi_1 ... \pi_n   Parameterized boxed data type

  Unboxed type  \nu     ::=  Int#
                        |   Double#
                        |   Bool#
                        |   \Chi# \pi_1 ... \pi_n   Parameterized boxed data type

-}


data Def a = ObjDef (Obj a)
           | DataDef TyCon
             deriving(Eq,Show)

-- Boxed: data \Chi \alpha_1 .. \alpha_t =
-- c_1 \tau_11 .. \tau_1a_1 | ... | c_n \tau_n1 .. \tau_na_1  
-- Unboxed: data unboxed \Chi# \alpha_1 .. \alpha_t =
-- c_1# \tau_11 .. \tau_1a_1 | ... | c_n# \tau_n1 .. \tau_na_1   
data TyCon = TyCon Bool Con [TyVar] [DataCon]
             deriving(Eq,Show)
             
-- Boxed True: c_x \tau_x1 .. \tau_xa_1 
-- Boxed False: c_x# \tau_x1 .. \tau_xa_1  
data DataCon = DataCon Bool Con [Monotype]
               deriving(Eq,Show)

type TyVar = String

data Polytype = PPoly [TyVar] Monotype
              | PMono Monotype
                deriving(Eq,Show)
                
data Monotype = MVar TyVar
              | MFun Monotype Monotype
              | MCon Bool Con [Monotype]
                deriving(Eq,Show)

-- Type constr name to arity, tag, boxed/unboxed
data TyConParam = TyConParam {tarity :: Int, 
                              ttag :: Int, 
                              tboxed :: Bool,
                              tdatacons :: [Con]}
                  deriving(Eq,Show)

type TyConMap = Map.Map String TyConParam

-- Data constr name to (arity, tag, boxedness, type constr name)
data DataConParam = DataConParam {darity :: Int, 
                                  dtag :: Int, 
                                  dboxed :: Bool, 
                                  dtycon :: Con} 
                    deriving(Eq,Show)

type DataConMap = Map.Map String DataConParam

type ConMaps = (TyConMap, DataConMap)
         
 -- starting tycon map
tyconmap :: TyConMap
tyconmap = Map.insert "Bool#" (TyConParam 0 0 False ["False_h","True_h"])
         $ Map.insert "Int_h"  (TyConParam 0 1 False [])
         $ Map.insert "Bool"  (TyConParam 0 2 True ["B"]) -- data Bool = B Bool#
         $ Map.insert "Int"   (TyConParam 0 3 True ["I"]) -- data Int = I Int#
         $ Map.insert "List"  (TyConParam 1 4 True ["Nil","Cons"]) -- data List a = Nil | Cons a (List a)
           Map.empty

-- starting datacon map
-- these tags must match what is in stg_header.h
dataconmap :: DataConMap
dataconmap = Map.insert "False_h" (DataConParam 0 0 False "Bool#") 
           $ Map.insert "True_h"  (DataConParam 0 1 False "Bool#") 
           $ Map.insert "B"     (DataConParam 1 2 True "Bool") 
           $ Map.insert "I"     (DataConParam 1 3 True "Int") 
           $ Map.insert "Unit"  (DataConParam 0 4 True "Unit")
           $ Map.insert "Nil"   (DataConParam 0 5 True "List") 
           $ Map.insert "Cons"  (DataConParam 2 6 True "List")
             Map.empty
