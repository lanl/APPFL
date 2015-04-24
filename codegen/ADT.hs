
module ADT (
  ObjData(..),
  TyCon(..),
  DataCon(..),
  TyVar,
  Polytype(..),
  Monotype(..),
  Boxedtype(..),
  Unboxedtype(..),
) where

import AST

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

data TyCon = TyCon Con [TyVar] [DataCon]
               deriving(Eq,Show)

data DataCon = DataCon Con [Monotype]
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

data Unboxedtype = UInt
                 | UDouble
                 | UTyCon Con [Boxedtype] 
                   deriving(Eq,Show)


