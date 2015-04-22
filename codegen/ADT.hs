
module ADT (
  ObjData(..),
  TopDecl(..),
  Constr(..),
  Constrs,
  TyVar,
  Polytype(..),
  Monotype(..),
  Boxedtype(..),
  Unboxedtype(..),
) where

import AST

{-
  Algebraic Datatypes:
    
  Ref : https://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-680004.2
    
  <topdecl> ::= data <polytype> = <constrs>    
    
  <polytype> ::= <con> <tyvar_1> ... <tyvar_k>  (k >= 0) they call this "simpletype"
     
  <constrs> ::= <constr_1> "|" ... "|" <constr_n>     (n >= 1)
    
  <constr> ::= <con>  <monotype_1> ... <monotype_k>     (arity con = k, k >= 0)


  Ref:  Unboxed Values as First-Class Citizens
  
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
             | ODData Polytype

data TopDecl = TopDecl Polytype Constrs
               deriving(Eq,Show)

data Constr = Constr Con [Monotype]
              deriving(Eq,Show)

type Constrs = [Constr]              

type TyVar = String

data Polytype = PPoly TyVar Polytype  -- curried forall
              | PMono Monotype
                deriving(Eq,Show)

data Monotype = MBoxed Boxedtype
              | MUnboxed Unboxedtype
                deriving(Eq,Show)

data Boxedtype = BVar TyVar
               | BFun Monotype Monotype
               | BTCon Con [Monotype]  -- SPJ paper says [Boxedtype]--typo?
                 deriving(Eq,Show)

data Unboxedtype = UInt
                 | UDouble
                 | UTCon Con [Monotype]  -- ditto
                   deriving(Eq,Show)


