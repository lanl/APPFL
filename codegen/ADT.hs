
module ADT (
  Polytype(..),
  Monotype(..),
  Boxedtype(..),
  Unboxedtype(..),
  Ctors
) where

{-
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

data Polytype = PPoly Char Polytype  -- curried forall
              | PMono Monotype
                deriving(Eq,Show)

data Monotype = MBoxed Boxedtype
              | MUnboxed Unboxedtype
                deriving(Eq,Show)

data Boxedtype = BVar Char
               | BFun Monotype Monotype
               | BTCon [Monotype] Ctors -- SPJ paper says [Boxedtype]--typo?
                 deriving(Eq,Show)

data Unboxedtype = UInt
                 | UDouble
                 | UTCon [Monotype] Ctors -- ditto
                   deriving(Eq,Show)

type Ctors = [(String, [Monotype])]

