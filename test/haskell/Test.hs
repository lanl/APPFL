{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude, FlexibleContexts #-}
module Test where
import qualified AppflPrelude as A
import APPFL.Prim

--import qualified Prelude as A

class MyEq a where
  eq :: a -> a -> A.Bool
  ne :: a -> a -> A.Bool

  ne a b = A.not (a `eq` b)
  eq a b = A.not (a `ne` b)
  {-# MINIMAL eq | ne #-}

class Foo a b where
  eqv :: a -> b -> A.Bool

instance {-# OVERLAPPABLE #-} Foo a b => Foo b a where
  eqv a b = eqv b a

instance A.Eq a => Foo a a where
  eqv = (A.==)

instance {-# OVERLAPPING #-} Foo A.Bool Bool where
  eqv a True = a
  eqv a False = A.not a

data T a = T a deriving (A.Eq)

data Bool = True | False

instance A.Eq Bool where
  (==) = eq

instance MyEq a =>  MyEq (T a) where
  eq (T a) (T b) = a `eq` b

instance MyEq A.Bool where
  A.True `eq` b  = b
  A.False `eq` b = A.not b

instance MyEq Bool where
  True `eq` b  = toA b
  False `eq` b = A.not (toA b)

toA True = A.True
toA False = A.False

eqtest = T A.True `eq` T A.False

ghceqtest = A.True A./= A.False

instance Foo Bool A.Int where
  True `eqv` (A.I# 0#) = A.True
  False `eqv` (A.I# 0#) = A.True
  _ `eqv` _ = A.False



zero = (A.I# 0#)

main = zero `eqv` False
