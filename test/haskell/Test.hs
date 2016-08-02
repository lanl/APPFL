{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude #-}
module Test where
import qualified AppflPrelude as A
import APPFL.Prim

--import GHC.Exts

class MyEq a where
  eq :: a -> a -> A.Bool
  ne :: a -> a -> A.Bool

  ne a b = A.not (a `eq` b)
  eq a b = A.not (a `ne` b)
  {-# MINIMAL eq | ne #-}

data T a = T a deriving (A.Eq)

data Bool = True | False

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

main = A.False
