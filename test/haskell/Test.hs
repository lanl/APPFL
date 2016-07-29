{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude #-}
module Test where
import AppflPrelude
import APPFL.Prim

--import GHC.Exts

class MyEq a where
  eq :: a -> a -> Bool
  ne :: a -> a -> Bool

  ne a b = not (a `eq` b)
  eq a b = not (a `ne` b)
  {-# MINIMAL eq | ne #-}

data T a = T a deriving (Eq)

instance MyEq a =>  MyEq (T a) where
  eq (T a) (T b) = a `eq` b
  

ten = I# 10#
nine = I# 9#

nineteen = I# 19#


main = 1719283470192834023914810298374

tr = True == False
