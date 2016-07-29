{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

zero = I# 0#
one = I# 1#
seven = I# 7#
result = I# 5040#

fac (I# 0#) = one
fac n = n * fac (n - one)

main = result == (fac seven)

