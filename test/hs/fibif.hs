{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

one = I# 1#
two = I# 2#
seven = I# 7#
result = I# 21#

fib n = if n < two then one else fib (n - one) + fib (n - two)

main = result == (fib seven)
