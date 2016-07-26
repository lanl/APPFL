{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

zero = I# 0#
one = I# 1#
two = I# 2#
seven = I# 7#
result = I# 21#

fib (I# 0#) = one
fib (I# 1#) = one
fib n = fib (n - one) + fib (n - two)

main = result == (fib seven)
