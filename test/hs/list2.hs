{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

zero = I# 0#
one =  I# 1#
seven = I# 7#

list = [zero, one, zero, one] ++ [zero, one]

last2 s = s!!(length s - one)

last3 s = head(reverse s)

init2 s = reverse(tail(reverse s))

init3 s = take (length s - one) s

result1 = last2 list == last3 list

result2 = head $ init2 list == head $ init3 list

result3 = length $ replicate seven one == seven

main = result1 && result2 && result3 
