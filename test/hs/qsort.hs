{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

one = I# 1#
two = I# 2#
three = I# 3#
four = I# 4#

list = [one, three, four, two]

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                        smaller = [a | a <- xs, a < x]
                        larger =  [b | b <- xs, b > x]

main = last $ qsort list == four 
