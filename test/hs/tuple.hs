{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

zero = I# 0#
one = I# 1#
tup = (True, False)
list = [zero, one, zero, one]

swap (x,y) = (y,x)

pair x y = (x,y)

teq (x1,y1) (x2,y2) = x1 == x2 && y1 == y2

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
           where n = (length xs)`div`2


result1 = fst tup 

result2 = snd $ swap tup

result3 = let tup2 = pair zero one
          in teq (zero,one) tup2

result4 = let (a,b) = halve list
          in head a == head b

main = result1 && result2 && result3 && result4
