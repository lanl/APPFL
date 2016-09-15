{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude

q :: [Int]
q = [5,9,1,8,2,7,3,6,4]

r :: [Int]
r = [1,2,3,4,5,6,7,8,9]

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where 
                        smaller = [a | a <- xs, a <= x]
                        larger =  [b | b <- xs, b > x]

main = r == qsort q
