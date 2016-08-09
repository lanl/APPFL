{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

list = [True, False, True, False] ++ [True, False]

result1 = length $ (init . tail) list == I# 4#

result2 = length $ filter (==True) list == I# 3#

result3 = length $ filter (/=False) (map not list) == I# 3#

result4 = not $ head (reverse list)

main = result1 && result2 && result3 && result4
