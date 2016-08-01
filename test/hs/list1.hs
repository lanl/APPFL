{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

list = [True, False, True, False] ++ [True, False]

result1 = length (init (tail list)) == I# 4#

isTrue (True) = True
isTrue (False) = False

result2 = length (filter isTrue list) == I# 3#

result3 = length (filter isTrue (map not list)) == I# 3#

main = result1 && result2 && result3
