{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

list = [True, False, True, False] ++ [True, False]

result = I# 4#
main = length (init (tail list)) == result

