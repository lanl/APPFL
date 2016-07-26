{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

three = I# 3#
four = I# 4#
seven = I# 7#

main = seven == (three + four)
