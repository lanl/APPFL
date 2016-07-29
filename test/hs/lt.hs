{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

three = I# 3#
four = I# 4#

main = three < four
