{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

zero = I# 0#
one =  I# 1#
seven = I# 7#

rep n x = [x | _ <- [one..n]]

main = length $ rep seven one == seven

