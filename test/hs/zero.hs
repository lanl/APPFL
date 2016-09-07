-- test z naming
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

zero = I# 0#
one = I# 1#

main = one == (one - zero)
