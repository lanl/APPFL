{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where
import AppflPrelude
import APPFL.Prim

realMain = True && False && True

ten = I# 10#
nine = I# 9#

nineteen = I# 19#

main = nineteen /= (ten + nine)
