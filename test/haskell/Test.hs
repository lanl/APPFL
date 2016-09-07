{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude, FlexibleContexts #-}
module Test where
import AppflPrelude
import APPFL.Prim

--import qualified Prelude as A


myPair = (I# 2#, C# 'b'#)

myOtherPair = (I# 2#, C# 'b'#)


str = "a string"

(I# t#) `shr` (I# s#) = I# (t# `uncheckedIShiftRA#` s#)

two = (I# 2#)
twentyFive = (I# (negateInt# 25#))

main = twentyFive `shr` two
         
