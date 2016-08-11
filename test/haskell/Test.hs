{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude, FlexibleContexts #-}
module Test where
import AppflPrelude
import APPFL.Prim

--import qualified Prelude as A


myPair = (I# 2#, C# 'b'#)

myOtherPair = (I# 2#, C# 'b'#)


str = "a string"

main = case length str of
  0 -> let x = x in x
  n -> str
         
