{-# LANGUAGE MagicHash, UnboxedTuples, NoImplicitPrelude, FlexibleContexts #-}
module Test where
import AppflPrelude
import APPFL.Prim

--import qualified Prelude as A


myPair = (I# 2#, C# 'a'#)

myOtherPair = (I# 2#, C# 'a'#)
snd (_,a) = a
fst (a,_) = a

main = myPair == myOtherPair
