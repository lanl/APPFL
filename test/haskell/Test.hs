{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Test where


import AppflPrelude


data C = C Int Bool
data Strict = MkStrict !Int

c i b =
  let f = C i
      g = C 15
  in if b
     then
       case i of
         2 -> f True
         _ -> f False
     else g False

t = (2,3)
t1 = (,) 2
t# x = (# True, x #)

strictN n = MkStrict n 

main = 3

f = \x -> x

twoOrThree b = case b of
  True  -> 3#
  False -> 2#
