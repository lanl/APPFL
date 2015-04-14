{-# LANGUAGE MagicHash #-}

import GHC.Prim

data MyInt = MyInt Int# deriving(Show)

func :: Int# -> Int# -> Int#
func x y = x +# y +# x 

main = print (MyInt (func 111111# 222222#))
