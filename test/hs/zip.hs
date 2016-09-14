module Zip where

import AppflPrelude
import APPFL.Prim

zip' :: [a] -> [b] -> [(a,b)]
zip' []     []     = []
zip' []     _bs    = []
zip' _as    []     = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

list = [True, False, True, False]
list2 = [True, False]

main = head (zip' list list2) == (True, True)
