module Letrec where

f :: Int -> [Int]
f n = let r = 0:s
          s = 1:r
      in take n (r++s) 
