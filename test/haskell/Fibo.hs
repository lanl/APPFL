module Fibo (fib) where

fib = head . flip drop fibs


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fib' n = aux 0 0 1
  where
    aux i f s | n == i = f
              | otherwise = aux (i + 1) s (f + s)
