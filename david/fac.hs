module Fac where
fac n = case n of
  0 -> 1
  _ -> n * fac (n-1)
