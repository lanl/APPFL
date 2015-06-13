module Sum0 where
f n = case n of
  0 -> 0
  _ -> n + f (n - 1)

data A a = A a
