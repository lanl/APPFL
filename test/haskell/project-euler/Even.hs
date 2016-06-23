module Even where

data Parity = Even | Odd
data TorJ a b = Treasure a | Junk b

parity n | even n = Even
         | otherwise = Odd
  where even 0 = True
        even n = odd $ toZero n
        odd = not . even . toZero
        toZero n | n > 0 = n - 1
               | otherwise = n + 1

nValue n = case parity n of
  Even -> Treasure n
  Odd  -> Junk n
