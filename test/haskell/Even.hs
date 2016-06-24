module Even where

import Prelude

data Parity = Even | Odd
data TorJ a b = Treasure a | Junk b deriving (Show)

parity :: Int -> Parity
parity n | even n = Even
         | otherwise = Odd
  where even 0 = True
        even n = odd . toZero $ n
        odd 0  = False
        odd n  = even . toZero $ n
        toZero n | n > 0     = n - 1
                 | otherwise = n + 1

nValue n = case parity n of
  Even -> Treasure n
  Odd  -> Junk n

someList = [1 .. 6]

main = nValue 3
