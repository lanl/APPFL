-- Ch10 exercises from Graham Hutton's Programming in Haskell

-- 10.3

data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

int2nat' :: Int -> Nat
int2nat' n = (iterate Succ Zero) !! n

int2nat'' :: Int -> Nat
int2nat'' = (!!) (iterates Succ Zero) 

