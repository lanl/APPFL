data Nat = Zero | Succ Nat deriving (Show)

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add' :: Nat -> Nat -> Nat
add' m n = int2nat (nat2int m + nat2int n)

add'' Zero n = n
add'' (Succ m) n = Succ (add'' m n)

mult1 :: Nat -> Nat -> Nat
mult1 _ Zero = Zero
mult1 Zero _ = Zero
mult1 m (Succ n) =  add'' m (mult1 m n)

