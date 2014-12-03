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
int2nat'' = (!!) (iterate Succ Zero) 

--Ex 1

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

mult:: Nat -> Nat -> Nat
mult Zero n = Zero
mult (Succ m) n = add n (mult m n) 

--Ex 2

data Tree = Leaf Int | Node Tree Int Tree deriving (Show)

t = Node(Node(Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

-- for all trees
occurs :: Int -> Tree -> Bool
occurs m (Leaf n) = m == n
occurs m (Node l n r) = m == n || occurs m l || occurs m r

-- for search trees
occurss :: Int -> Tree -> Bool
occurss m (Leaf n) = m == n
occurss m (Node l n r) | m == n = True
                       | m < n = occurss m l
                       | otherwise = occurss m r
 
occurss' :: Int -> Tree -> Bool
occurss' m (Leaf n) = m == n
occurss' m (Node l n r) | o == EQ = True
                        | o == LT = occurss' m l
                        | o == GT = occurss' m r
                        where o = compare m n 

occurss'' :: Int -> Tree -> Bool
occurss'' m (Leaf n) = m == n
occurss'' m (Node l n r) = case compare m n of
                             EQ -> True
                             LT -> occurss' m l
                             GT -> occurss' m r


