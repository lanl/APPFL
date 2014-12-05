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

-- one compare vs. possibly 2 
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
                             LT -> occurss'' m l
                             GT -> occurss'' m r

--Ex 3

data BTree = BLeaf Int | BNode BTree BTree deriving (Show)

bt = BNode(BNode (BLeaf 1) (BLeaf 2)) (BNode (BLeaf 3) (BLeaf 4))

ubt = BNode (BNode(BNode (BLeaf 1) (BLeaf 2)) (BNode (BLeaf 3) (BLeaf 4)))
        (BNode (BLeaf 5) (BLeaf 6))

flatten :: BTree -> [Int]
flatten (BLeaf n) = [n]
flatten (BNode l r) = flatten l ++ flatten r

nLeaf :: BTree -> Int
nLeaf = length . flatten

nLeaf' :: BTree -> Int
nLeaf' (BLeaf _) = 1
nLeaf' (BNode l r) = nLeaf' l + nLeaf' r 

balanced :: BTree -> Bool
balanced (BLeaf _) = True
balanced (BNode l r) = diff <= 1 && balanced l && balanced r
                       where diff = abs(nLeaf l - nLeaf r) 
                   
--Ex 4

spliter :: [a] -> ([a],[a])
spliter xs = (take n xs, drop n xs) where n = (length xs) `div` 2

balance :: [Int] -> BTree
balance [x] = BLeaf x 
balance xs = BNode (balance ls) (balance rs) 
             where (ls,rs) = spliter xs
                   
--Ex 5 see ch10-5.lhs

--Ex 6 bparser.lhs is start

--Ex 7 see ch10-7.lhs

--Ex 8

data MyMaybe a = MyNothing | MyJust a  

instance Monad MyMaybe where
    return x = MyJust x
   
    MyNothing >>= f = MyNothing
    (MyJust x) >>= f = f x

data MyList a = Empty | Cons a (MyList a) deriving (Show, Eq, Ord)  

mymap :: (a -> b) -> MyList a -> MyList b
mymap f Empty = Empty
mymap f (Cons x xs) = Cons (f x) (mymap f xs)

instance Monad MyList where
    return x = Cons x Empty

