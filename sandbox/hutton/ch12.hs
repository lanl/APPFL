-- Ch12 exercises from Graham Hutton's Programming in Haskell

--Ex 4

fibn :: Integer -> Integer
fibn 0 = 0
fibn 1 = 1
fibn n = fibn(n-1) + fibn(n-2)

fibs' :: [Integer]
fibs' = [fibn n | n <- [0..]]

fibs'' :: [Integer]
fibs'' = 0:1:zipWith (+) fibs'' (tail fibs'')

fibs :: [Integer] 
fibs = 0:1:[x + y | (x,y) <- zip fibs (tail fibs)]

--Ex 5

fib :: Integer
fib = fibs !! 10

fib1000 :: Integer
fib1000 = last $ takeWhile (<1000) fibs

fib1000' :: Integer
fib1000' = last $ fst $ span (<1000) fibs

--Ex 6

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving(Show)

repeatT :: a -> Tree a
repeatT x = Node (repeatT x) x (repeatT x)

takeT :: Int -> Tree a -> Tree a
takeT 0 _ = Leaf
takeT 1 (Node l x r) = Node Leaf x Leaf
takeT 2 (Node l x r) = Node (takeT 1 l) x Leaf
takeT 3 (Node l x r) = Node (takeT 1 l) x (takeT 1 r)
takeT n (Node l x r) = Node (takeT half l) x (takeT (n-1-half) r)
                       where half =  n `div` 2                       

replicateT :: Int -> a -> Tree a 
replicateT n x = takeT n (repeatT x)
 
