data Tree a = Leaf | Node (Tree a ) a (Tree a) deriving (Show)

repeat' :: a -> Tree a
repeat' x = Node (repeat' x) x (repeat' x)

take' :: Int -> Tree a -> Tree a
take' 0 _ = Leaf
take' n Leaf = Leaf 
take' n (Node x y z) = Node (take' (n-1) x) y (take' (n-1) z)

replicate' :: Int -> a -> Tree a
replicate' n = take' n . repeat' 
