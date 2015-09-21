data Tree = Leaf Int | Node Tree Int Tree

occurs :: Int -> Tree ->  Bool
occurs m (Leaf n) = m ==n
occurs m (Node l n r) = m==n || occurs m l || occurs m r

t :: Tree
t = Node(Node(Leaf 1) 3 (Leaf 4)) 5 (Node (Leaf 6) 7 (Leaf 9))

occurs1 :: Int -> Tree -> Bool
occurs1 m (Leaf n) = m==n
occurs1 m (Node l n r) | m==n = True
                       | m < n = occurs1 m l
                       | otherwise = occurs1 m r

data Ordering1 = LT1 | EQ1 | GT1

compare1 :: Ord a => a -> a -> Ordering1
compare1 x y | x < y = LT1
             | x == y = EQ1
             | x > y = GT1

occurs2 :: Int -> Tree -> Bool
occurs2 m (Leaf n) = m==n
occurs2 m (Node l n r) =  case compare1 m n of
                               EQ1 -> True
                               LT1 -> occurs2 m l
                               GT1 -> occurs2 m r
