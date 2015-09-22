data Tree = Leaf Int | Node Tree Tree deriving (Show)
{-
instance Show Tree where 
 show (Leaf i) = "Thorn " ++ show i
 show (Node t1 t2) = "Branch " ++ show t1 ++ " " ++ show t2
-}

halve :: [a] -> ([a],[a])
halve xs = splitAtt n xs
           where n = (length xs)`div`2

splitAtt :: Int -> [a] -> ([a],[a])
splitAtt 0 (x:xs) = ([], (x:xs))
splitAtt n [] = ([],[])
splitAtt n (x:xs) = (x:ys, zs)
                    where (ys,zs) = splitAtt (n-1) xs

balance :: [Int] -> Tree
balance xs | length xs == 1 = (Leaf (head xs))
           | otherwise = (Node (balance w) (balance z))
                          where (w,z) = halve xs

balance2 :: [Int] -> Tree
balance2 [] = error "You lose"
balance2 [x] = (Leaf x)
balance2  xs  = (Node (balance2 w) (balance2 z))
               where (w,z) = halve xs

countLeaves :: Tree -> Int
countLeaves (Leaf _) = 1
countLeaves (Node x y) = countLeaves x + countLeaves y
