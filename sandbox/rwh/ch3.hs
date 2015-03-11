import Data.Maybe
import Data.List
import Data.Function

--Ex 1
data List a = Cons a (List a)
            | Nil
              deriving (Show)

list = Cons 'a' (Cons 'b' (Cons 'c' Nil))


fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

toList :: List a -> [a]
toList Nil = []
toList (Cons x y) = x:toList y

--Ex 2
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)


data Tree' a = Node' (Maybe a) (Maybe(Tree' a)) (Maybe(Tree' a))

--Ex 1b/2b
length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs

--Ex 3b
mysum :: Num a => [a] -> a
mysum [] = 0
mysum (x:xs) = x + mysum xs

mean :: Fractional a => [a] -> a 
mean xs = (mysum xs)/n
          where n = fromIntegral(length' xs)

--Ex 4b
makePD :: [a] -> [a]
makePD xs = xs ++ reverse xs

--Ex 5b
isPD :: Eq a => [a] -> Bool
isPD [] = True
isPD [x] = True
isPD (x:xs) = if x == last xs then isPD(init xs) else False 

isPD' :: Eq a => [a] -> Bool
isPD' xs = xs == reverse xs

--Ex 6b
sortLen :: [[a]] -> [[a]]
sortLen = sortBy(\xs ys -> compare (length xs) (length ys))

sortLen' :: [[a]] -> [[a]]
sortLen' = sortBy(compare `on` length)

--Ex 7b
isperse :: a -> [[a]] -> [a]
isperse _ [] = []
isperse _ [x] = x
isperse c (x:xs) = x ++ [c] ++ isperse c xs

--Ex 8b
height :: Tree a -> Int
height Empty = 0
height (Node x l r) = 1 + max (height l) (height r)

--Ex 9b
data Turn = LeftT | RightT | Straight deriving (Show)

--Ex 10b
direction :: (Num a, Ord a) => (a,a) -> (a,a) -> (a,a) -> Turn
direction (x1,y1) (x2,y2) (x3,y3) | d == 0 = Straight
                                  | d > 0 = LeftT
                                  | d < 0 = RightT
                                  where d = (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)

--Ex 11b
directions :: (Num a, Ord a) => [(a,a)] -> [Turn]
directions (a:b:c:xs) = (direction a b c):(directions (b:c:xs))
directions _ = []

