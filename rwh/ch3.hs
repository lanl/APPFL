import Data.Maybe

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

