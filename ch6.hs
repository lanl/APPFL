
import Prelude hiding((*),(^),(!!))

mult :: Int -> Int -> Int
m `mult` 0  = 0 
m `mult` n  = m + (m `mult` (n-1))

(*) :: Int -> Int -> Int
m * 0  = 0 
m * n  = m + (m * (n-1))

(^) :: Int -> Int -> Int
m ^ 0  = 1 
m ^ n  = m * (m ^ (n-1))

fact :: Int -> Int
fact 0 = 1
fact n = n*fact(n-1)
 
and' :: [Bool] -> Bool
and' [] = True 
and' (x:xs) = x && (and' xs)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n xs = xs:replicate' (n-1) xs 

replicate'' :: Int -> a -> [a]
replicate'' n xs  
	| n == 0 = []
	| otherwise = xs:replicate'' (n-1) xs

bangbang :: [a] -> Int -> a
(x:xs) `bangbang` 0 = x
(x:xs) `bangbang` n = xs `bangbang` (n-1)

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
(x:xs) !! n = xs !! (n-1)

elem' :: Eq a => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys) = if x == y then True else elem' x ys

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] y = y 
merge x [] = x
merge axs@(x:xs) ays@(y:ys) 
      | x <= y = x:merge xs ays
      | otherwise = y:merge ys axs

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' n [] = []
take' n (x:xs) = x:take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop' (n-1) xs

halve :: [a] -> ([a],[a])
halve xs = (take' n xs, drop' n xs) where n = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort xs 
      | length xs <= 1 = xs
      | otherwise = merge (msort ys) (msort zs)  where (ys,zs) = halve xs

msort' :: Ord a => [a] -> [a]
msort' [] = [] 
msort' [x] = [x]
msort' axs@(x:xs) = merge (msort' ys) (msort' zs) where (ys,zs) = halve axs 

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

