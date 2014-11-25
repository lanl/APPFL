-- Ch7 exercises from Graham Hutton's Programming in Haskell

import Data.Char 

fp :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fp f p xs = [f x | x <- xs, p x]

fp' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fp' f p xs = map f (filter p xs)

fp'' :: (a -> b) -> (a -> Bool) -> [a] -> [b]
fp'' f p = map f . filter p 

all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) = f x && all' f xs

all'' :: (a -> Bool) -> [a] -> Bool
all'' f = (foldr (&&) True) . map f

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) = f x || any' f xs

any'' :: (a -> Bool) -> [a] -> Bool
any'' f = (foldr (||) False) . map f

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) 
	| f x = x:takeWhile' f xs
        | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f axs@(x:xs) 
	| f x = dropWhile' f xs
        | otherwise = axs

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

map'' :: (a -> b) -> [a] -> [b]
map'' f = foldr ((:).f) []

map2 :: (a -> b) -> [a] -> [b]
map2 f = foldr (\x xs -> f x : xs) []

map3 :: (a -> b) -> [a] -> [b]
map3 f = foldr (\x -> (f x:) ) []

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 f [] = []
filter1 f (x:xs) 
	| f x = x: filter1 f xs
	| otherwise = filter1 f xs

filter2 :: (a -> Bool) -> [a] -> [a]
filter2 f [] = []
filter2 f (x:xs) = y ++ filter2 f xs 
		where y = if (f x) then [x] else [] 

helper :: (a -> Bool) -> a -> [a]
helper f x = if (f x) then [x] else []

filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f [] = []
filter3 f (x:xs) = helper f x ++ filter3 f xs 

helper2 :: (a -> Bool) -> a -> [a] -> [a]
helper2 f x xs = if (f x) then x:xs else xs

filter4 :: (a -> Bool) -> [a] -> [a]
filter4 f [] = []
filter4 f (x:xs) = helper2 f x (filter4 f xs) 

filter5 :: (a -> Bool) -> [a] -> [a]
filter5 f = foldr (\x xs -> if f x then x:xs else xs) []

filter6 :: (a -> Bool) -> [a] -> [a]
filter6 f = foldr (\x -> if f x then (x:) else id) []

