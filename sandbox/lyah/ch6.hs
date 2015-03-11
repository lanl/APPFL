-- learn you a haskell ch 6

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)  
flip' f = g  
    where g x y = f y x  

flip'' :: (a -> b -> c) -> b -> a -> c  
flip'' f y x = f x y  

quicksort :: (Ord a) => [a] -> [a]    
quicksort [] = []    
quicksort (x:xs) =     
    let smallerSorted = quicksort (filter (<=x) xs)  
        biggerSorted = quicksort (filter (>x) xs)   
    in  smallerSorted ++ [x] ++ biggerSorted 

chain :: (Integral a) => a -> [a]  
chain 1 = [1]  
chain n  
    | even n =  n:chain (n `div` 2)  
    | odd n  =  n:chain (n*3 + 1)  

numLongChains :: Int  
numLongChains = length (filter isLong (map chain [1..100]))  
    where isLong xs = length xs > 15 

-- chains longer than starting value
longChains :: Int -> [[Int]]
longChains n = filter isLong (map chain [1..n]) 
	where isLong all@(x:xs) = length all > x

numLongChains' :: Int  
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))  

longChains' :: Int -> [[Int]]
longChains' n = filter (\(x:xs) -> length (x:xs) > x) (map chain [1..n]) 

longChains'' :: Int -> [[Int]]
longChains'' n = filter (\all@(x:xs) -> length all > x) (map chain [1..n]) 

flipl :: (a -> b -> c) -> b -> a -> c  
flipl f = \x y -> f y x 

sum' :: (Num a) => [a] -> a  
sum' xs = foldl (\acc x -> acc + x) 0 xs 

sum'' :: (Num a) => [a] -> a  
sum'' = foldl (+) 0 

elem' :: (Eq a) => a -> [a] -> Bool  
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys  

elem'' :: (Eq a) => a -> [a] -> Bool  
elem'' y = foldl (\acc x -> if x == y then True else acc) False  

elem''' :: (Eq a) => a -> [a] -> Bool  
elem''' y = foldl (\acc x -> case () of _ | x == y -> True | otherwise -> acc) False  

mapr :: (a -> b) -> [a] -> [b]  
mapr f xs = foldr (\x acc -> f x : acc) [] xs  

mapr' :: (a -> b) -> [a] -> [b]  
mapr' f = foldr (\x acc -> f x : acc) [] 

mapl :: (a -> b) -> [a] -> [b]  
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs

mapl' :: (a -> b) -> [a] -> [b]  
mapl' f = foldl (\acc x -> acc ++ [f x]) [] 

maximum' :: (Ord a) => [a] -> a  
maximum' = foldr1 (\x acc -> if x > acc then x else acc)  
 
reverse' :: [a] -> [a]  
reverse' = foldl (\acc x -> x : acc) []  

reverse'' :: [a] -> [a]  
reverse'' = foldl (flip (:)) []
  
product' :: (Num a) => [a] -> a  
product' = foldr1 (*)  
  
filter' :: (a -> Bool) -> [a] -> [a]  
filter' p = foldr (\x acc -> if p x then x : acc else acc) []  
  
filter'' :: (a -> Bool) -> [a] -> [a]  
filter'' p = foldr (\x -> if p x then (x:) else id) []  

head' :: [a] -> a  
head' = foldr1 (\x _ -> x)  
  
last' :: [a] -> a  
last' = foldl1 (\_ x -> x)

sqrtSums :: Int  
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer  
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..]))) 

oddSquareSum' :: Integer  
oddSquareSum' = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..] 

oddSquareSum'' :: Integer  
oddSquareSum'' =   
    let oddSquares = filter odd $ map (^2) [1..]  
        belowLimit = takeWhile (<10000) oddSquares  
    in  sum belowLimit 

