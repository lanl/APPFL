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


