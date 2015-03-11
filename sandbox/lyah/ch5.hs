-- learn you a haskell ch 5

maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum'' xs)  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

replicate'' :: (Num i, Ord i) => i -> a -> [a]  
replicate'' 0 x = [] 
replicate'' n x  = x:replicate'' (n-1) x  

replicatel :: (Num i, Enum i) => i -> a -> [a]
replicatel n x = [ x | _ <- [1..n]]

take' :: (Num i, Ord i) => i -> [a] -> [a]  
take' n _  | n <= 0   = []  
take' _ []     = []  
take' n (x:xs) = x : take' (n-1) xs  

reverse' :: [a] -> [a]  
reverse' [] = []  
reverse' (x:xs) = reverse' xs ++ [x]  

repeat' :: a -> [a]  
repeat' x = x:repeat' x  

zip' :: [a] -> [b] -> [(a,b)]  
zip' _ [] = []  
zip' [] _ = []  
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool  
elem' a [] = False  
elem' a (x:xs)  
    | a == x    = True  
    | otherwise = a `elem'` xs   

elem'' :: (Eq a) => a -> [a] -> Bool  
elem'' a [] = False  
elem'' a (x:xs) = if a == x then True else elem'' a xs  


elemc :: (Eq a) => a -> [a] -> Bool  
elemc a [] = False  
elemc a (x:xs) = case a == x of
			True -> True
			False -> elemc a xs
quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  

