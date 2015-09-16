{-
Step 1: Define Type
sum :: [a] -> a
Step 2: Enumerate Cases
sum [] = 
sum (x:xs) = 
Step 3: Define The Simple Cases
sum [] = 0
Step 4: Define Other Cases
sum (x:xs) = x + (sum xs)
Step 5: Generalise and Simplify
-}

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (x:xs) = x + (sum1 xs)

{-
Step 1: Define Type
take :: Int -> [a] -> [a]
Step 2: Enumerate Cases
take 0 xs =
take (n+1) (x:xs) = 
Step 3: Define The Simple Cases
take 0 xs = []
Step 4: Define Other Cases
take (n+1) (x:xs) = (x) : (take n xs)
Step 5: Generalise and Simplify
-}

take1 :: Int -> [a] -> [a]
take1 0 _ = []
take1 _ [] = []
take1 n (x:xs) = x:(take (n-1) xs)

{-
Step 1: Define Type
last :: [a] -> a
Step 2: Enumerate Cases
last (x:xs)|(length xs)==0 =   
last (x:xs)|otherwise = 
Step 3: Define The Simple Cases
last (x:xs)|(length xs)==0 = x 
Step 4: Define Other Cases
last (x:xs)|otherwise = last xs
Step 5: Generalise and Simplify
-}

last1 :: [a] -> a
last1 (x:xs)|(length xs)==0 = x
            |otherwise = last1 xs


last2 :: [a] -> a
last2 [a] = a 
last2 (x:xs) = last2 xs 
