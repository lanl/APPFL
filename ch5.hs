import Data.Char

sqs :: Int -> [Int]
sqs n = [x^2 | x <- [1..n]]

rep :: Int -> a -> [a]
rep n x = [x | _ <- [1..n]]

pyths :: Int -> [(Int,Int,Int)]
pyths n = [(x,y,z)| x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1..n], 2*x == sum (factors x)]

zipper =  [(x,y)|x <-[1,2,3], y <-[4,5,6]] 

zipper' = concat [[(x,y)| y<-[4,5,6]] | x<-[1,2,3]]

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v|(k',v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i|(x',i) <-zip xs [0..n], x == x']
                 where n = length xs - 1

positions' :: Eq a => a -> [a] -> [Int]
positions' x xs = find x (zip xs [0..]) 
                
sprod :: [Int] -> [Int] -> Int
sprod xs ys = sum[x*y | (x,y) <- zip xs ys]

let2int c = ord c - ord 'a'
int2let n = chr(ord 'a' + n)

let2int' c = ord c - ord 'A'
int2let' n = chr(ord 'A' + n)

shift n c | isLower c = int2let((let2int c + n) `mod` 26)
          | isUpper c = int2let'((let2int' c + n) `mod` 26)
          | otherwise = c

encode n xs = [shift n x | x <- xs]


