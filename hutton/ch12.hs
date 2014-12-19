-- Ch12 exercises from Graham Hutton's Programming in Haskell

--Ex 4

fibn :: Integer -> Integer
fibn 0 = 0
fibn 1 = 1
fibn n = fibn(n-1) + fibn(n-2)

fibs' :: [Integer]
fibs' = [fibn n | n <- [0..]]

fibs'' :: [Integer]
fibs'' = 0:1:zipWith (+) fibs'' (tail fibs'')

fibs :: [Integer] 
fibs = 0:1:[x + y | (x,y) <- zip fibs (tail fibs)]

--Ex 5

fib :: Integer
fib = fibs !! 10

fib1000 :: Integer
fib1000 = last $ takeWhile (<1000) fibs

fib1000' :: Integer
fib1000' = last $ fst $ span (<1000) fibs
 
