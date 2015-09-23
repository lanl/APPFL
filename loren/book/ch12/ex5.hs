fibs :: [Integer] 
fibs = 0:1:[x+y|(x,y) <- zip (fibs) (tail fibs)]

fib :: Int -> Integer
fib x = fibs !! x

-- fib 1000 calculates the 1000th fibonacci number
