fibs :: [Integer] 
fibs = 0:1:[x+y|(x,y) <- zip (fibs) (tail fibs)]
