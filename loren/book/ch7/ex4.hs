dec2int :: [Int] -> Int 
dec2int xs = foldl (\acc y -> 10*acc+y) 0 xs
