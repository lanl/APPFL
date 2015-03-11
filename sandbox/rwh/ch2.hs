
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last xs

lastButOne :: [a] -> a
lastButOne = last . init 

lastButOne' :: [a] -> a
lastButOne' = head . tail . reverse 


