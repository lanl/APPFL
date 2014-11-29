-- learn you a haskell ch3

addThree :: Int -> Int -> Int -> Int  
addThree x y z = x + y + z  

factorial :: Integer -> Integer  
factorial n = product [1..n] 

circumference :: Float -> Float  
circumference r = 2 * pi * r  

circumference' :: Double -> Double  
circumference' r = 2 * pi * r 

circumference'' :: Floating a => a -> a
circumference'' r = 2 * pi * r 

fi = fromIntegral (length [1,2,3,4]) + 3.2

