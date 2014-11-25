q = [1,2,3,4,5,6]

halve xs = (take l xs, drop l xs)
           where l = (length xs) `div` 2

stail1 :: [a] -> [a]
stail1 xs = if null xs then [] else tail xs

stail1' :: [a] -> [a]
stail1' xs = if length xs == 0 then [] else tail xs

stail2 :: [a] -> [a]
stail2 xs | null xs = []
          | otherwise = tail xs

stail2' :: [a] -> [a]
stail2' xs | length xs == 0 = []
          | otherwise = tail xs

stail2'' :: Eq a => [a] -> [a]
stail2'' xs | xs == [] = []
          | otherwise = tail xs

stail3 :: [a] -> [a]
stail3 [] = []
stail3 xs = tail xs 

d1 :: Bool -> Bool -> Bool
True `d1` True = True 
True `d1` False = True
False `d1` True = True
False `d1` False = False

d2 :: Bool -> Bool -> Bool
False `d2` False = False
_ `d2` _ = True

d3 :: Bool -> Bool -> Bool
True `d3` _ = True
False `d3` a = a

a `d4` b | a == True = True
         | otherwise = b

a `d5` b | a == False = b
         | otherwise = True

a `c1` b = if a == True then if b == True then True else False else False

a `c2` b = if a == True then b else False

mult1 x y z = x*y*z

mult2 = \x -> (\y -> (\z -> x*y*z))

