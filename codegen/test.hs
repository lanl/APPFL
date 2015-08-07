len :: Num b => [a] -> b
len [] = 0
len (x:xs) = 1 + len xs
