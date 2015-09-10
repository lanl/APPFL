--Conditional
safetail1 :: [a] -> [a]
safetail1 xs = if null xs  then [] else drop 1 xs

--Guarded Equations
safetail2 :: [a] -> [a]
safetail2 xs | length xs > 0  = drop 1 xs
             | otherwise = []
--Pattern Matching
safetail3 :: [a] -> [a]
safetail3 [] = []
safetail3 (_:xs) = xs

