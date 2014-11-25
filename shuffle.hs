
interleave :: [a] -> [a] -> [a]
interleave xs     []     = xs
interleave []     ys     = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

outshuffle :: [a] -> [a]
outshuffle xs = interleave (take n xs) (drop n xs) where n = (length xs) `div` 2

inshuffle :: [a] -> [a]
inshuffle xs = interleave (drop n xs) (take n xs) where n = (length xs) `div` 2

oddshuffle :: [a] -> [a]
oddshuffle xs = interleave  (take n xs) (drop n xs) where n = 1 + (length xs) `div` 2

inn :: Int -> [Int]
inn n = inshuffle [1..n]

outn :: Int -> [Int]
outn n = outshuffle [1..n]

oddn :: Int -> [Int]
oddn n = oddshuffle [1..n]

-- apply shuffle function m times
apply :: ([Int] -> [Int]) -> Int -> Int -> [Int]
apply f n 0 = [1..n]
apply f n 1  = f [1..n]
apply f n m = f (apply f n (m-1))

{- 
check :: ([Int] -> [Int]) -> Int -> Int 

-}
