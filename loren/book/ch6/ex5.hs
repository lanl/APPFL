merge :: Ord a => [a] -> [a] -> [a] 
merge [] xs = xs
merge xs [] = xs
merge xs ys = if head xs < head ys then head xs : merge (tail xs) ys
              else head ys : merge xs (tail ys)

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
         where n = (length xs)`div`2

msort :: Ord a => [a] -> [a]
msort xs | (length xs)<=1  = xs
msort xs | otherwise = merge (msort (fst (a)))
                          (msort(snd (a)))
               where a = halve xs


