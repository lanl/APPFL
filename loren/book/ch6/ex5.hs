merge :: Ord a => [a] -> [a] -> [a] 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge (xs) (y:ys)
              else y : merge (x:xs) (ys)

halve :: [a] -> ([a],[a])
halve xs = (take n xs, drop n xs)
         where n = (length xs)`div`2

msort :: Ord a => [a] -> [a]
msort xs | (length xs)<=1  = xs
         | otherwise = merge (msort (a)) (msort(b))
               where (a,b) = halve xs

{-
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
-}
