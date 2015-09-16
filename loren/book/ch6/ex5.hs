merge :: Ord a => [a] -> [a] -> [a] 
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x < y then x : merge (xs) (y:ys)
              else y : merge (x:xs) (ys)

halve :: [a] -> ([a],[a])
halve xs = splitAt n xs
         where n = (length xs)`div`2

msort :: Ord a => [a] -> [a]
msort xs | (length xs)<=1  = xs
         | otherwise = merge (msort (a)) (msort(b))
               where (a,b) = halve xs

splitAtt :: Int -> [a] -> ([a],[a])
splitAtt 0 (x:xs) = ([], (x:xs))
splitAtt n [] = ([],[])
splitAtt n (x:xs) = (x:ys, zs)
                 where (ys,zs) = splitAtt (n-1) xs
