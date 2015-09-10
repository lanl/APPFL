merge :: Ord a => [a] -> [a] -> [a] 
merge [] xs = xs
merge xs [] = xs
merge xs ys = if head xs < head ys then head xs : merge (tail xs) ys
              else head ys : merge xs (tail ys)
