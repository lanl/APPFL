and1 :: [Bool]->Bool
and1[] = True
and1(x:xs) = if x then and xs else False

concat1::[[a]]->[a]
concat1[] = []
concat1(xs: css) = xs++concat1 css

replicated ::Int -> a -> [a]
replicated 0 a = []
replicated n a = a:(replicated (n-1) a)

(!!!) :: [a] -> Int -> a
(!!!) xs 0 = head xs
(!!!) xs n = (!!!) (tail xs) (n-1)

elem1 :: Eq a => a -> [a] -> Bool
elem1 a [] = False
elem1 a (x:xs) = if x==a then True else elem1 a xs

--New Concat Code
concat2 :: [[a]] -> [a]
concat2 [[]] = []
concat2 (xs:css) | null xs = concat2 css
concat2 ((x:xs):css) = x:(concat2 (xs:css))
--End New Concat Code

