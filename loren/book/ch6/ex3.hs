and :: [Bool]->Bool
and[] = True
and(x:xs) = if x then and xs else False

concat::[[a]]->[a]
concat[] = []
concat(xs: css) = xs++concat(css)

replicate ::Int -> a -> [a]
replicate 0 a = head xs
replicate(n+1) a = a:(replicate n a)

(!!) :: [a] -> Int -> a
(!!) xs 0 = head[xs]
(!!) xs (n+1) = (!!) (tail xs) n

elem :: Eq a => a -> [a] -> Bool
elem a [] = False
elem a xs = if head[xs]==a then True else element a (tail xs)


