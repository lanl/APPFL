map1 :: (a -> b) -> [a] -> [b]
map1 f xs = foldr (\x acc -> f x : acc)[] xs

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p xs = foldr (\x acc -> if p x then x : acc else acc)[] xs


