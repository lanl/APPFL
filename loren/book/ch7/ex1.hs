{-
filter p (map f xs)
-}

newList :: (a -> b) -> (b -> Bool) -> [a] -> [b]
newList f p xs = filter p (map f xs)

triple x = 3*x
