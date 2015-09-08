all1 :: (a -> Bool) -> [a] -> Bool
all1 f xs = if length [x|x<- xs, f x]==(length xs) then True else False

any1 :: (a -> Bool) -> [a] -> Bool
any1 f xs = if length [x|x<- xs, f x] >=1 then True else False

takeWhile1 :: (a -> Bool) -> [a] -> [a]
takeWhile1 f xs | null xs = []
takeWhile1 f xs | otherwise = if f (head xs)
                                  then (head xs):(takeWhile1 f (tail xs))
                                             else []

dropWhile1 :: (a -> Bool) -> [a] -> [a] 
dropWhile1 f xs | null xs = []
dropWhile1 f xs | otherwise = if f (head xs)
                              then dropWhile1 f (tail xs) else xs

