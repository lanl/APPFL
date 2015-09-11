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

all2 :: (a -> Bool) -> [a] -> Bool
all2 f [] = True
all2 f (x:xs) = if f x then all2 f xs else False

any2 :: (a -> Bool) -> [a] -> Bool
any2 f [] = False
any2 f (x:xs) = if f x then True else any2 f xs

takeWhile2 :: (a -> Bool) -> [a] -> [a]
takeWhile2 f xs | null xs   = []
                | otherwise = if f (head xs)
                                  then (head xs):(takeWhile1 f (tail xs))
                                             else []

dropWhile2 :: (a -> Bool) -> [a] -> [a] 
dropWhile2 f xs | null xs   = []
                | otherwise = if f (head xs)
                              then dropWhile1 f (tail xs) else xs

takeWhile3 :: (a -> Bool) -> [a] -> [a]
takeWhile3 f xs = take n xs 
                      where n = findTake f 0 xs (length xs)

findTake :: (a -> Bool)-> Int -> [a] -> Int -> Int
findTake f n xs k | n==k = k
                | otherwise = if f (xs !! n) then findTake f (n+1) xs k else n 

dropWhile3 :: (a -> Bool) -> [a] -> [a]
dropWhile3 f xs = drop n xs 
                      where n = findTake f 0 xs (length xs)
