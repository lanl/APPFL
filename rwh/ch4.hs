--ex 1

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing 
safeTail xs = Just (tail xs)

--ex 2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith _ [x] = [[x]]
splitWith f (x:xs) | not (f x) = [x]:y
                   | otherwise = [x:head y]  ++ tail y
                   where y = splitWith f xs

