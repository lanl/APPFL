import Data.Char (digitToInt) 
import Data.List (isInfixOf)

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

--ex 1b
asInt :: String -> Int
asInt "" = error "empty string"
asInt "-" = error "bad string -"
asInt xs | isInfixOf "." xs = error ". not allowed"
asInt ('-':xs) = -(asInt xs)
asInt xs = foldl step 0 xs
           where step acc x = acc * 10 + digitToInt x

--ex 2b
type ErrorMessage = String
asInt' :: String -> Either ErrorMessage Int 
asInt' "" = Left "empty string"
asInt' "-" = Left "bad string -"
asInt' xs | isInfixOf "." xs = Left ". not allowed"
asInt' ('-':xs) = neg (asInt' xs)
                  where neg (Right x) = Right (-x)
asInt' xs = Right (foldl step 0 xs)
           where step acc x = acc * 10 + digitToInt x

--ex3b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' xs = foldr (++) [] xs

--ex4b
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs) = if f x then x:takeWhile' f xs else []

takeWhile'' :: (a -> Bool) -> [a] -> [a]
takeWhile'' f xs = foldr step [] xs
                 where step acc ys = if f acc then acc:ys else [] 



