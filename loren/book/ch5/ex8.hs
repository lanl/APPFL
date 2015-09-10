import Data.Char

let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let((let2int c + n)`mod` 26)
          | otherwise = c

encode n xs = [shift n x|x <- xs]

table= [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
        6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

percent n m = (fromIntegral n /fromIntegral m) * 100

freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr os es = sum [((o-e)^2)/e |(o,e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

lowers :: String -> Int
lowers xs = length[x|x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x==x']
               where n = length xs - 1

crack ys  = backToUpper zs  (encode (-factor) xs)
    where
        factor = head(positions(minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n<- [0..25]]
        table' = freqs xs
        xs = lowerList ys
        zs = getUpperPositions ys
        

lowerList :: [Char] -> [Char]
lowerList xs = [if isUpper x then toLower x else x | x <-xs]

getUpperPositions :: [Char] -> [Int]
getUpperPositions xs = [i | (x',i) <- zip xs [0..n], isUpper x']
               where n = length xs - 1

backToUpper :: [Int] -> [Char] -> [Char]
backToUpper xs ys = [if length (positions i xs) >= 1 then toUpper x' else x' | (x',i) <- zip ys [0..n]]
               where n = length ys - 1
 

{-
import Data.Char

let2int c = ord c - ord 'a'

int2let n = chr (ord 'a' + n)

shift n c | isLower c = int2let((let2int c + n)`mod` 26)
          | otherwise = c

encode n xs = [shift n x|x <- xs]

table= [8.2,1.5,2.8,4.3,12.7,2.2,2.0,6.1,7.0,0.2,0.8,4.0,2.4,
        6.7,7.5,1.9,0.1,6.0,6.3,9.1,2.8,1.0,2.4,0.2,2.0,0.1]

percent n m = (fromIntegral n /fromIntegral m) * 100

freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
    where n = lowers xs

chisqr os es = sum [((o-e)^2)/e |(o,e) <- zip os es]

rotate n xs = drop n xs ++ take n xs

crack xs = encode (-factor) xs
    where
        factor = head(positions(minimum chitab) chitab)
        chitab = [chisqr (rotate n table') table | n<- [0..25]]
        table' = freqs xs

lowers :: String -> Int
lowers xs = length[x|x <- xs, isLower x]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, x==x']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x',i) <- zip xs [0..n], x==x']
               where n = length xs - 1
-}
