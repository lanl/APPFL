-- Playing with card shuffle math

import Data.List 
import Data.Maybe

interleave :: [a] -> [a] -> [a]
interleave xs     []     = xs
interleave []     ys     = ys
interleave (x:xs) (y:ys) = x : y : interleave xs ys

outshuffle :: [a] -> [a]
outshuffle xs = interleave (take n xs) (drop n xs) where n = (length xs) `div` 2

inshuffle :: [a] -> [a]
inshuffle xs = interleave (drop n xs) (take n xs) where n = (length xs) `div` 2

oddshuffle :: [a] -> [a]
oddshuffle xs = interleave  (take n xs) (drop n xs) where n = 1 + (length xs) `div` 2

inn :: Int -> [Int]
inn n = inshuffle [1..n]

outn :: Int -> [Int]
outn n = outshuffle [1..n]

oddn :: Int -> [Int]
oddn n = oddshuffle [1..n]

-- apply shuffle function m times
apply :: ([Int] -> [Int]) -> Int -> Int -> [Int]
apply f n 0 = [1..n]
apply f n 1  = f [1..n]
apply f n m = f (apply f n (m-1))

-- find how many shuffles till cards are in order again
check :: ([Int] -> [Int]) -> Int -> Int 
check f n = head [m | m <-[1..n], apply f n m == [1..n]]

-- same using Map
check' :: ([Int] -> [Int]) -> Int -> Int 
check' f n =  1 + fromJust (findIndex (==[1..n]) (map (apply f n) [1..n]))

check'' :: ([Int] -> [Int]) -> Int -> Int 
check'' f n =  1 + fromJust (elemIndex [1..n] (map (apply f n) [1..n]))

-- find how many shuffles till cards are in order again
-- for all shuffle types for deck lengths 1..n
shuffler n = [ (m,s1 m, s2 m) | m<-[1..n]]
		where s1 m | even m = check inshuffle m
			   | otherwise = s3 m
		      s2 m | even m = check outshuffle m
			   | otherwise = s3 m
                      s3 m = check oddshuffle m

-- for outshuffle number of shuffles is smallest 2^k =1 (mod n-1)
outMultOrder 2 = 1  --special case
outMultOrder n | odd n = error "not even"
outMultOrder n = head [k | k<-[1..n], 2^k `mod` (n-1) == 1]

-- for inshuffle number of shuffles is smallest 2^k =1 (mod n+1)
inMultOrder n | odd n = error "not even"
inMultOrder n = head [k | k<-[1..n], 2^k `mod` (n+1) == 1]

-- for oddshuffle number of shuffles is smallest 2^k =1 (mod n)
oddMultOrder 1 = 1 --special case
oddMultOrder n | even n = error "not odd"
oddMultOrder n = head [k | k<-[1..n], 2^k `mod` n == 1]
	
shuffler' n = [ (m,s1 m, s2 m) | m<-[1..n]]
		where s1 m | even m = inMultOrder m
			   | otherwise = s3 m
		      s2 m | even m = outMultOrder m
			   | otherwise = s3 m
                      s3 m = oddMultOrder m


