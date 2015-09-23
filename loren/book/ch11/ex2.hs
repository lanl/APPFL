import Data.List

--first try
{-
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] ys = True
isChoice xs [] = False
isChoice (x:xs) ys | (length ys) /= (length (ys \\(x:[]))) = isChoice xs (ys \\ (x:[]))  
                   | otherwise = False 
-}
--second try
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] ys = True
isChoice xs [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeElem x ys) 

removeElem :: Eq a => a -> [a] -> [a]
removeElem x [] = [] 
removeElem x (y:ys) | x == y = ys
                    | otherwise = y:(removeElem x ys)
