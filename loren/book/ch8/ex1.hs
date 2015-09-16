import Data.Char
type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v inp = [(v, inp)]

failed :: Parser a b
failed inp = []

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy p []     = fail []
satisfy p (x:xs) | p x       = succeed x xs
                 | otherwise = fail xs

literal :: Char -> Parser Char Char
literal x = satisfy (==x)

cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs

alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) inp = p1 inp ++ p2 inp

thens :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `thens` p2) inp = [((v1, v2), out2) | (v1, out1) <- p1 inp, (v2,out2) <- p2 out1]  

using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) inp = [(f v, out) | (v,out) <- p inp]

many :: Parser a b -> Parser a [b]
many p = ((p `thens` many p) `using` cons)`alt` (succeed [])

some :: Parser a b -> Parser a [b]
some p = (p `thens` many p) `using` cons

number :: Parser Char [Char]
number = some (satisfy isDigit)

-- Start My Code
negNumber :: Parser Char [Char]
negNumber (x:xs) | x == '-' = (x:xs,""): (appendMinus(number xs))
                 | otherwise  = failed xs

appendMinus :: [([Char],[Char])] -> [([Char],[Char])]
appendMinus [] = []
appendMinus (x:xs) = (fst(x),'-':snd(x)):(appendMinus xs)

digit :: Parser Char Char
digit = satisfy isDigit

int :: Parser Char [Char]
int = number `alt` negNumber

optMinus :: Parser Char [Char]
optMinus (x:xs) | x == '-' = [("-",xs)]
           | otherwise = [("",(x:xs))]


concatPair :: [(([Char],[Char]),[Char])] -> [([Char],[Char])]
concatPair [] = []
concatPair (x:xs) = (fst(fst(x)) ++ snd(fst(x)),snd(x)):(concatPair xs)

opt :: Char -> Parser Char [Char]
opt c (x:xs) | x == c = [([c],xs)]
             | otherwise = [("", (x:xs))]

int2 :: Parser Char [Char]
int2 (x:xs) = concatPair (((opt '-') `thens` int) (x:xs))
--End My Code
