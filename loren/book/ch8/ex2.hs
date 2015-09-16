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

concatPair :: [((Char,Char),[Char])] -> [([Char],[Char])]
concatPair [] = []
concatPair x = [(fst(fst(head x)):(snd(fst(head x)):[]) ,snd(head x))]

startComment :: Parser Char [Char]
startComment inp = concatPair ((literal '-' `thens` literal '-') inp)


notNewLine :: Char -> Bool
notNewLine x | x == '\n' = False
             | otherwise = True

concatPairs :: [(([Char],[Char]),[Char])] -> [([Char],[Char])]
concatPairs [] = []
concatPairs (x:xs) = (fst(fst(x)) ++ snd(fst(x)),snd(x)):(concatPairs xs)

comment :: Parser Char [Char]
comment x = concatPairs((startComment `thens` some (satisfy notNewLine)) x)


{-
notNewLine :: Char -> Bool
notNewLine x | x == '\n' = False
             | otherwise = True

concatPairs :: [(([Char],[Char]),[Char])] -> [([Char],[Char])]
concatPairs [] = []
concatPairs (x:xs) = (fst(fst(x)) ++ snd(fst(x)),snd(x)):(concatPairs xs)

comment :: Parser Char [Char]
comment x = concatPairs((startComment `thens` some (satisfy notNewLine)) x)
-}
