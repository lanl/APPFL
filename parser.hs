import Data.Char 

type Parser b a = [b] -> [(a,[b])]

--succeed :: a -> b -> [(a,b)]
--succeed :: a -> [b] -> [(a,[b])]
succeed :: a -> Parser b a
succeed v inp = [(v,inp)]

--failure :: [b] -> [(a,[b])]
failure :: Parser b a
failure inp = []

satisfy :: (b->Bool) -> Parser b b
satisfy p [] = failure []
satisfy p (x:xs) 
	| p x = succeed x xs
	| otherwise = failure xs

literal :: Eq b => b -> Parser b b
literal x = satisfy (==x)

alt :: Parser b a -> Parser b a -> Parser b a
(p1 `alt` p2) inp = p1 inp ++ p2 inp

sequ :: Parser b a -> Parser b c -> Parser b (a,c)
(p1 `sequ` p2) inp = [((v1,v2),out2) | (v1,out1) <- p1 inp, (v2,out2) <- p2 out1]

using :: Parser b a -> (a -> c) -> Parser b c
(p `using` f) inp = [(f v,out) | (v,out) <- p inp]

cons :: (a, [a]) -> [a]
cons (x,xs) = x:xs

many :: Parser b a -> Parser b [a]
many p = ((p `sequ` many p) `using` cons ) `alt` (succeed [])

some :: Parser b a -> Parser b [a]
some p = (p `sequ` many p) `using` cons

number :: Parser Char [Char]
number = some (satisfy isDigit)

word :: Parser Char [Char]
word = some (satisfy isAlpha)

string :: Eq b => [b] -> Parser b [b]
string [] = succeed []
string (x:xs) = (literal x `sequ` string xs) `using` cons

