-- From "Higher-Order Functions for Parsing" Graham Hutton

import Data.Char 

-- 2 Parsing Using Combinators

type Parser b a = [b] -> [(a,[b])]

-- 2.1 Primitive parsers

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

-- 2.2 Combinators

alt :: Parser b a -> Parser b a -> Parser b a
(p1 `alt` p2) inp = p1 inp ++ p2 inp

-- then
sequ :: Parser b a -> Parser b c -> Parser b (a,c)
(p1 `sequ` p2) inp = [((v1,v2),out2) | (v1,out1) <- p1 inp, (v2,out2) <- p2 out1]

-- 2.3 Manipulationg values

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

xthen :: Parser b a -> Parser b c -> Parser b c
p1 `xthen` p2 = (p1 `sequ` p2) `using` snd

thenx :: Parser b a -> Parser b c -> Parser b a
p1 `thenx` p2 = (p1 `sequ` p2) `using` fst

-- return
ret :: Parser b a -> c -> Parser b c
p `ret` v = p `using` (const v) 

-- 2.4 Example
{-
expn = ((term `sequ` literal '+' `xthen` term) `using` plus) `alt`
       ((term `sequ` literal '-' `xthen` term) `using` minus) `alt`
       term 

term = ((factor `sequ` literal '*' `xthen` factor) `using` times) 
--`alt`
--       ((factor `sequ` literal '/' `xthen` factor) `using` divide) `alt`
--       factor
-}
factor = (number `using` value) 
--`alt`
--         (literal '(' `xthen` expn `thenx` literal ')')


value :: String -> Int
value xs = read xs :: Int

plus :: Num a => (a, a) -> a
plus (x,y) = x + y

minus :: Num a => (a, a) -> a
minus (x,y) = x - y

times :: Num a => (a, a) -> a
times (x,y) = x * y

divide :: Integral a => (a, a) -> a
divide (x,y) = x `div` y

