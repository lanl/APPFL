module Parsing
( Parser
, Pos
, succeed
, failure
, literal
, alt
, then'
, using
, many
, some
, number
, word
, varname
, string
, conname
, comment
, char
, uchar
, xthen
, thenx
, any'
, satisfy
, offside
) where


-- From "Higher-Order Functions for Parsing" Graham Hutton 
-- Section 4 Miranda like parser

import Data.Char

-- 2 Parsing Using Combinators

type Parser b a = [b] -> [(a,[b])]

-- 2.1 Primitive parsers

succeed :: a -> Parser b a
succeed v inp = [(v,inp)]

failure :: Parser b a
failure inp = []

literal :: Eq b => b -> Parser (Pos b) b
literal x = satisfy (==x)

-- 2.2  Combinators

alt :: Parser b a -> Parser b a -> Parser b a
(p1 `alt` p2) inp = p1 inp ++ p2 inp

-- then
then' :: Parser b a -> Parser b c -> Parser b (a,c)
(p1 `then'` p2) inp = [((v1,v2),out2) | (v1,out1) <- p1 inp, (v2,out2) <- p2 out1]

-- 2.3 Manipulating values

using :: Parser b a -> (a -> c) -> Parser b c
(p `using` f) inp = [(f v,out) | (v,out) <- p inp]

cons :: (a, [a]) -> [a]
cons (x,xs) = x:xs

append :: ([a], [a]) -> [a]
append (xs,ys) = xs ++ ys

many :: Parser b a -> Parser b [a]
many p = ((p `then'` many p) `using` cons ) `alt` (succeed [])

some :: Parser b a -> Parser b [a]
some p = (p `then'` many p) `using` cons

one :: Parser b a -> Parser b [a]
one p = (p `then'` succeed []) `using` cons

number :: Parser (Pos Char) [Char]
number = some (satisfy isDigit)

word :: Parser (Pos Char) [Char]
word = some (satisfy isAlpha)

-- first char is lower rest alphaNum
varname :: Parser (Pos Char) [Char]
varname = (one (satisfy isLower) `then'` many (satisfy isAlphaNum)) 
          `using` append

-- first char is upper rest alphaNum
conname :: Parser (Pos Char) [Char]
conname = (one (satisfy isUpper) `then'` many (satisfy isAlphaNum)) 
          `using` append

-- start of comment really should be # in col 0 
comment :: Parser (Pos Char) [Char]
comment = (one (satisfy (=='!')) `thenx` many (satisfy (/='\n')) 
          `then'` one (satisfy (=='\n'))) `using` append

string :: Eq b => [b] -> Parser (Pos b) [b]
string [] = succeed []
string (x:xs) = (literal x `then'` string xs) `using` cons

char :: Parser (Pos Char) [Char]
char = one (satisfy isAlpha)

uchar :: Parser (Pos Char) [Char]
uchar = one (satisfy isUpper)

xthen :: Parser b a -> Parser b c -> Parser b c
p1 `xthen` p2 = (p1 `then'` p2) `using` snd

thenx :: Parser b a -> Parser b c -> Parser b a
p1 `thenx` p2 = (p1 `then'` p2) `using` fst

-- 3.1 Free-format input
any' :: (b -> Parser a c) -> [b] -> Parser a c
any' p = foldr (alt . p) failure

-- 3.3 The offside combinator

type Pos b = (b, (Int,Int))

satisfy :: (b->Bool) -> Parser (Pos b) b
satisfy p [] = failure []
satisfy p (x:xs)
        | p a = succeed a xs
        | otherwise = failure xs
          where (a,(r,c)) = x

offside :: Parser (Pos b) a -> Parser (Pos b) a
offside p inp = [(v,inpOFF) | (v,[]) <- p inpON]
        where
                inpON = takeWhile (onside (head inp)) inp
                inpOFF = drop (length inpON) inp
                onside (a,(r,c)) (b,(r',c')) = r'>=r && c'>=c

