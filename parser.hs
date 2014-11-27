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

-- 2.3 Manipulating values

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


data Expr = Num Int | Add Expr Expr | Sub Expr Expr 
		    | Mul Expr Expr | Div Expr Expr 
		    deriving (Show)

expn :: Parser Char Expr
expn = ((term `thenx` literal '+' `sequ` term) `using` plus) `alt`
       ((term `thenx` literal '-' `sequ` term) `using` minus) `alt`
       term 

term :: Parser Char Expr
term = ((factor `thenx` literal '*' `sequ` factor) `using` times) `alt`
       ((factor `thenx` literal '/' `sequ` factor) `using` divide) `alt`
       factor

factor :: Parser Char Expr
factor = (number `using` value) `alt`
         (literal '(' `xthen` expn `thenx` literal ')')

-- in the paper they do it more like (with some missing ())
expn' :: Parser Char Expr
expn' = ((term' `sequ` (literal '+' `xthen` term')) `using` plus) `alt`
       ((term' `sequ` (literal '-' `xthen` term')) `using` minus) `alt`
       term' 

term' :: Parser Char Expr
term' = ((factor' `sequ` (literal '*' `xthen` factor')) `using` times) `alt`
       ((factor' `sequ` (literal '/' `xthen` factor')) `using` divide) `alt`
       factor'

factor' :: Parser Char Expr
factor' = (number `using` value) `alt`
         (literal '(' `xthen` expn' `thenx` literal ')')

value :: String -> Expr 
value xs = Num (read xs :: Int)

plus :: (Expr, Expr) -> Expr
plus (x,y) = Add x y

minus :: (Expr, Expr) -> Expr
minus (x,y) = Sub x y

times :: (Expr, Expr) -> Expr
times (x,y) = Mul x y

divide :: (Expr, Expr) -> Expr
divide (x,y) = Div x y

-- evaluator

eexpn :: Parser Char Int
eexpn = ((eterm `thenx` literal '+' `sequ` eterm) `using` eplus) `alt`
       ((eterm `thenx` literal '-' `sequ` eterm) `using` eminus) `alt`
       eterm 

eterm :: Parser Char Int
eterm = ((efactor `thenx` literal '*' `sequ` efactor) `using` etimes) `alt`
       ((efactor `thenx` literal '/' `sequ` efactor) `using` edivide) `alt`
       efactor

efactor :: Parser Char Int 
efactor = (number `using` evalue) `alt`
         (literal '(' `xthen` eexpn `thenx` literal ')')

evalue :: String -> Int 
evalue xs = read xs :: Int

eplus :: (Int, Int) -> Int
eplus (x,y) = x + y

eminus :: (Int, Int) -> Int
eminus (x,y) = x - y

etimes :: (Int, Int) -> Int
etimes (x,y) = x * y

edivide :: (Int, Int) -> Int 
edivide (x,y) = x `div` y

-- 3.1 Free-format input

nibble :: Parser Char b -> Parser Char b
nibble p = white `xthen` (p `thenx` white)
	where white = many (any' literal " \t\n")

any' :: (b -> Parser a c) -> [b] -> Parser a c
any' p = foldr (alt . p) failure

symbol :: [Char] -> Parser Char [Char]
symbol = nibble . string

-- 3.3 The offside combinator

type Pos b = (b, (Int,Int))

satisfy' :: (b->Bool) -> Parser (Pos b) b
satisfy' p [] = failure []
satisfy' p (x:xs)
        | p a = succeed a xs
        | otherwise = failure xs
          where (a,(r,c)) = x

offside :: Parser (Pos b) a -> Parser (Pos b) a
offside p inp = [(v,inpOFF) | (v,[]) <- p inpON]
	where
		inpON = takeWhile (onside (head inp)) inp
		inpOFF = drop (length inpON) inp
		onside (a,(r,c)) (b,(r',c')) = r'>r && c'>c


