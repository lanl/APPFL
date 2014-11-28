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

number :: Parser (Pos Char) [Char]
number = some (satisfy isDigit)

word :: Parser (Pos Char) [Char]
word = some (satisfy isAlpha)

string :: Eq b => [b] -> Parser (Pos b) [b]
string [] = succeed []
string (x:xs) = (literal x `sequ` string xs) `using` cons

xthen :: Parser b a -> Parser b c -> Parser b c
p1 `xthen` p2 = (p1 `sequ` p2) `using` snd

thenx :: Parser b a -> Parser b c -> Parser b a
p1 `thenx` p2 = (p1 `sequ` p2) `using` fst

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
                onside (a,(r,c)) (b,(r',c')) = r'>r && c'>c


-- 4.1 Example language

type Var = [Char]

data Script = Script [Def] deriving (Show)

data Def = Def Var [Var] Expn deriving (Show)

data Expn = Var Var | Num Int | Apply Expn Expn 
		| Where Expn [Def] deriving (Show)

-- 4.2 Layout analysis

prelex :: [Char] -> [Pos Char]
prelex = pl (0,0)
    where
        pl (r,c) [] = []
        pl (r,c) (x:xs) 
            | x == '\t' = (x,(r,c)) : pl (r, tab c) xs
            | x == '\n' = (x,(r,c)) : pl (r+1, 0) xs
            | otherwise = (x,(r,c)) : pl (r, c+1) xs
        tab c = ((c `div` 8)+1)*8		

-- 4.3 Lexical analysis

data Tag = Ident | Number | Symbol | Junk deriving (Eq,Show)

type Token = (Tag,[Char])

tok :: Parser (Pos Char) [Char] -> Tag -> Parser (Pos Char) (Pos Token)
(p `tok` t) inp = [(((t,xs),(r,c)),out) | (xs,out) <- p inp]
	          where (x,(r,c)) = head inp

lex' :: [(Parser (Pos Char) [Char],Tag)] -> Parser (Pos Char) [Pos Token]
lex' = many . (foldr op failure)
	where (p,t) `op` xs = (p `tok` t) `alt` xs	

lexer :: Parser (Pos Char) [Pos Token]
lexer = lex' [(some (any' literal " \t\n"), Junk),
		(string "where", Symbol),
		(word, Ident),
		(number, Number),
		( any' string ["(",")","="], Symbol)]

-- 4.4 Scanning

strip :: [Pos Token] -> [Pos Token]
strip = filter ((/=Junk).fst.fst)

-- 4.5 Syntax analysis

kind :: Tag -> Parser (Pos Token) [Char]
kind t = (satisfy ((==t).fst)) `using` snd

lit :: [Char] -> Parser (Pos Token) [Char]
lit xs = literal (Symbol,xs) `using` snd

prog :: Parser (Pos Token) Script
prog = many defn `using` Script

defn :: Parser (Pos Token) Def
-- todo: offside is broken
--defn = (some (kind Ident) `sequ` (lit "=" `xthen` offside body)) `using` defnFN
defn = (some (kind Ident) `sequ` (lit "=" `xthen` body)) `using` defnFN

body :: Parser (Pos Token) Expn
body = (expr `sequ` (( lit "where" `xthen` some defn) `opt` [])) `using` bodyFN

expr :: Parser (Pos Token) Expn
expr = some prim `using` (foldl1 Apply) 

prim :: Parser (Pos Token) Expn
prim = (kind Ident `using` Var) `alt`
       (kind Number `using` numFN) `alt`
       (lit "(" `xthen` (expr `thenx` lit ")"))

opt:: Parser b a -> a -> Parser b a
p `opt` v = p `alt` (succeed v)

defnFN :: ([Var], Expn) -> Def
defnFN (f:xs,e) = Def f xs e

numFN :: String -> Expn
numFN xs = Num (read xs :: Int)

bodyFN :: (Expn, [Def]) -> Expn
bodyFN (e,[]) = e
bodyFN (e,d:ds) = e `Where` (d:ds)

-- 4.6 The complete parser

parse :: [Char] -> Script
parse = fst.head.prog.strip.fst.head.lexer.prelex

