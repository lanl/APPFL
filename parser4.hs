-- From "Higher-Order Functions for Parsing" Graham Hutton 
-- Section 4 Miranda like parser

import Data.Char

type Parser b a = [b] -> [(a,[b])]

type Pos b = (b, (Int,Int))

-- 4.1 Example language

type Var = [Char]

data Script = SCRIPT [Def] deriving (Show)

data Def = DEF Var [Var] Expn deriving (Show)

data Expn = VAR Var | NUM Int | APPLY Expn Expn 
		| WHERE Expn [Def] deriving (Show)

-- 4.3 Lexical Analysis

data Tag = Ident | Number | Symbol | Junk

type Token = (Tag,[Char])

tok :: Parser (Pos Char) [Char] -> Tag -> Parser (Pos Char) (Pos Token)
(p `tok` t) inp = [(((t,xs),(r,c)),out) | (xs,out) <- p inp]
	          where (x,(r,c)) = head inp


