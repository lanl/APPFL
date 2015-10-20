import Data.List
import Data.Char

type Parser a b = a -> [(b,a)]

succeed :: b -> Parser a b
succeed v inp = [(v, inp)]

failed :: Parser a b
failed inp = []

satisfy :: (Char -> Bool) -> Parser [Char] Char
satisfy p []     = fail []
satisfy p (x:xs) | p x       = succeed x xs
                 | otherwise = fail xs

literal :: Char -> Parser [Char] Char
literal x = satisfy (==x)

alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) inp = p1 inp ++ p2 inp

thens :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `thens` p2) inp = [((v1, v2), out2) | (v1, out1) <- p1 inp, (v2,out2) <- p2 out1]  

xthen :: Parser a b -> Parser a c -> Parser a c
(p1 `xthen` p2) inp = ((p1 `thens` p2) `using` snd) inp

thenx :: Parser a b -> Parser a c -> Parser a b
(p1 `thenx` p2) inp = ((p1 `thens` p2) `using` fst) inp

using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) inp = [(f v, out) | (v,out) <- p inp]

cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs

many :: Parser a b -> Parser a [b]
many p = ((p `thens` many p) `using` cons)`alt` (succeed [])

some :: Parser a b -> Parser a [b]
some p = (p `thens` many p) `using` cons

number :: Parser [Char] [Char]
number = some (satisfy isDigit)

word :: Parser [Char] [Char]
word = some (satisfy isAlpha)

space :: Parser [Char] Char
space = literal ' '

tab :: Parser [Char] Char
tab = literal '\t' 

newLine :: Parser [Char] Char
newLine = literal '\n'

whitespace :: Parser [Char] Char
whitespace = space `alt` tab `alt` newLine 

whitespaces :: Parser [Char] [Char]
whitespaces = many whitespace 

satisfy1 :: (String -> Bool) -> Parser [String] String
satisfy1 p []     = failed []
satisfy1 p (x:xs) | p x       = succeed x xs
                  | otherwise = failed xs

literal1 :: String -> Parser [String] String
literal1 x = satisfy1 (==x)

--BEGIN LEXERRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
lexer :: String -> [String]
lexer [] = []
lexer (x:xs) | x == ' ' = lexer(snd(head(whitespace (x:xs))))
             | x == '\\' = "\\":(lexer xs)
             | x == '(' = "(":(lexer xs)
             | x == ')' = ")":(lexer xs)
             | isDigit x = (fst(ys)):lexer(snd(ys))
             | isAlpha x = (fst(zs)):lexer(snd(zs))
               where ys = head(number(x:xs))
                     zs = head(word(x:xs))

--END LEXERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR
data Expr = Var String | Abs String Expr | Paren Expr | Comb Expr Expr  deriving (Show, Eq, Read)
--BEGIN PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRR

parse :: String -> Expr
parse = fst.head.lexpr.lexer

combing :: Expr -> Expr -> Expr
combing x y = Comb x y

lexpr :: Parser [String] Expr
lexpr = (some expr) `using` (foldl1 combing)

expr :: Parser [String] Expr
expr = var `alt` lambda `alt` parenExpr

var :: Parser [String] Expr
var = alphaNum `using` Var

lambda :: Parser [String] Expr
lambda = ((literal1 "\\") `thens` alphaNum `thens` expr) `using` lambdaing

lambdaing :: ((String,String),Expr) -> Expr
lambdaing ((x,y),z) = Abs y z

parenExpr :: Parser [String] Expr
parenExpr = ((literal1 "(") `xthen` lexpr  `thenx` (literal1 ")")) `using` Paren

--END PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

alphaNum :: Parser [String] String
alphaNum = (number1 `alt` word1)

number1 :: Parser [String] String
number1 [] = []
number1 (x:xs) | isDigit (head x) = succeed x xs
               | otherwise = failed xs

word1 :: Parser [String] String
word1 [] = []
word1 (x:xs) | isAlpha (head x) = succeed x xs
             | otherwise = failed xs

c1 = "\\ x (\\y (z e))"
c2 = "(\\ z x)(\\ g f)"
c3 = "\\x y e \\x \\z o 3 "
c4 = "\\ z x (\\ g f)"
c5 = "(\\ z x)"
c6 = "(\\ z x (\\ x y))"
