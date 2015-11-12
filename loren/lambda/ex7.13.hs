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
lexer (x:xs) | x == '\n' = lexer(snd(head(whitespace (x:xs))))
             | x == '\t' = lexer(snd(head(whitespace (x:xs))))
             | x == ' ' = lexer(snd(head(whitespace (x:xs))))
             | x == '\\' = "\\":(lexer xs)
             | x == '(' = "(":(lexer xs)
             | x == ')' = ")":(lexer xs)
             | isDigit x = (fst(ys)):lexer(snd(ys))
             | isAlpha x = (fst(zs)):lexer(snd(zs))
               where ys = head(number(x:xs))
                     zs = head(word(x:xs))

--END LEXERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

--BEGIN PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRR

data Expr = Var String | Abs Expr Expr | Paren Expr | Comb Expr Expr  deriving (Show, Eq, Read)

parse :: String -> Expr
parse = fst.head.lexpr.lexer

combing :: Expr -> Expr -> Expr
combing x y = Comb x y

lexpr :: Parser [String] Expr
lexpr = (some expr) `using` (foldl1 combing)

expr :: Parser [String] Expr
expr = var `alt` lambda `alt` parenExpr

var :: Parser [String] Expr
var = alphaNums `using` Var

lambda :: Parser [String] Expr
lambda = ((literal1 "\\") `thens` var `thens` expr) `using` lambdaing

lambdaing :: ((String,Expr),Expr) -> Expr
lambdaing ((x,y),z) = Abs y z

parenExpr :: Parser [String] Expr
parenExpr = ((literal1 "(") `xthen` lexpr  `thenx` (literal1 ")")) `using` Paren

alphaNums :: Parser [String] String
alphaNums [] = []
alphaNums (x:xs) | (isAlpha (head x) && wordt x)= succeed x xs
                 | otherwise = failed xs

wordt :: String -> Bool
wordt [] = True
wordt (x:xs) = if ((isAlpha x) || (isDigit x)) then wordt xs else False

c1 = "\\ x (\\y (z e))"
c2 = "(\\ z x)(\\ g f)"
c3 = "\\x y e \\x \\z o 3 "
c4 = "\\ z x (\\ g f)"
c5 = "(\\ z x)"
c6 = "(\\ z x (\\ x y))"
c7 = "1 3 5 234 2o 281 s0"
c8 = "(\\ var1 var2  var3) (\\ d 3 (s d))"

--END PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

--BEGIN FREE VARIABLES
freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Comb x y) = (freeVars x)++(freeVars y)
freeVars (Abs x y) = setDiff (freeVars x) (freeVars y) []
freeVars (Paren x) = freeVars x

setDiff :: [String] -> [String] -> [String] -> [String]
setDiff [] ys _ = ys
setDiff (x:xs) [] zs = setDiff xs zs []
setDiff (x:xs) (y:ys) zs = if x == y then setDiff (x:xs) ys zs else setDiff (x:xs) ys (y:zs)
--END FREE VARIABLES

--BEGIN ALPHA REDUCTION

checkAlphaReduce :: String ->  Expr -> Expr 
checkAlphaReduce x (Abs (Var y) z)  = if mayAlphaReduce x (Abs (Var y) z) then alphaReduce x y (Abs (Var y) z) else (Abs (Var y) z)
checkAlphaReduce x y = y

alphaReduce :: String -> String -> Expr -> Expr
alphaReduce x w (Abs y z) = Abs (alphaReduce x w y) (alphaReduce x w z)
alphaReduce x w (Var y) | w == y = Var x
                        | otherwise = Var y
alphaReduce x w (Paren y) = Paren (alphaReduce x w y)
alphaReduce x w (Comb y z) = Comb (alphaReduce x w y) (alphaReduce x w z)


mayAlphaReduce :: String -> Expr -> Bool
mayAlphaReduce z (Abs (Var x) y) = if notOccur z y then True else False
mayAlphaReduce _ (Abs x y) = False
mayAlphaReduce _ (Var x) = False
mayAlphaReduce _ (Paren x) = False
mayAlphaReduce _ (Comb x y) = False

notOccur :: String -> Expr -> Bool
notOccur z (Abs x y) = (notOccur z x) && (notOccur z y)
notOccur z (Var x) = if x == z then False else True
notOccur z (Paren x) = notOccur z x
notOccur z (Comb x y) = (notOccur z x) && (notOccur z y)

--END ALPHA REDUCTION

--BEGIN BETA REDUCTION

betaReduce :: Expr -> String -> Expr -> Expr
betaReduce (Var x) "y" z = z
