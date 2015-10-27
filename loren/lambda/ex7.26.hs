--NORMAL ORDER and APPLICATIVE ORDER using HALF

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

data Expr = Var String | Abs Expr Expr |  Expr | Comb Expr Expr  deriving (Show, Eq, Read)

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
lambda = ((literal1 "\\") `thens` var `thens` lexpr) `using` lambdaing

lambdaing :: ((String,Expr),Expr) -> Expr
lambdaing ((x,y),z) = Abs y z

parenExpr :: Parser [String] Expr
parenExpr = ((literal1 "(") `xthen` lexpr  `thenx` (literal1 ")")) 

alphaNums :: Parser [String] String
alphaNums [] = []
alphaNums (x:xs) | (isAlpha (head x) && wordt x)= succeed x xs
                 | otherwise = failed xs

wordt :: String -> Bool
wordt [] = True
wordt (x:xs) = if ((isAlpha x) || (isDigit x)) then wordt xs else False

b1 = Comb (Abs (Var "g") (Comb (Var "g") (Var "g"))) (Var "y")
c1 = "\\ x (\\y (z e))"
c2 = "(\\ z x)(\\ g f)"
c3 = "\\x y e \\x \\z o 3 "
c4 = "\\ z x (\\ g f)"
c5 = "(\\ z x)"
c6 = "(\\ z x (\\ x y))"
c7 = "(\\ z z z x) y"
c8 = "(\\ x y x) z i j"
c9 = "(\\ z z f)(\\ g g)"
c10 = "(\\ x x x)(\\ g g)"
c11 = "(\\ x x x)(\\ g g g)"
c12 = "(\\ x x y)(\\ g g)"
c13 = "(\\ x x y)(\\ g g g)"
s1 = ["a","b","c","d"]
--Remember - variables must start with a letter. 

--END PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

--betaReduce1 ["z","g"] (parse "(\\ y (\\ f f x) y)") (Var "x") (Comb (Var "f") (Var "y"))
--Abs (Var "z") (Comb (Abs (Var "g") (Comb (Var "g") (Comb (Var "f") (Var "y")))) (Var "z"))
--END BETA REDUCTION

--BEGIN EVALUATOR

--eval ["z","w"] (parse c2) = Var "x"
--parse c3 = Abs (Var "x") (Comb (Comb (Var "y") (Var "e")) (Abs (Var "x") (Abs (Var "z") (Var "o"))))
--eval ["z","w"] (parse c7) = Comb (Comb (Var "y") (Var "y")) (Var "x")

--Scratch Work 
{-
evalOnce :: [String] -> Expr -> Expr
evalOnce ws (Var x) = (Var x)
evalOnce ws (Abs x y) = (Abs x y)
evalOnce ws (Comb (Abs (Var x) y) z) = (betaReduce1 ws y (Var x) z)
evalOnce ws (Comb x y) = (Comb (evalOnce ws  x) (evalOnce ws y))

evalOne :: [String] -> Expr -> Expr
evalOne ws (Var x) = (Var x)
evalOne ws (Comb (Abs (Var x) y) z) = evalOne ws (betaReduce1 ws y (Var x) z)
evalOne ws (Abs x y) = (Abs x (evalOne ws y))
evalOne ws (Comb x y) = (Comb (evalOne ws  x) (evalOne ws y))

--Multiple Evaluator
eval :: [String] -> Expr -> Expr
eval ws (Var x) = (Var x)
eval ws (Comb (Abs (Var x) y) z) = if (Comb (Abs (Var x) y) z) == (betaReduce1 ws y (Var x) z) then (Comb (Abs (Var x) y) z) else eval ws (betaReduce1 ws y (Var x) z)
eval ws (Abs x y) = if (Abs x (eval ws y)) == (Abs x y) then (Abs x y) else eval ws (Abs x (eval ws y))
eval ws (Comb x y) = if  (Comb (eval (oneSplit ws) x) (eval (twoSplit ws) y)) == (Comb x y) then (Comb x y) else eval ws (Comb (eval (oneSplit ws) x) (eval (twoSplit ws) y))
--End Multiple Evaluator
-}


--BEGIN FREE VARIABLES
freeVars :: Expr -> [String]
freeVars (Var x) = [x]
freeVars (Comb x y) = (freeVars x)++(freeVars y)
freeVars (Abs x y) = setDiff (freeVars x) (freeVars y) []

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
alphaReduce x w (Comb y z) = Comb (alphaReduce x w y) (alphaReduce x w z)

mayAlphaReduce :: String -> Expr -> Bool
mayAlphaReduce z (Abs (Var x) y) = if notOccur z y then True else False
mayAlphaReduce _ (Abs x y) = False
mayAlphaReduce _ (Var x) = False
mayAlphaReduce _ (Comb x y) = False

notOccur :: String -> Expr -> Bool
notOccur z (Abs x y) = (notOccur z x) && (notOccur z y)
notOccur z (Var x) = if x == z then False else True
notOccur z (Comb x y) = (notOccur z x) && (notOccur z y)
--END ALPHA REDUCTION


--BEGIN BETA REDUCTION
betaReduce1 :: [String] -> Expr -> Expr -> Expr -> Expr
betaReduce1 xs (Var a) (Var b) c = if a == b then c else (Var a)
betaReduce1 xs (Comb a b) c d = Comb (betaReduce1 xs a c d) (betaReduce1 xs b c d)
betaReduce1 (x:xs) (Abs (Var a) b) (Var c) d | a == c = (Abs (Var a) b)
                                             | (a /= c) && (not (a `elem` (freeVars d))) = (Abs (Var a) (betaReduce1 xs b (Var c) d))
                                             | otherwise = (Abs (Var (fst r)) (betaReduce1 (snd r) (betaReduce1 (snd r) b (Var a) (Var (fst r))) (Var c) d))    
                                               where r = nextFree (x:xs) (Abs (Var a) b) d

nextFree :: [String] -> Expr -> Expr -> (String,[String])
nextFree [] _ _ = error "ran out"
nextFree (x:xs) y z | x `elem` (freeVars y)++(freeVars z) = nextFree xs y z
                    | otherwise = (x,xs)
--END BETA REDUCTION


--BEGIN FREE VARIABLES
fvs = ['v' : show i| i<- [0..]]

oneSplit :: [String] -> [String]
oneSplit (x:y:xs) = x:(oneSplit(xs))

twoSplit :: [String] -> [String]
twoSplit (x:y:xs) = y:(twoSplit(xs))
--END FREE VARIABLES


--BEGINNNNNN Normal Order Evaluator
normEval :: [String] -> Expr -> Expr
normEval ws (Var x) = (Var x)
normEval ws (Comb (Abs (Var x) y) z) | (Comb (Abs (Var x) y) z) == (betaReduce1 ws y (Var x) z) = (Comb (Abs (Var x) y) z)
                                     | otherwise = normEval (oneSplit ws) (betaReduce1 (twoSplit ws) y (Var x) z)
normEval ws (Abs x y) | (Abs x (normEval ws y)) == (Abs x y) = (Abs x y) 
                      | otherwise = normEval (oneSplit ws) (Abs x (normEval (twoSplit ws) y))
normEval ws (Comb x y) | (Comb (normEval (oneSplit ws) x) (normEval (twoSplit ws) y)) == (Comb x y) = (Comb x y)
                       | otherwise =  normEval (oneSplit ws) (Comb (normEval (oneSplit (twoSplit ws)) x) (normEval (oneSplit (twoSplit ws)) y))
--ENDDDDDDDD Normal Order Evaluator


--BEGINNNNNN Applicative Order Evaluator
appEval :: [String] -> Expr -> Expr 
appEval ws (Var x) = (Var x) 
appEval ws (Comb (Abs (Var x) y) z) | (Comb (Abs (Var x) y) z) == (Comb (Abs (Var x) (appEval ws y)) z) = if (Comb (Abs (Var x) y) z) == (Comb (Abs (Var x) y) (appEval ws z)) then (if (betaReduce1 (twoSplit ws) y (Var x) z) == (Comb (Abs (Var x) y) z) then (Comb (Abs (Var x) y) z) else appEval (oneSplit ws) (betaReduce1 (twoSplit ws) y (Var x) z)) else appEval (oneSplit ws) (Comb (Abs (Var x) y) (appEval (twoSplit ws) z))
                                    | otherwise = appEval (oneSplit ws) (Comb (Abs (Var x) (appEval (twoSplit ws) y)) z)
appEval ws (Abs x y) | (Abs x (appEval ws y)) == (Abs x y) = (Abs x y) 
                     | otherwise = appEval (oneSplit ws) (Abs x (appEval (twoSplit ws) y))
appEval ws (Comb x y) | (Comb (appEval (oneSplit ws) x) (appEval (twoSplit ws) y)) == (Comb x y) = (Comb x y)
                      | otherwise =  appEval (oneSplit ws) (Comb (appEval (oneSplit (twoSplit ws)) x) (appEval (oneSplit (twoSplit ws)) y))
--ENDDDDDDDD Applicative Order Evaluator
