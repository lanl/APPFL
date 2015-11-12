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
c14 = "(\\ x x) (x y)"
c15 = "(\\ x (\\ y (\\ f f x y))) y"
c16 = "(\\ x (\\ y (\\ f (\\ f x y y)))) y"
c17 = "(\\ x (\\ y (\\ f (\\ f x y y))) f) y"
c18 = "(\\ x (\\ y y x)) (\\ z y z)"
c19 = "(\\ f (f y))(\\x x)"
c20 = "(\\ s (s s))(\\ x x x)"
d1 = parse c1
d2 = parse c2
d3 = parse c3
d4 = parse c4
d5 = parse c5
d6 = parse c6
d7 = parse c7
d8 = parse c8
d9 = parse c9
d10 = parse c10
d11 = parse c11
d12 = parse c12
d13 = parse c13
d14 = parse c14
d15 = parse c15
d16 = parse c16
d17 = parse c17
d18 = parse c18
d19 = parse c19 
d20 = parse c20
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


--BEGIN BETA REDUCTION
betaReduce1 :: [String] -> Expr -> Expr -> Expr -> ([String],Expr)
betaReduce1 xs (Var a) (Var b) c = if a == b then (xs,c) else (xs,(Var a))
betaReduce1 xs (Comb a b) c d = reduction1 xs (Comb a b) c d
betaReduce1 xs (Abs (Var a) b) (Var c) d | a == c = (xs,(Abs (Var a) b))
                                         | (a /= c) && (not (a `elem` (freeVars d))) = reduction2 xs (Abs (Var a) b) (Var c) d
                                         | otherwise = reduction3 xs (Abs (Var a) b) (Var c) d
                                          
reduction1 :: [String] -> Expr -> Expr -> Expr -> ([String],Expr)
reduction1 xs (Comb a b) c d = (fst z,(Comb (snd y) (snd z)))
                               where y = betaReduce1 xs a c d
                                     z = (betaReduce1 (fst y) b c d)
reduction1 _ _ _ _ = error "Here 1"

reduction2 :: [String] -> Expr -> Expr -> Expr -> ([String],Expr)
reduction2 xs (Abs (Var a) b) (Var c) d = (fst y, Abs (Var a) (snd y))
                                          where y = (betaReduce1 xs b (Var c) d)
reduction2 _ _ _ _ = error "Here 2"

reduction3 :: [String] -> Expr -> Expr -> Expr -> ([String],Expr)
reduction3 xs (Abs (Var a) b) (Var c) d = (fst v, Abs (Var (fst r)) (snd v))
                                          where r = nextFree xs (Abs (Var a) b) d
                                                t = (betaReduce1 (snd r) b  (Var a) (Var (fst r)))
                                                v = (betaReduce1 (fst t) (snd t) (Var c) d)
reduction3 _ _ _ _ = error "Here 3"

nextFree :: [String] -> Expr -> Expr -> (String,[String])
nextFree [] _ _ = error "ran out"
nextFree (x:xs) y z | x `elem` (freeVars y)++(freeVars z) = nextFree xs y z
                    | otherwise = (x,xs)
--END BETA REDUCTION

--BEGIN FREE VARIABLES
fvs :: [String]
fvs = ['v' : show i| i<- [0..]]

oneSplit :: [String] -> [String]
oneSplit (x:y:xs) = x:(oneSplit(xs))

twoSplit :: [String] -> [String]
twoSplit (x:y:xs) = y:(twoSplit(xs))
--END FREE VARIABLES


--BEGINNNNNN Normal Order Evaluator
startEvalNormOrder :: Expr -> [Expr]
startEvalNormOrder expression = expression:(evaluateNormalOrder (fvs,expression))

evaluateNormalOrder :: ([String],Expr) -> [Expr] 
evaluateNormalOrder (xs,expression) = case theEval (xs,expression) of
                                              Just a -> (snd a):(evaluateNormalOrder (fst a, snd a))
                                              Nothing -> []

normEval :: ([String],Expr) -> ([String],Expr)
normEval (ws,(Var x)) = (ws,(Var x))
normEval (ws,(Comb (Abs (Var x) y) z)) = ((oneSplit ws),snd (betaReduce1 (twoSplit ws) y (Var x) z))
normEval (ws,(Abs x y)) = (oneSplit ws,(Abs x (snd (normEval ((twoSplit ws),y)))))
normEval (ws,(Comb x y)) | Comb (snd (normEval ((twoSplit ws), x))) y /= (Comb x y) = (oneSplit ws, Comb (snd (normEval ((twoSplit ws), x))) y)
                         | Comb x (snd (normEval ((twoSplit ws), y))) /= (Comb x y) = (oneSplit ws, Comb x (snd (normEval ((twoSplit ws), y))))
                         | otherwise = (ws,(Comb x y))

theEval :: ([String],Expr) -> Maybe ([String],Expr) 
theEval (xs,expression) | checkEval expression = Just (normEval (xs,expression))
                        | otherwise = Nothing
--ENDDDDDDDD Normal Order Evaluator

checkEval :: Expr -> Bool
checkEval (Var x) = False
checkEval (Comb (Abs (Var x) y) z) = True
checkEval (Abs x y) = checkEval y
checkEval (Comb x y) = (checkEval x) || (checkEval y)

--BEGINNNNNN Applicative Order Evaluator
startEvalAppOrder :: Expr -> [Expr]
startEvalAppOrder expression = expression:(evaluateApplicativeOrder (fvs,expression))

evaluateApplicativeOrder :: ([String],Expr) -> [Expr]
evaluateApplicativeOrder (xs,expression) = case anEval (xs,expression) of
                                             Just a -> (snd a):(evaluateApplicativeOrder (fst a, snd a))
                                             Nothing -> []

anEval :: ([String],Expr) -> Maybe ([String],Expr) 
anEval (xs,expression) | checkEval expression = Just (appEval (xs,expression))
                       | otherwise = Nothing

appEval :: ([String],Expr) -> ([String],Expr) 
appEval (ws,(Var x)) = (ws,(Var x)) 
appEval (ws,(Comb (Abs (Var x) y) z)) | y /= (snd (appEval ((twoSplit ws),y))) = (oneSplit ws, Comb (Abs (Var x) (snd(appEval (twoSplit ws, y)))) z)
                                      | z /= (snd (appEval ((twoSplit ws),z))) = (oneSplit ws, Comb (Abs (Var x) y) (snd (appEval(twoSplit ws, z))))
                                      | otherwise = ((oneSplit ws),snd (betaReduce1 (twoSplit ws) y (Var x) z))        

appEval (ws,(Abs x y)) = (oneSplit ws,(Abs x (snd (appEval ((twoSplit ws),y)))))
appEval (ws,(Comb x y)) | Comb (snd (appEval ((twoSplit ws), x))) y /= (Comb x y) = (oneSplit ws, Comb (snd (appEval ((twoSplit ws), x))) y)
                        | Comb x (snd (appEval ((twoSplit ws), y))) /= (Comb x y) = (oneSplit ws, Comb x (snd (appEval ((twoSplit ws), y))))
                        | otherwise = (ws,(Comb x y))
--ENDDDDDDDD Applicative Order Evaluator


--TEST CASES

--Use the functions startEvalAppOrder and startEvalNormOrder with a single expression as a parameter
--Some premade expressions are from d1 to d20

--startEvalNormOrder d10 = [Comb (Abs (Var "x") (Comb (Var "x") (Var "x"))) (Abs (Var "g") (Var "g")),Comb (Abs (Var "g") (Var "g")) (Abs (Var "g") (Var "g")),Abs (Var "g") (Var "g")]
--startEvalAppOrder d10 = [Comb (Abs (Var "x") (Comb (Var "x") (Var "x"))) (Abs (Var "g") (Var "g")),Comb (Abs (Var "g") (Var "g")) (Abs (Var "g") (Var "g")),Abs (Var "g") (Var "g")]
--startEvalNormOrder d11 = **infinite recursion**
--startEvalAppOrder d11 = **infinite recursion**
--startEvalAppOrder d17 = [Comb (Abs (Var "x") (Comb (Abs (Var "y") (Abs (Var "f") (Abs (Var "f") (Comb (Comb (Var "x") (Var "y")) (Var "y"))))) (Var "f"))) (Var "y"),Comb (Abs (Var "x") (Abs (Var "v3") (Abs (Var "v7") (Comb (Comb (Var "x") (Var "f")) (Var "f"))))) (Var "y"),Abs (Var "v3") (Abs (Var "v7") (Comb (Comb (Var "y") (Var "f")) (Var "f")))]
--startEvalNormOrder d18 = [Comb (Abs (Var "x") (Abs (Var "y") (Comb (Var "y") (Var "x")))) (Abs (Var "z") (Comb (Var "y") (Var "z"))),Abs (Var "v1") (Comb (Var "v1") (Abs (Var "z") (Comb (Var "y") (Var "z"))))]
