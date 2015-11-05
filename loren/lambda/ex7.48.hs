--NORMAL ORDER and APPLICATIVE ORDER using Thread

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

data Expr = Var String 
          | Abs Expr Expr 
          | Comb Expr Expr  deriving (Eq, Read)

instance Show Expr where
    show (Var v) = v
    show (Abs e1 e2) = "(\\" ++ show e1 ++ "." ++ show e2 ++ ")"
    show (Comb e1 e2) = show e1 ++ " " ++ show e2

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

--END PARSERRRRRRRRRRRRRRRRRRRRRRRRRRRRRRR

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
nor e = putStrLn $ intercalate "\n" $ map show $ startEvalNormOrder e
nor2 e = last (startEvalNormOrder e)
nor3 e = startLambToInt (nor2 e) 

startEvalNormOrder :: Expr -> [Expr]
startEvalNormOrder expression = expression:(evaluateNormalOrder (fvs,expression))

evaluateNormalOrder :: ([String],Expr) -> [Expr] 
evaluateNormalOrder (xs,expression) = case theEval (xs,expression) of
                                              Just a -> (snd a):(evaluateNormalOrder (fst a, snd a))
                                              Nothing -> []

normEval :: ([String],Expr) -> ([String],Expr)
normEval (ws,(Var x)) = (ws,(Var x))
normEval (ws,(Comb (Abs (Var x) y) z)) = (betaReduce1 ws y (Var x) z)
normEval (ws,(Abs x y)) = (t1,(Abs x t2))
                          where (t1,t2) = (normEval (ws,y))
normEval (ws,(Comb x y)) | (checkEval x) = (s1, Comb s2 y)
                         | (checkEval y) = (t1, Comb x t2)             
                         | otherwise = (ws,(Comb x y))
                                       where (s1,s2) = normEval (ws, x)
                                             (t1,t2) = normEval (ws, y)
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
nor1 e = putStrLn $ intercalate "\n" $ map show $ startEvalAppOrder e

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
appEval (ws,(Comb (Abs (Var x) y) z)) | y /= (snd (appEval (ws,y))) = (s1, Comb (Abs (Var x) s2) z)
                                      | z /= (snd (appEval (ws,z))) = (t1, Comb (Abs (Var x) y) t2)
                                      | otherwise = (betaReduce1 ws y (Var x) z)    
                                                    where (s1,s2) = appEval (ws, y)
                                                          (t1,t2) = appEval (ws, z)
appEval (ws,(Abs x y)) = (t1,(Abs x t2))
                         where (t1,t2) = (appEval (ws,y))
appEval (ws,(Comb x y)) | Comb (snd (appEval (ws, x))) y /= (Comb x y) = (s1, Comb s2 y)
                        | Comb x (snd (appEval (ws, y))) /= (Comb x y) = (t1, Comb x t2)             
                        | otherwise = (ws,(Comb x y))
                                      where (s1,s2) = appEval (ws, x)
                                            (t1,t2) = appEval (ws, y)
--ENDDDDDDDD Applicative Order Evaluator
identity = "(\\x x)"

succ1  = "(\\n \\f \\x f (n f x))"
psucc1 = parse succ1

zero = "(\\f \\x x)"
ones = "(\\f \\x f x)"
twos = "(\\f \\x f(f x))"
threes = "(\\f \\x f(f(f x)))"
fours = "(\\f \\x f(f(f(f x))))"
fives = "(\\f \\x f(f(f(f(f x)))))"

true1 = "(\\x \\y x)"

false1 = "(\\x \\y y)"

and1 = "(\\p \\q p q p)"

or1 = "(\\p \\q p p q)"

not1 = "(\\p \\a \\b p b a)"

ifThenElse = "(\\p \\a \\b p a b)"

isZero = "(\\n n (\\x (\\x \\y y)) (\\x \\y x))"

add1 = "(\\m (\\n (\\f (\\x m f (n f x)))))"

mult = "(\\g (\\h (\\i (g (h i)))))"

pow = "(\\m (\\e e m))"

pred1 = "(\\n (\\f (\\x n (\\g  (\\h h (g f))) (\\u x) (\\u u))))"

sub1 = "(\\m (\\n n " ++ pred1 ++ " m ) )"

checkZero y = "( " ++ isZero ++ " "++  y ++ " )"

startIntToLamb :: (Eq a, Num a) => a -> [Char]
startIntToLamb n = "(\\f (\\ x (" ++ (intToLamb n) ++ ")))"
intToLamb 0 = " x"
intToLamb n = " ( f " ++ (intToLamb (n-1)) ++ " ) " 

startLambToInt :: Expr -> Int
startLambToInt (Abs x (Abs y z)) = lambToInt z
lambToInt (Comb x (Comb y z)) = 1  + lambToInt (Comb y z)
lambToInt (Comb (Var x) (Var y)) = 1

--nor $ (parse (ifThenElse ++  true1 ++ " True" ++ " False")) = True
--nor $ (parse (ifThenElse ++  false1 ++ " True" ++ " False")) = False
--nor $ parse (succ1 ++ (startIntToLamb 4)) = (\f.(\x.f f f f f x))
--nor $ (parse (or1 ++ true1 ++ true1)) = (\x.(\y.x))
--nor $ (parse (or1 ++ true1 ++ true1 ++ " True " ++ " False ")) = True
--nor $ (parse (not1 ++ true1 ++ " True " ++ " False ")) = False
--nor $ (parse (not1 ++ false1 ++ " True " ++ " False ")) = True
--nor $ parse (isZero ++ zero ++ " True" ++ " False") = (\y.True) False
--nor $ parse (isZero ++ (startIntToLamb 1) ++ " True" ++ " False")
--(\n.n (\x.(\x.(\y.y))) (\x.(\y.x))) (\f.(\x.f x)) = False
--nor $ parse (add1 ++ (startIntToLamb 2) ++ (startIntToLamb 2)) = (\f.(\x.f f f f x))
--nor $ parse (mult ++ (startIntToLamb 2) ++ (startIntToLamb 2))

try1 = nor $ parse (mult ++ (startIntToLamb 100) ++ (startIntToLamb 20))

try2 = Abs (Var "i") (Abs (Var "x") (Comb (Comb (Comb (Var "i") (Var "i")) (Abs (Var "x") (Comb (Comb (Var "i") (Var "i")) (Var "x")))) (Var "x")))

try3 = parse (" ( " ++ (startIntToLamb 2) ++ "( " ++ (startIntToLamb 1) ++ " i ))")

try4 = Comb (Abs (Var "x") (Comb (Var "i") (Abs (Var "x") (Comb (Var "i") (Var "x"))))) (Var "x")

{-
(\g.(\h.(\i.g h i))) (\f.(\x.f f x)) (\f.(\x.f x))
(\h.(\i.(\f.(\x.f f x)) h i)) (\f.(\x.f x))
(\i.(\f.(\x.f f x)) (\f.(\x.f x)) i)
(\i.(\x.(\f.(\x.f x)) i (\f.(\x.f x)) i x))
(\i.(\x.(\x.i x) (\f.(\x.f x)) i x))
(\i.(\x.i (\f.(\x.f x)) i x))
(\i.(\x.i (\x.i x) x))
-}

{-
Comb (Comb (Abs (Var "g") (Abs (Var "h") (Abs (Var "i") (Comb (Var "g") (Comb (Var "h") (Var "i")))))) (Abs (Var "f") (Abs (Var "x") (Comb (Comb (Var "f") (Var "f")) (Var "x"))))) (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x"))))
Comb (Abs (Var "h") (Abs (Var "i") (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Comb (Var "f") (Var "f")) (Var "x")))) (Comb (Var "h") (Var "i"))))) (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x"))))
Abs (Var "i") (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Comb (Var "f") (Var "f")) (Var "x")))) (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x")))) (Var "i")))
Abs (Var "i") (Abs (Var "x") (Comb (Comb (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x")))) (Var "i")) (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x")))) (Var "i"))) (Var "x")))
Abs (Var "i") (Abs (Var "x") (Comb (Comb (Abs (Var "x") (Comb (Var "i") (Var "x"))) (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x")))) (Var "i"))) (Var "x")))
Abs (Var "i") (Abs (Var "x") (Comb (Comb (Var "i") (Comb (Abs (Var "f") (Abs (Var "x") (Comb (Var "f") (Var "x")))) (Var "i"))) (Var "x")))
Abs (Var "i") (Abs (Var "x") (Comb (Comb (Var "i") (Abs (Var "x") (Comb (Var "i") (Var "x")))) (Var "x")))
-}

try6 = nor $ parse (pow ++ (startIntToLamb 2) ++ (startIntToLamb 9))

try7 = nor $ parse (sub1 ++ (startIntToLamb 50) ++ (startIntToLamb 5))

try8 = nor $ parse (mult ++ (startIntToLamb 100) ++ (startIntToLamb 20))

try9 = nor2 $ parse (mult ++ (startIntToLamb 100) ++ (startIntToLamb 20))

try10 = nor $ parse (pow ++ (startIntToLamb 2) ++ (startIntToLamb 2))

try11 = nor3 $ parse (pow ++ (startIntToLamb 2) ++ (startIntToLamb 2))

try12 = nor3 $ parse (add1 ++ (startIntToLamb 102) ++ (startIntToLamb 2))

try13 = nor $ parse (isZero ++ zero)
-- try13 = (\x.(\y.x))
try14 = nor $ parse (ifThenElse  ++ isZero ++ (startIntToLamb 3) ++ identity ++ succ1)

try15 = nor $ parse (ifThenElse  ++ (checkZero (startIntToLamb 3)) ++ succ1 ++ identity)

try16 = nor $ parse (ifThenElse  ++ (checkZero zero) ++ succ1 ++ identity)

func1 = "(\\n " ++ ifThenElse ++ "( " ++ isZero ++ " n" ++ " ))" ++ ones ++ identity ++ succ1

func2 = "(\\n " ++ ifThenElse ++ "( " ++ isZero ++ " n" ++ " ))" ++ zero ++ identity ++ succ1

safeSub = "(\\t ( " ++ ifThenElse ++ "( " ++ isZero ++ " t ))" ++ "( "++ identity ++ " t ) " ++ "( "++ sub1 ++ " t " ++ ones ++ " )) "
--sends zero to zero else subtracts one.

fix = "(\\g (\\x g (x x)) (\\x g (x x)))"

--G := (λr.λn.(1, if n = 0; else n × (r (n−1))))

g = "(\\r (\\n (" ++ ifThenElse ++ "( " ++ isZero ++ " n ))" ++ "( " ++ ones ++ ")" ++ "( " ++ mult ++ "(n)" ++ "( r (" ++ sub1 ++ "(n)" ++ ones ++ ")))))"

fac0 = nor $ parse (fix ++ g ++ zero)
fac1 = nor $ parse (fix ++ g ++ ones)
fac2 = nor $ parse (fix ++ g ++ twos)
fac3 = nor $ parse (fix ++ g ++ threes)
fac4 = nor $ parse (fix ++ g ++ fours)
fac5 = nor $ parse (fix ++ g ++ fives)
