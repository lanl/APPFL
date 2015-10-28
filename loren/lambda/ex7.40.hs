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
--          | Expr 
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
c3 = "\\x y e \\x \\z o"
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

--BEGIN FREE VARIABLES
fvs :: [String]
fvs = ['v' : show i| i<- [0..]]

oneSplit :: [String] -> [String]
oneSplit (x:y:xs) = x:(oneSplit(xs))

twoSplit :: [String] -> [String]
twoSplit (x:y:xs) = y:(twoSplit(xs))
--END FREE VARIABLES

f :: (a,b,c) -> a
f (x,_,_) = x
s :: (a,b,c) -> b
s (_,y,_) = y
t :: (a,b,c) -> c
t (_,_,z) = z

type Env = [(String,Expr)]

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

--BEGINNNNNN Normal Order Evaluator
nor e = putStrLn $ intercalate "\n" $ map show $ startEvalNormOrder e

startEvalNormOrder :: Expr -> [Expr]
startEvalNormOrder expression = expression:(evaluateNormalOrder (expression,fvs,[]))

evaluateNormalOrder :: (Expr,[String],Env) -> [Expr] 
evaluateNormalOrder b = case theEval b of
                          Just a -> (f a):(evaluateNormalOrder a)
                          Nothing -> []

normEval :: (Expr,[String],Env) -> (Expr,[String],Env)
normEval (Var x,ws,es) | canSub (x,es) = doSub (Var x,ws,es)
                       | otherwise = error "NOOOO1"
normEval (Comb (Abs (Var x) y) z,ws,es) = (t1, t2, (x,z):es)
                                          where (r1,r2) = nextFree ws y z
                                                (t1,t2) = changeVar(y,x,z,r2)            
normEval (Abs x y,ws,es) | canStep (y,es) = (Abs x t1,t2,t3)
                         | otherwise = error "NOOOO2"
                           where (t1,t2,t3) = normEval (y,ws,es)
normEval (Comb x y,ws,es) | canStep (x,es) = (Comb s1 y, s2, s3)
                          | canStep (y,es) = (Comb x t1, t2, t3)             
                          | otherwise = error "NOOOO3"
                                        where (s1,s2,s3) = normEval (x,ws,es)
                                              (t1,t2,t3) = normEval (y,ws,es)
--ENDDDDDDDD Normal Order Evaluator
--BEGIN CHECKING STUFF
theEval :: (Expr,[String],Env) -> Maybe (Expr,[String],Env) 
theEval (expression,ws,es) | canStep (expression,es) = Just (normEval (expression,ws,es))
                           | otherwise = Nothing

canStep :: (Expr,Env) -> Bool
canStep (Var x,es) = canSub (x,es)
canStep (Comb (Abs (Var x) y) z,es) = True
canStep (Abs x y,es) = canStep (y,es)
canStep (Comb x y,es) = (canStep (x,es)) || (canStep (y,es))

canSub :: (String,Env) -> Bool
canSub (_,[]) = False
canSub (x,e:es) | x == fst e = True
                | otherwise = canSub (x,es)

doSub :: (Expr,[String],Env) -> (Expr,[String],Env)
doSub (Var x,ws,es) = (makeSub (x,es),ws,es)
doSub (_,_,_) = error "NOOOO4"

makeSub :: (String,Env) -> Expr
makeSub (_,[]) = error "NOOO5"
makeSub (x,e:es) | x == fst e = snd e
                 | otherwise = makeSub(x,es)

nextFree :: [String] -> Expr -> Expr -> (String,[String])
nextFree [] _ _ = error "ran out"
nextFree (x:xs) y z | x `elem` (freeVars y)++(freeVars z) = nextFree xs y z
                    | otherwise = (x,xs)
--END CHECKING STUFF
--BEGIN Changing Variables
changeVar :: (Expr,String,Expr,[String]) -> (Expr,[String])
changeVar (Var a,x,z,ws) = (Var a,ws)
changeVar (Comb a b,x,z,ws) = (Comb r1 s1, s2)
                            where (r1,r2) = changeVar (a,x,z,ws)
                                  (s1,s2) = changeVar (b,x,z,r2)
changeVar (Abs (Var a) b,x,z,ws) | a == x = (Abs (Var a) b,ws)
                                 | (a /= x) && (not (a `elem` (freeVars z))) = (Abs (Var a) r1, r2)
                                 | otherwise = (Abs (Var t1) s1,s2)  
                                   where (r1,r2) = changeVar (b,x,z,ws)
                                         (t1,t2) = nextFree ws b b 
                                         (s1,s2) = betaReduce (b,a,(Var t1),t2)
--END Changing Variables
--Begin Beta Reduce (in expression, change this to that with these free variables)
betaReduce :: (Expr,String,Expr,[String]) -> (Expr,[String])
betaReduce (Var a,x,z,ws) | a == x = (z,ws)
                          | otherwise = (Var a,ws)
betaReduce (Comb a b,x,z,ws) = (Comb s1 t1,t2)
                               where (s1,s2) = betaReduce (a,x,z,ws)
                                     (t1,t2) = betaReduce (b,x,z,s2)
betaReduce (Abs (Var a) b,x,z,ws) | x == a = (Abs (Var a) b, ws)
                                  | (x /= a) && (not (x `elem` freeVars z)) = reduction1 (Abs (Var a) b,x,z,ws)
                                  | otherwise = reduction2 (Abs (Var a) b,x,z,ws)

reduction1 :: (Expr,String,Expr,[String]) -> (Expr,[String])
reduction1 (Abs (Var a) b,x,z,ws) = (Abs (Var a) s1, s2)
                                    where (s1,s2) = betaReduce (b,x,z,ws)

reduction2 :: (Expr,String,Expr,[String]) -> (Expr,[String])
reduction2 (Abs (Var a) b,x,z,ws) = (Abs (Var s1) t1, t2)
                                    where (s1,s2) = nextFree ws b b
                                          (r1,r2) = betaReduce (b,a,(Var s1),ws)
                                          (t1,t2) = betaReduce (r1,x,z,r2)
--End Beta Reduce 
c22 = "\\x y"
d22 = parse c22
set = ["v0","v1","v2","v3","v4"]
c23 = "(\\x (x (\\y (\\z y x) w)x))y"
d23 = parse c23

c24 = "(\\x x)"
d24 = parse c24

c25 = "(x y)"
d25 = parse c25
