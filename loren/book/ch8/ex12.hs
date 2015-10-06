import Data.Char
type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v inp = [(v, inp)]
--succeed "12" "123" = [("12","123")

failed :: Parser a b
failed inp = []
--failed "anything" = []

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy p []     = fail []
satisfy p (x:xs) | p x       = succeed x xs
                 | otherwise = fail xs
--satisfy isDigit "12345" = [('1',"2345")]
--satisfy isDigit "asdf" = []

literal :: Char -> Parser Char Char
literal x = satisfy (==x)
--literal '3' "1234" = []
--literal '1' "1234" = [('1',"234")]

cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs
--cons ('i', " how are you?")"i how are you?"

alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) inp = p1 inp ++ p2 inp
--(literal '3' `alt` literal '5') "2345" = []
--(literal '2' `alt` literal '5') "2345" = [('2',"345")]
--(literal '3' `alt` literal '2') "2345" = [('2',"345")]
--(literal '2' `alt` literal '3') "2345" = [('2',"345")]

thens :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `thens` p2) inp = [((v1, v2), out2) | (v1, out1) <- p1 inp, (v2,out2) <- p2 out1]  
--(number `thens` number) "123" = [(("12","3"),""),(("1","23"),""),(("1","2"),"3")]
--(digit `thens` digit) "1234" = [(('1','2'),"34")]

xthen :: Parser a b -> Parser a c -> Parser a c
(p1 `xthen` p2) inp = ((p1 `thens` p2) `using` snd) inp
--(digit `xthen` digit) "1234" = [('2',"34")]

thenx :: Parser a b -> Parser a c -> Parser a b
(p1 `thenx` p2) inp = ((p1 `thens` p2) `using` fst) inp
--(digit `thenx` digit) "1234" = [('1',"34")]

using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) inp = [(f v, out) | (v,out) <- p inp]
--((succeed 'e') `using` toUpper) "anything" = [('E',"anything")]

many :: Parser a b -> Parser a [b]
many p = ((p `thens` many p) `using` cons)`alt` (succeed [])
--many digit "12345" = [("12345",""),("1234","5"),("123","45"),("12","345"),("1","2345"),("","12345")]
--many digit "a12345" = [("","a12345")]

some :: Parser a b -> Parser a [b]
some p = (p `thens` many p) `using` cons
--some digit "a12345" = []
--some digit "12345" = [("12345",""),("1234","5"),("123","45"),("12","345"),("1","2345")]

number :: Parser Char [Char]
number = some (satisfy isDigit)
--number "12345" = [("12345",""),("1234","5"),("123","45"),("12","345"),("1","2345")]
--number "a1234" = []

negNumber :: Parser Char [Char]
negNumber (x:xs) | x == '-' = (x:xs,""): (appendMinus(number xs))
                 | otherwise  = failed xs
--negNumber "12345" = []
--negNumber "-12345" = [("-12345",""),("12345","-"),("1234","-5"),("123","-45"),("12","-345"),("1","-2345")]

appendMinus :: [([Char],[Char])] -> [([Char],[Char])]
appendMinus [] = []
appendMinus (x:xs) = (fst(x),'-':snd(x)):(appendMinus xs)
--appendMinus [("123","343")] = [("123","-343")]

digit :: Parser Char Char
digit = satisfy isDigit
--digit "123" = [('1',"23")]
--digit "a123" = []

int :: Parser Char [Char]
int = number `alt` negNumber
--int  "1234" = [("1234",""),("123","4"),("12","34"),("1","234")]
--int "-1234" = [("-1234",""),("1234","-"),("123","-4"),("12","-34"),("1","-234")]


optMinus :: Parser Char [Char]
optMinus (x:xs) | x == '-' = [("-",xs)]
           | otherwise = [("",(x:xs))]
--optMinus "-1234" = [("-","1234")]
--optMinus "1234" = [("","1234")]

concatPair :: [(([Char],[Char]),[Char])] -> [([Char],[Char])]
concatPair [] = []
concatPair (x:xs) = (fst(fst(x)) ++ snd(fst(x)),snd(x)):(concatPair xs)
--concatPair [(("1234","5678"),"Hi")] = [("12345678","Hi")]

opt :: Char -> Parser Char [Char]
opt c (x:xs) | x == c = [([c],xs)]
             | otherwise = [("", (x:xs))]
-- opt '3' "32455" = [("3","2455")]
-- opt '3' "2455" = [("","2455")]

int2 :: Parser Char [Char]
int2 (x:xs) = concatPair (((opt '-') `thens` int) (x:xs))
--int2 "12345" = [("12345",""),("1234","5"),("123","45"),("12","345"),("1","2345")]
--int2 "-12345" = [("-12345",""),("-1234","5"),("-123","45"),("-12","345"),("-1","2345")]
--int2 "-a12345" = []

--Start here
data Expr = Add Term Term | Sub Term Term | Ter Term deriving (Show, Eq, Ord)
data Term = Mul Power Power | Div Power Power | Pow Power deriving (Show, Eq, Ord)
data Power = Po Factor Factor | Fact Factor deriving (Show, Eq, Ord)
data Factor = Num Int | Exp Expr deriving (Show, Eq, Ord)

expr :: Parser Char Expr
expr = ((term `thenx` (literal '+') `thens` term) `using` plus) `alt` ((term `thenx` (literal '-') `thens` term) `using` minus) `alt` (term `using` Ter) 

term :: Parser Char Term
term = ((power  `thenx` (literal '*') `thens` power) `using` times) `alt` ((power  `thenx` (literal '/') `thens` power) `using` divide) `alt` (power `using` Pow)

power :: Parser Char Power
power = ((factor `thenx` (literal '^') `thens` factor) `using` exponentiate) `alt` (factor `using` Fact)

factor :: Parser Char Factor
factor = factorNum `alt` parenExpr

parse :: [Char] -> Expr
parse = fst . head . expr

bigExpr n = '0':concat['+':'(':show i|i <- [1..n]]++(take n (repeat ')'))

factorNum :: Parser Char Factor
factorNum = number `using` value

parenExpr :: Parser Char Factor
parenExpr = ((literal '(') `xthen` expr `thenx` (literal ')')) `using` Exp

value :: String -> Factor
value = Num . read

plus :: (Term,Term) -> Expr
plus (x,y) = Add x y

minus :: (Term,Term) -> Expr
minus (x,y) = Sub x y

times :: (Power,Power) -> Term
times (x,y) = Mul x y

divide :: (Power,Power) -> Term
divide (x,y) = Div x y

exponentiate :: (Factor,Factor) -> Power
exponentiate (x,y) = Po x y


--EVALUATOR
evaluate :: Expr -> Int
evaluate (Add x y) = (evaluate1 x) + (evaluate1 y)
evaluate (Sub x y) = (evaluate1 x) - (evaluate1 y)
evaluate (Ter x) = (evaluate1 x)

evaluate1 :: Term -> Int
evaluate1 (Mul x y) = (evaluate2 x) * (evaluate2 y)
evaluate1 (Div x y) = (evaluate2 x) `div` (evaluate2 y)
evaluate1 (Pow x) = (evaluate2 x)

evaluate2 :: Power -> Int
evaluate2 (Po x y) = (evaluate3 x) ^ (evaluate3 y)
evaluate2 (Fact x) = (evaluate3 x)

evaluate3 :: Factor -> Int
evaluate3 (Num x) = x
evaluate3 (Exp x) = evaluate x

evaluator :: [Char] -> Int
evaluator xs = evaluate (parse xs)

data RPN = SPush Int | SAdd | SSub | SMul | SDiv | SPow deriving (Show)
--Expression to [RPN]
toRPN :: [Char] -> [RPN]
toRPN xs = toRPN1 (parse xs)

toRPN1 :: Expr -> [RPN]
toRPN1 (Add x y) = (toRPN2 x) ++ (toRPN2 y) ++ [SAdd]
toRPN1 (Sub x y) = (toRPN2 x) ++ (toRPN2 y) ++ [SSub]
toRPN1 (Ter x) = (toRPN2 x)

toRPN2 :: Term -> [RPN]
toRPN2 (Mul x y) = (toRPN3 x) ++ (toRPN3 y) ++ [SMul]
toRPN2 (Div x y) = (toRPN3 x) ++ (toRPN3 y) ++ [SDiv]
toRPN2 (Pow x) = (toRPN3 x)

toRPN3 :: Power -> [RPN]
toRPN3 (Po x y) = (toRPN4 x) ++ (toRPN4 y) ++ [SPow]
toRPN3 (Fact x) = (toRPN4 x)

toRPN4 :: Factor -> [RPN]
toRPN4 (Num x) = [SPush x]
toRPN4 (Exp x) = toRPN1 x

--Interp
interp :: [RPN] -> Int
interp [SPush x] = x
interp ((SPush x):(SPush y):(SPush z):xs) = interp((SPush x):(interp1 ((SPush y):(SPush z):xs)))
interp ((SPush x):(SPush y):(SAdd):xs) = interp ((SPush (x+y)):xs)
interp ((SPush x):(SPush y):(SSub):xs) = interp ((SPush (x-y)):xs)
interp ((SPush x):(SPush y):(SMul):xs) = interp ((SPush (x*y)):xs)
interp ((SPush x):(SPush y):(SDiv):xs) = interp ((SPush (x`div`y)):xs)
interp ((SPush x):(SPush y):(SPow):xs) = interp ((SPush (x^y)):xs)
interp [] = 0 

interp1 :: [RPN] -> [RPN]
interp1 ((SPush x):(SPush y):(SPush z):xs) = (SPush x):(interp1((SPush y):(SPush z):xs))
interp1 ((SPush x):(SPush y):(SAdd):xs) = ((SPush (x+y)):xs)
interp1 ((SPush x):(SPush y):(SSub):xs) = ((SPush (x-y)):xs)
interp1 ((SPush x):(SPush y):(SMul):xs) = ((SPush (x*y)):xs)
interp1 ((SPush x):(SPush y):(SDiv):xs) = ((SPush (x`div`y)):xs)
interp1 ((SPush x):(SPush y):(SPow):xs) = ((SPush (x^y)):xs)

--Interp TRY #2
terp :: [RPN] -> [RPN] -> Int
terp [SPush x] [] = x
terp xs ((SPush z):ys) = terp ((SPush z):xs) ys
terp ((SPush x1):(SPush x2):xs) ((SAdd):ys) = terp ((SPush (x2+x1)):xs) ys
terp ((SPush x1):(SPush x2):xs) ((SSub):ys) = terp ((SPush (x2-x1)):xs) ys
terp ((SPush x1):(SPush x2):xs) ((SMul):ys) = terp ((SPush (x2*x1)):xs) ys
terp ((SPush x1):(SPush x2):xs) ((SDiv):ys) = terp ((SPush (x2`div`x1)):xs) ys
terp ((SPush x1):(SPush x2):xs) ((SPow):ys) = terp ((SPush ((x2)^(x1))):xs) ys


--Interp TRY #3
terp1 :: [Int]-> [RPN] -> Int
terp1 [x] [] = x
terp1 xs ((SPush z):ys) = terp1 (z:xs) ys
terp1 (x1:x2:xs) ((SAdd):ys) = terp1 ((x2+x1):xs) ys
terp1 (x1:x2:xs) ((SSub):ys) = terp1 ((x2-x1):xs) ys
terp1 (x1:x2:xs) ((SMul):ys) = terp1 ((x2*x1):xs) ys
terp1 (x1:x2:xs) ((SDiv):ys) = terp1 ((x2`div`x1):xs) ys
terp1 (x1:x2:xs) ((SPow):ys) = terp1 (((x2)^(x1)):xs) ys

final :: [Char] -> Int
final xs = interp (toRPN xs)
--final "2+(4-1)*3" = 11
--final "2+(4-1)" = 5
--final "(2*2)+(4-1)" = 7
--final "(2*2)+((4-1)^3)" = 31

final1 :: [Char] -> Int
final1 xs = terp [] (toRPN xs)

final2 :: [Char] -> Int
final2 xs = terp1 [] (toRPN xs)
