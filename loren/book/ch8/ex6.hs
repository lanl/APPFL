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

{-
expn = ((term `thenx` (literal '+') `thens` term) `using` plus) `alt` ((term `thenx` (literal '-') `thens` term) `using` minus) `alt` term

term = ((factor `thenx` (literal '*') `thens` factor) `using` times) `alt` ((factor `thenx` (literal '/') `thens` factor) `using` divide) `alt` factor

factor = (number `using` value) `alt` ((literal '(') `xthen` expn `thenx` (literal ')'))
-}

expr :: Parser Char Expr
expr = ((term `thenx` (literal '+') `thens` term) `using` plus) `alt` ((term `thenx` (literal '-') `thens` term) `using` minus) `alt` (term `using` Ter) 

term :: Parser Char Term
term = ((power  `thenx` (literal '*') `thens` power) `using` times) `alt` ((power  `thenx` (literal '/') `thens` power) `using` divide) `alt` (power `using` Pow)

power :: Parser Char Power
power = ((factor `thenx` (literal '^') `thens` factor) `using` exponentiate) `alt` (factor `using` Fact)

factor :: Parser Char Factor
factor = factorNum `alt` parenExpr

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

{-
factor1 :: Parser Char Factor
factor1 = number `using` value

factor2 :: Parser Char Factor
factor2 = ((literal '(') `xthen` number `thenx` (literal ')')) `using` value

term1 :: Parser Char Term
term1 = (factor1  `thenx` (literal '*') `thens` factor1) `using` times

expr1 :: Parser Char Expr
expr1 = (term1 `thenx` (literal '+') `thens` term1) `using` plus 

term2 :: Parser Char Term
term2 = ((factor1  `thenx` (literal '*') `thens` factor1) `using` times) `alt` ((factor1  `thenx` (literal '/') `thens` factor1) `using` divide)

expr2 :: Parser Char Expr
expr2 = ((term2 `thenx` (literal '+') `thens` term2) `using` plus) `alt` ((term2 `thenx` (literal '-') `thens` term2) `using` plus)

factorNum :: Parser Char Factor
factorNum = number `using` value

factor :: Parser Char Factor
factor = factorNum `alt` parenExpr

parenExpr :: Parser Char Factor
parenExpr = ((literal '(') `xthen` expr `thenx` (literal ')')) `using` Exp

term :: Parser Char Term
term = ((factor  `thenx` (literal '*') `thens` factor) `using` times) `alt` ((factor  `thenx` (literal '/') `thens` factor) `using` divide) `alt` (factor `using` Fact)

expr :: Parser Char Expr
expr = ((term `thenx` (literal '+') `thens` term) `using` plus) `alt` ((term `thenx` (literal '-') `thens` term) `using` minus) `alt` (term `using` Ter)
-}
{-
data Expr = Add Term Term | Sub Term Term | Ter Term deriving (Show, Eq, Ord)
data Term = Mul Factor Factor | Div Factor Factor | Fact Factor deriving (Show, Eq, Ord)
data Factor = Num Int | Exp Expr deriving (Show, Eq, Ord)
-}
