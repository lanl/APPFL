--Scanner
import Data.Char
import Data.List

data Token = Ident [Char] | Lamb | Const Con | LPar | RPar deriving (Show, Eq, Read)
data Con = Integ Int | Trues | Falses | Succ | Sqr | Add | Sub | Mul deriving (Show,Eq,Read)


scanner :: [Char] -> [Token]
scanner [] = [] 
scanner (x:xs) | x == ' ' = scanner xs
               | x == '\\' = Lamb:(scanner xs)
               | x == ')' = LPar:(scanner xs)
               | x == '(' = RPar:(scanner xs)
               | otherwise = (fst y):(scanner (snd y))
                             where y = chopOff (x:xs) 
                
chopOff :: [Char] -> (Token,[Char])
chopOff [] = error "Bad" 
chopOff (x:xs) | isDigit x = (readNum (fst (head y)), drop (length (fst (head y))) (x:xs))   
               | isAlpha x = (getCon (fst (head z)), drop (length (fst (head z))) (x:xs))
               | otherwise = error "Bad"
               where y = number (x:xs)
                     z = word (x:xs)

getCon :: [Char] -> Token
getCon x | x == "sqr" = Const Sqr
         | x == "true" = Const Trues
         | x == "false" = Const Falses
         | x == "succ" = Const Succ
         | x == "add" = Const Add
         | x == "sub" = Const Sub
         | x == "mul" = Const Mul
         | otherwise = Ident x
               
readNum :: [Char] -> Token
readNum xs = Const (Integ (read xs)) 
--END Scanner



--Parsing Stuff
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

word :: Parser Char [Char]
word = some (satisfy isAlpha)

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
