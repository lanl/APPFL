import Data.Char
type Parser a b = [a] -> [(b,[a])]

succeed :: b -> Parser a b
succeed v inp = [(v, inp)]

failed :: Parser a b
failed inp = []

satisfy :: (Char -> Bool) -> Parser Char Char
satisfy p []     = fail []
satisfy p (x:xs) | p x       = succeed x xs
                 | otherwise = fail xs
literal :: Char -> Parser Char Char
literal x = satisfy (==x)

alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) inp = p1 inp ++ p2 inp

thens :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `thens` p2) inp = [((v1, v2), out2) | (v1, out1) <- p1 inp, (v2,out2) <- p2 out1]  

using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) inp = [(f v, out) | (v,out) <- p inp]

cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs

many :: Parser a b -> Parser a [b]
many p = ((p `thens` many p) `using` cons)`alt` (succeed [])

some :: Parser a b -> Parser a [b]
some p = (p `thens` many p) `using` cons

number :: Parser Char [Char]
number = some (satisfy isDigit)
              
word :: Parser Char [Char]
word = some (satisfy isAlpha)

{-
string :: [Char] -> Parser Char [Char]
string [] = succeed []
string (x:xs) = ((literal x) `thens` (string xs)) `using` cons
-}

xthen :: Parser a b -> Parser a c -> Parser a c
p1 `xthen` p2 = (p1 `thens` p2) `using` snd 

thenx :: Parser a b -> Parser a c -> Parser a b
p1 `thenx` p2 = (p1 `thens` p2) `using` fst

returns :: Parser a b -> c -> Parser a c
p `returns` v = p `using` (const v)
                 

expn :: term+term || term -term || term
term :: factor*factor|factor/factor|factor
factor :: digit^ | (expn)

expn = (term `then` literal `*` `then` factor `using` plus) `alt`
       (term `then` literal `-` `xthen` term) `using` minus) `alt`
       term

term = (factor `then` literal `*` `xthen` factor `using` times) `alt`
       (factor `then` literal `/` `xthen` factor `using` divide) `alt`
       factor

factor = (number `using` value) `alt`
         (literal `(` `xthen` expn `thenx` literal `)`)

value xs = Num (numval xs)
plus (x,y) = x `Add` y
minus (x,y) = x `Sub` y
times (x,y) = x `Mul` y 
divide (x,y) = x `Div` y



