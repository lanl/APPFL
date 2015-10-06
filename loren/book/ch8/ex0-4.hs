import Data.Char 
type Token = (Tag,[Char])
type Parser a b = [a] -> [(b,[a])]
data Tag = Ident | Number | Symbol | Junk deriving (Show, Eq, Ord)
type Pos a = (a,(Int,Int))



succeed :: b -> Parser a b
succeed v inp = [(v, inp)]
-- succeed 23423 "Success!!!   " = [(23423,"Success!!!   ")]
failed :: Parser a b
failed inp = []
--failed "anything" = []
satisfy :: (a -> Bool) -> Parser (Pos a) a
satisfy p []     = fail []
satisfy p (x:xs) | p b       = succeed b xs
                 | otherwise = failed xs
                   where (b,(r,c)) = x
--satisfy isDigit [('2',(0,1)),('3',(0,2)),('4',(0,3))] = [('2',[('3',(0,2)),('4',(0,3))])]
literal :: Eq a => a -> Parser (Pos a) a
literal x = satisfy (==x)
-- literal 'a' [('a',(0,1)),('a',(0,2)),('a',(0,3))] =[('a',[('a',(0,2)),('a',(0,3))])]
alt :: Parser a b -> Parser a b -> Parser a b
(p1 `alt` p2) inp = p1 inp ++ p2 inp
--((literal 'b') `alt` (literal 'a')) [('a', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [('a',[('a',(0,2)),('2',(0,3))])]
thens :: Parser a b -> Parser a c -> Parser a (b,c)
(p1 `thens` p2) inp = [((v1, v2), out2) | (v1, out1) <- p1 inp, (v2,out2) <- p2 out1]  
--((literal 'a') `thens` (literal 'a')) [('a', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [(('a','a'),[('2',(0,3))])]
using :: Parser a b -> (b -> c) -> Parser a c
(p `using` f) inp = [(f v, out) | (v,out) <- p inp]
--((literal 'a') `using` toUpper ) [('a', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [('A',[('a',(0,2)),('2',(0,3))])]
cons :: (a,[a]) -> [a]
cons (x,xs) = x:xs
--cons (2,[2,2,2]) = [2,2,2,2]
many :: Parser a b -> Parser a [b]
many p = ((p `thens` many p) `using` cons)`alt` (succeed [])
--many (literal 'a') [('a', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [("aa",[('2',(0,3))]),("a",[('a',(0,2)),('2',(0,3))]),("",[('a',(0,1)),('a',(0,2)),('2',(0,3))])]
--many (literal 'a') [('b', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [("",[('b',(0,1)),('a',(0,2)),('2',(0,3))])]
some :: Parser a b -> Parser a [b]
some p = (p `thens` many p) `using` cons
--some (literal 'a') [('a', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] =[("aa",[('2',(0,3))]),("a",[('a',(0,2)),('2',(0,3))])]
--some (literal 'a') [('b', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = []
number :: Parser (Pos Char) [Char]
number = some (satisfy isDigit)
--number [('1', (0, 1)), ('1', (0, 2)), ('2', (0, 3))] = [("112",[]),("11",[('2',(0,3))]),("1",[('1',(0,2)),('2',(0,3))])]
--number [('1', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [("1",[('a',(0,2)),('2',(0,3))])]
word :: Parser (Pos Char) [Char]
word = some (satisfy isAlpha)
--word [('a', (0, 1)), ('a', (0, 2)), ('2', (0, 3))] = [("aa",[('2',(0,3))]),("a",[('a',(0,2)),('2',(0,3))])]
string :: Eq a => [a] -> Parser (Pos a) [a]
string [] = succeed []
string (x:xs) = ((literal x) `thens` (string xs)) `using` cons
--string "davis" [('d', (0, 1)), ('a', (0, 2)), ('v', (0, 3)),('i',(0,4)),('s',(0,5)),('z',(0,6))] = [("davis",[('z',(0,6))])]
xthen :: Parser a b -> Parser a c -> Parser a c
p1 `xthen` p2 = (p1 `thens` p2) `using` snd 
--((string "davis") `xthen` (literal 'z')) [('d', (0, 1)), ('a', (0, 2)), ('v', (0, 3)),('i',(0,4)),('s',(0,5)),('z',(0,6))] = [('z',[])]
thenx :: Parser a b -> Parser a c -> Parser a b
p1 `thenx` p2 = (p1 `thens` p2) `using` fst
--((string "davis") `thenx` (literal 'z')) [('d', (0, 1)), ('a', (0, 2)), ('v', (0, 3)),('i',(0,4)),('s',(0,5)),('z',(0,6))] = [("davis",[])]
returns :: Parser a b -> c -> Parser a c
p `returns` v = p `using` (const v)
--((string "davis") `returns` 3) [('d', (0, 1)), ('a', (0, 2)), ('v', (0, 3)),('i',(0,4)),('s',(0,5)),('z',(0,6))] = [(3,[('z',(0,6))])]

--Section 3 

any1 :: (a -> Parser c b) -> [a] -> Parser c b
any1 p = foldr (alt . p) failed

nibble p = white `xthen` p `thenx` white
           where white = many (any1 literal " \t\n")
-- nibble word [(' ',(0,0)),('a', (0, 1)),(' ',(0,2)), ('a', (0, 3)), ('2', (0, 4)),(' ',(0,5))] = [("a",[('a',(0,3)),('2',(0,4)),(' ',(0,5))]),("a",[(' ',(0,2)),('a',(0,3)),('2',(0,4)),(' ',(0,5))])]
symbol :: [Char] -> Parser (Pos Char) [Char]
symbol = nibble.string
--(symbol "hi") [('h',(0,0)),('i',(0,1)),(' ',(0,2))] = [("hi",[]),("hi",[(' ',(0,2))])]
offside :: Parser (Pos x) y -> Parser (Pos x) y
offside p inp = [(v,inpOFF) | (v,[]) <- p inpON]
                where
                  inpON = takeWhile (onside (head inp)) inp
                  inpOFF = drop (length inpON) inp     
                  onside (a,(r,c)) (b,(r',c')) = r' >= r && c' >= c
--Section 4.2
prelex :: [Char] -> [Pos Char]
prelex = pl (0,0)
         where 
           pl (r,c) [] = []
           pl (r,c) (x:xs) | x=='\t' = (x,(r,c)) : pl (r,tab c) xs
                           | x=='\n' = (x,(r,c)) : pl (r+1,0) xs
                           | otherwise = (x,(r,c)) : pl (r,c+1) xs
           tab c = ((c `div` 8)+1)*8
--prelex "loren" = [('l',(0,0)),('o',(0,1)),('r',(0,2)),('e',(0,3)),('n',(0,4))]
--Section 4.3
tok :: ([(t, (t4, t5))] -> [(t3, t1)])-> t2 -> [(t, (t4, t5))] -> [(((t2, t3), (t4, t5)), t1)]
(p `tok` t) inp = [(((t,xs),(r,c)),out)|(xs,out) <- p inp]
                  where (x,(r,c)) = head inp

lex1 :: [([(t, (t3, t4))] -> [(t2, [(t, (t3, t4))])], t1)] -> Parser (t, (t3, t4)) [((t1, t2), (t3, t4))]
lex1 = many.(foldr op failed)
      where (p,t) `op` xs = (p `tok` t) `alt` xs

lexer :: Parser (Pos Char) [Pos Token]
lexer = lex1 [(some (any1 literal " \t\n"), Junk),
             (string "where"            , Symbol),
             (word                      , Ident),
             (number                    , Number),
             (any1 string ["(",")","="]  , Symbol)]
--Example: lexer (prelex "Loren")

--Section 4.4

strip :: [Pos Token] -> [Pos Token]
strip = filter ((/= Junk ).fst.fst)
--strip[((Junk,"Loren"),(0,0)),((Ident,"Los Alamos"),(0,1))] = [((Ident,"Los Alamos"),(0,1))]
--Example of type [Pos Token] = [((Junk,"Loren"),(0,0)),((Ident,"Los Alamos"),(0,1))]

--Section 4.5
kind :: Tag -> Parser (Pos Token) [Char]
kind t = (satisfy ((==t).fst)) `using` snd
--kind Junk  [((Junk,"Loren"),(0,0)),((Ident,"Los Alamos"),(0,1)),((Junk,"Hi"),(0,2))] = [("Loren",[((Ident,"Los Alamos"),(0,1)),((Junk,"Hi"),(0,2))])]
lit :: [Char] -> Parser (Pos Token) [Char]
lit xs = literal (Symbol, xs) `using` snd
--lit "Loren"[((Symbol,"Loren"),(0,0)),((Ident,"Los Alamos"),(0,1)),((Junk,"Hi"),(0,2))] = [("Loren",[((Ident,"Los Alamos"),(0,1)),((Junk,"Hi"),(0,2))])]
opt :: Parser a b -> b -> Parser a b
opt p v = p `alt` (succeed v)

numval :: (Read a) => (Num a) => String -> a 
numval cs = read cs

data Script = Scr [Def] deriving (Show, Eq, Ord)
data Def = Defin Var [Var] Expn deriving (Show, Eq, Ord)
data Expn = V Var | Numb Int | Apply Expn Expn | Where Expn [Def] deriving (Show, Eq, Ord)
type Var = [Char] 

--Main Stuff
prog = many defn `using` Scr
defn = (some (kind Ident) `thens` ((lit "=") `xthen` offside body)) `using` defnFN
body = (expr `thens` ((lit "where" `xthen` some defn) `opt` [])) `using` bodyFN

expr :: Parser (Pos Token) Expn
expr = some prim `using` (foldl1 Apply)

prim :: Parser (Pos Token) Expn
prim = (kind Ident `using` V) `alt` (kind Number `using` numFN) `alt` (lit "(" `xthen` expr `thenx` lit ")")

--Helper Functions
defnFN :: ([Var], Expn) -> Def
defnFN (x:xs,e) = Defin x xs e

numFN :: String -> Expn
numFN xs = Numb (numval xs)

bodyFN :: (Expn, [Def]) -> Expn
bodyFN (e,[]) = e
bodyFN (e,d:ds) = Where e (d:ds)

parse :: [Char] -> Script
parse = fst.head.prog.strip.fst.head.lexer.prelex

--SECTION 5
type Parser' a b = [a] -> Maybe' [(b,[a])]
data Maybe' a = Fail' [Char] | Error' [Char] | OK' a deriving (Show)

nofail :: Parser' a b -> Parser' a b
(nofail p) inp = f (p inp)
                 where 
                   f (Fail' xs) = Error' xs
                   f other = other
--nofail (succeed1 "Hello") "World" = OK' [("Hello","World")]

succeed1 :: b -> Parser' a b
succeed1 v inp = OK' [(v, inp)]
--succeed1 "Loren" "hi" = OK' [("Loren","hi")]
failed1 :: Parser' a b
failed1 inp = Fail' []
--failed1 "Histuff" = Fail' ""
satisfy1 :: (a -> Bool) -> Parser' a a
satisfy1 p []     = Fail' []
satisfy1 p (x:xs) | p x       = succeed1 x xs
                  | otherwise = failed1 xs
--satisfy1 isUpper "A" = OK' [('A',"")]
--satisfy1 isUpper "a" = Fail' ""
--satisfy1 isUpper "b" = Fail' ""
alt1 :: Parser' a b -> Parser' a b -> Parser' a b
(p1 `alt1` p2) inp = f(p1 inp, p2 inp)
                     where
                       f(Fail' xs, Fail' ys) = Fail' (xs ++ ys)
                       f(Fail' xs, OK' ys) = OK' ys
                       f(Error' xs, _) = Error' xs
                       f(_, Error' ys) = Error' ys
                       f(OK' xs, OK' ys) = OK' (xs++ys)
                       f(OK' xs, Fail' ys) = OK' xs 
--((satisfy1 isUpper) `alt1` (satisfy1 isDigit)) "Loren" = OK' [('L',"oren")]
--((satisfy1 isDigit) `alt1`((satisfy1 isDigit) `alt1`(nofail (satisfy1 isDigit)))) "I2345" = Error' ""
--((satisfy1 isDigit) `alt1`((satisfy1 isDigit) `alt1`(nofail (satisfy1 isDigit)))) "2345" = OK' [('2',"345"),('2',"345"),('2',"345")]
using1 :: Parser' a b -> (b -> c) -> Parser' a c
(p `using1` f) inp = g (p inp)
                     where 
                       g (Fail' xs) = Fail' xs
                       g (Error' xs) = Error' xs
                       g (OK' xs) = OK' [(f v, out) | (v,out) <- xs]
--((satisfy1 isUpper) `using1` (toLower)) "LOREN" = OK' [('l',"OREN")]
into :: Parser' a b -> (b -> Parser' a c) -> Parser' a c
(p `into` f) inp = g (p inp)
                   where 
                     g (OK' [(v,inp')]) = f v inp'
                     g (Fail' xs) = Fail' xs
                     g (Error' xs) = Error' xs
--using2 :: Parser' a b -> (b -> c) -> Parser' a c
using2 :: Parser' a b -> (b -> c) -> Parser' a c
p `using2` f = p `into` (\v -> (succeed1 (f v)))
--((satisfy1 isUpper ) `using2` (toLower)) "Loren" = OK' [('l',"oren")]
then2 :: Parser' a t -> Parser' a t1 -> Parser' a (t, t1)
p `then2` q = p `into` (\x -> (q `using2` (\w -> (x,w))))
--((satisfy1 isUpper) `then2` (satisfy1 isLower)) "Loren" = OK' [(('L','o'),"ren")]
--((satisfy1 isUpper) `then2` (satisfy1 isLower)) "LLoren" = Fail' ""
helper :: Parser' a t -> Parser' a t1 -> Parser' a t1 -> ((t, t1) -> c) -> Parser' a c
helper p q r h = (p `then2` (q `alt1` r)) `using1` h
--helper (satisfy1 isUpper) (satisfy1 isUpper) (satisfy1 isLower) (helping) "Loren" = OK' [('O',"ren")]
--helper (satisfy1 isUpper) (satisfy1 isUpper) (satisfy1 isLower) (helping) "LLoren" = OK' [('L',"oren")]
--helper (satisfy1 isUpper) (satisfy1 isUpper) (satisfy1 isLower) (helping) "LLLoren" = OK' [('L',"Loren")]
--helper (satisfy1 isUpper) (satisfy1 isUpper) (satisfy1 isLower) (helping) "loren" = Fail' ""
helping :: (Char,Char)-> Char
helping (x,y) = (toUpper y)
--helping ('f','s') = 'S'
parser2 :: Parser' a b -> Parser' a b1-> (b -> b1 -> c) -> Parser' a b2 -> (b -> b2 -> c) -> Parser' a c
parser2 p q f r g = p `into`(\v -> ((q `using2` f v) `alt1` (r `using2` g v)))
--parser2 (satisfy1 isUpper) (satisfy1 isLower) (help) (satisfy1 isUpper) (help) "Loern" = OK' [('L',"ern")]
--parser2 (satisfy1 isUpper) (satisfy1 isLower) (help2) (satisfy1 isUpper) (help2) "Loren" = OK' [('o',"ren")]
help:: Char -> Char-> Char
help a b = a
help2:: Char -> Char-> Char
help2 a b = b
compose :: (b1 -> Parser' a b) -> (b -> Parser' a c) -> b1 -> Parser' a c
(p `compose` q) v = (succeed1 v) `into` p `into` q
