import Data.Char
type Parser a b = [a] -> [(b,[a])] 
type Token = (Tag,[Char])
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

--Start Here
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

helper :: Parser (Pos Token) ([[Char]],[Char])
helper = (some (kind Ident) `thens` (lit "="))
