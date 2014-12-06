-- Tautology parser w/ lexer

--Tautology checker example from section 10.4 of Programming in Haskell,
--Graham Hutton, Cambridge University Press, 2007.

import Data.Char

--Tautology checker
-----------------

data Prop                     =  Const Bool
                               |  Var Char
                               |  Not Prop
                               |  And Prop Prop
                               |  Imply Prop Prop
                               |  Or Prop Prop
                               |  Equ Prop Prop
                               deriving (Show)

type Subst                    =  Assoc Char Bool

type Assoc k v                =  [(k,v)]

find                          :: Eq k => k -> Assoc k v -> v
find k t                      =  head [v | (k',v) <- t, k == k']

eval                          :: Subst -> Prop -> Bool
eval _ (Const b)              =  b
eval s (Var x)                =  find x s
eval s (Not p)                =  not (eval s p)
eval s (And p q)              =  eval s p && eval s q
eval s (Imply p q)            =  eval s p <= eval s q
eval s (Or p q)               =  eval s p || eval s q
eval s (Equ p q)              =  eval s p == eval s q

vars                          :: Prop -> [Char]
vars (Const _)                =  []
vars (Var x)                  =  [x]
vars (Not p)                  =  vars p
vars (And p q)                =  vars p ++ vars q
vars (Imply p q)              =  vars p ++ vars q
vars (Or p q)                 =  vars p ++ vars q
vars (Equ p q)                =  vars p ++ vars q

bools                         :: Int -> [[Bool]]
bools 0                       =  [[]]
bools n                       =  map (False:) bss ++ map (True:) bss
                                  where bss = bools (n-1)

rmdups                        :: Eq a => [a] -> [a]
rmdups []                     =  []
rmdups (x:xs)                 =  x : rmdups (filter (/= x) xs)

substs                        :: Prop -> [Subst]
substs p                      =  map (zip vs) (bools (length vs))
                                  where vs = rmdups (vars p)

isTaut                        :: Prop -> Bool
isTaut p                      =  and [eval s p | s <- substs p]

--Parser
-----------

-- 2 Parsing Using Combinators

type Parser b a = [b] -> [(a,[b])]

-- 2.1 Primitive parsers

succeed :: a -> Parser b a
succeed v inp = [(v,inp)]

failure :: Parser b a
failure inp = []

satisfy :: (b->Bool) -> Parser b b
satisfy p [] = failure []
satisfy p (x:xs)
    | p x = succeed x xs
    | otherwise = failure xs

literal :: Eq b => b -> Parser b b
literal x = satisfy (==x)

-- 2.2 Combinators

alt :: Parser b a -> Parser b a -> Parser b a
(p1 `alt` p2) inp = p1 inp ++ p2 inp

-- then
sequ :: Parser b a -> Parser b c -> Parser b (a,c)
(p1 `sequ` p2) inp = [((v1,v2),out2) | (v1,out1) <- p1 inp, (v2,out2) <- p2 out1]

-- 2.3 Manipulating values

using :: Parser b a -> (a -> c) -> Parser b c
(p `using` f) inp = [(f v,out) | (v,out) <- p inp]

cons :: (a, [a]) -> [a]
cons (x,xs) = x:xs

many :: Parser b a -> Parser b [a]
many p = ((p `sequ` many p) `using` cons ) `alt` (succeed [])

some :: Parser b a -> Parser b [a]
some p = (p `sequ` many p) `using` cons

word :: Parser Char [Char]
word = some (satisfy isAlpha)

string :: Eq b => [b] -> Parser b [b]
string [] = succeed []
string (x:xs) = (literal x `sequ` string xs) `using` cons

xthen :: Parser b a -> Parser b c -> Parser b c
p1 `xthen` p2 = (p1 `sequ` p2) `using` snd

thenx :: Parser b a -> Parser b c -> Parser b a
p1 `thenx` p2 = (p1 `sequ` p2) `using` fst

-- 3.1 Free-format input

any' :: (b -> Parser a c) -> [b] -> Parser a c
any' p = foldr (alt . p) failure

-- Scanning

strip :: [Token] -> [Token]
strip = filter ((/=Junk).fst)

-- Lexical analysis

data Tag = Ident | Symbol | Junk deriving (Eq,Show)

type Token = (Tag,[Char])

tok :: Parser Char [Char] -> Tag -> Parser Char Token
(p `tok` t) inp = [((t,xs),out) | (xs,out) <- p inp]

lexit :: [(Parser Char [Char], Tag)] -> Parser Char [Token]
lexit = many . (foldr op failure)
        where (p,t) `op` xs = (p `tok` t) `alt` xs

lexer :: Parser Char [Token]
lexer = lexit [(some (any' literal " \t\n"), Junk),
               (any' string ["=>","->","||","&&","=="], Symbol),
               (any' string ["(",")","=","|","^","&","!","-"], Symbol),
               (word, Ident)]

-- Syntax analysis

kind :: Tag -> Parser Token [Char] 
kind t = (satisfy ((==t).fst)) `using` snd

lit :: [Char] -> Parser Token [Char]
lit xs = literal (Symbol,xs) `using` snd

char :: Parser Char Char
char = satisfy isAlpha

prop :: Parser Token Prop
prop =  ((term `thenx` lit "=" `sequ` term) `using` equ) `alt`
        ((term `thenx` lit "==" `sequ` term) `using` equ) `alt`
        term 

term :: Parser Token Prop
term = ((factor `thenx` lit "&" `sequ` factor) `using` conj) `alt`
       ((factor `thenx` lit "^" `sequ` factor) `using` conj) `alt`
       ((factor `thenx` lit "&&" `sequ` factor) `using` conj) `alt`
       ((factor `thenx` lit "=>" `sequ` factor) `using` imply) `alt`
       ((factor `thenx` lit "->" `sequ` factor) `using` imply) `alt`
       ((factor `thenx` lit "|" `sequ` factor) `using` disj) `alt`
       ((factor `thenx` lit "||" `sequ` factor) `using` disj) `alt`
        factor

factor :: Parser Token Prop
factor = (kind Ident `using` var) `alt`
          (lit "(" `xthen` prop `thenx` lit ")") `alt`
          ((lit "!" `xthen` factor) `using` neg) `alt`
          ((lit "-" `xthen` factor) `using` neg)

var :: [Char] -> Prop
var "T" = Const True
var "F" = Const False
var "t" = Const True
var "f" = Const False
var [x] = Var x
var _   = error("bad variable")

neg :: Prop -> Prop
neg x = Not x

conj :: (Prop, Prop) -> Prop
conj (x,y) = And x y

imply :: (Prop, Prop) -> Prop
imply (x,y) = Imply x y

disj :: (Prop, Prop) -> Prop
disj (x,y) = Or x y

equ :: (Prop, Prop) -> Prop
equ (x,y) = Equ x y

parse = fst.head.prop.strip.fst.head.lexer

check = isTaut.parse

p1' = "A & !A"
p1 = "A ^ !A"
p2' = "(A && B) => A"
p2 = "(A ^ B) => A"
p3 = "A => (A ^ B)"
p4 = "(A ^ (A => B)) => B"
p4' = "(A ^ (A => B)) -> B"

p5 = "!A & !B = !(A || B)"
p5' = "-A ^ -B = -(A | B)"

p6 = "T = -F"
p6' = "t = !f"

