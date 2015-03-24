

type Parser a b = [a] -> [(b,[a])]

-- Always succeeds
succeedp :: t1 -> Parser t t1
succeedp v inp = [(v, inp)]

-- Always fails
failp :: Parser b c
failp inp = []


-- If list is not empty, and p of the first item is true, then succeed. Otherwise fail.
satisfyp :: (b -> Bool) -> Parser b b
satisfyp p [] = failp []
satisfyp p (x:xs) | p x		= succeedp x xs
	  	  | otherwise	= failp xs
-- satisfyp (>2) [4,5,6]     returns    [(4,[5,6])]


-- If first item of list ==x then succeed. Otherwise fail.
literalp :: Eq b => b -> Parser b b
literalp x = satisfyp (==x)
-- literalp 2 [2,3,4]    returns   [(2,[3,4])]


-- applies p1 and p2 to inp and returns the list of results
altp :: Parser a b -> Parser a b -> Parser a b
(p1 `altp` p2) inp = p1 inp ++ p2 inp
-- ((succeedp 5) `altp` (succeedp 4)) (succeedp 3 [])    returns    [(5,[(3,[])]),(4,[(3,[])])]


-- returns tuple of results to p1 and p2 inside tuple with inp
thenp :: Parser t1 t2 -> Parser t1 t3 -> Parser t1 (t2, t3)
(p1 `thenp` p2) inp = [((v1, v2),out2) | (v1, out1) <- p1 inp, (v2, out2) <- p2 out1]
-- ((succeedp 5) `thenp` (succeedp 4)) (succeedp 3 [])     returns     [((5,4),[(3,[])])]


-- returns results of p inp with f of p inp at the begining
usingp :: Parser t1 t2 -> (t2 -> t3) -> Parser t1 t3
(p `usingp` f) inp = [(f v,out) | (v,out) <- p inp]
-- ((failp) `usingp` cons) (succeedp 3 [3,4,5])		   returns    []
-- ((succeedp 2) `usingp` (+6)) (succeedp 3 [3,4,5])       returns    [(8,[(3,[3,4,5])])]


-- functions similar to BNF*  returns list of results inside tuple with remaining input inside list. Each other item in the list removes the end from the results list
manyp :: Parser a b -> Parser a [b]
manyp p = ((p `thenp` manyp p) `usingp` cons) `altp` (succeedp [])
-- (manyp (succeedp 2)) []        doesn't seem to stop
-- manyp (literalp 'a') "aaab"    returns      [("aaa","b"),("aa","ab"),("a","aab"),("","aaab")]

maybep :: Parser a t2 -> Parser a (Maybe t2)
maybep p = (p `usingp` Just) `altp` (succeedp Nothing)

-- Functions as :'s operator. Takes fst and appends it to snd.
cons (x,xs) = x:xs
-- cons (2,[3])     returns   [2,3]


-- appears to be similar to manyp except it doesn't return a tuple with an empty result
somep :: Parser a b -> Parser a [b]
somep p = (p `thenp` manyp p) `usingp` cons
-- somep (literalp 'a') "aaab"     returns     [("aaa","b"),("aa","ab"),("a","aab")]


-- returns numbers
numberp :: Parser Char [Char]
numberp = somep (satisfyp digit)
       	 where digit x = '0' <= x && x <= '9'
-- numberp "123a"    returns   [("123","a"),("12","3a"),("1","23a")]


-- returns letters
wordp :: Parser Char [Char]
wordp = somep (satisfyp letter)
       where letter x = ('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z')
-- wordp "ab123"     returns    [("ab","123"),("a","b123")]


-- checks if first string is an element of second and returns second string with first removed
stringp :: Eq a => [a] -> Parser a [a]
stringp [] = succeedp []
stringp (x:xs) = (literalp x `thenp` stringp xs) `usingp` cons
-- stringp "ab" "abcdef"    returns    [("ab","cdef")]


-- returns v followed by inp
-- it appears that returnp is not used anywhere else.
returnp :: Parser t1 t2 -> t3 -> Parser t1 t3
p `returnp` v = p `usingp` (const v)
  	      	where const x y = x
-- ((succeedp 3) `returnp` (6)) (succeedp 2 [2,3,4])     returns    [(6,[(2,[2,3,4])])]


-- works like return but uses p2
xthenp :: Parser t1 t2 -> Parser t1 t3 -> Parser t1 t3
p1 `xthenp` p2 = (p1 `thenp` p2) `usingp` snd
-- ((failp) `xthenp` (failp)) []      returns    []
-- ((succeedp 7) `xthenp` (succeedp 1)) (succeedp 1 [1,2,3])     returns    [(1,[(1,[1,2,3])])]


-- works like return
thenxp :: Parser t1 t3 -> Parser t1 t2 -> Parser t1 t3
p1 `thenxp` p2 = (p1 `thenp` p2) `usingp` fst
-- ((succeedp 7) `thenxp` (succeedp 1)) (succeedp 1 [1,2,3])     returns    [(7,[(1,[1,2,3])])]





--data Expt = Num Int | Add Expt Expt | Sub Expt Expt | Mul Expt Expt | Div Expt Expt deriving(Show)

{-value xs = Num (read xs)
plus (x,y) = x `Add` y
minus (x,y) = x `Sub` y
times (x,y) = x `Mul` y
divide (x,y) = x `Div` y-}

value :: [Char] -> Int
value xs = read xs
plus (x,y) = x + y
minus (x,y) = x - y
times :: (Int, Int) -> Int
times (x,y) = x * y
divide (x,y) = x `div` y

-- Removes white space
nibble :: Parser Char t3 -> Parser Char t3
nibble p = white `xthenp` p `thenxp` white
       	   where white = manyp (anyp literalp " \t\n")


anyp :: (a -> Parser a1 b) -> [a] -> Parser a1 b
anyp p = foldr (altp.p) failp
-- (anyp literalp "a") "abcd"     returns     [('a',"bcd")]


symbolp :: [Char] -> Parser Char [Char]
symbolp = nibble.stringp
-- (symbolp "hi") "      hi abc"     returns    [("hi","abc"),("hi"," abc")]

-- My code
type Token = (Tag, [Char])
data Tag = Number | Ident | Junk  | Symbol deriving(Show, Eq)


tokp :: Parser Char [Char] -> Tag -> Parser Char Token
(p `tokp` t) inp = [((t,xs),out) | (xs,out) <- p inp]
   	    	  where x = hd inp
-- (stringp "where" `tokp` Symbol) "where are you"     returns    [((Symbol,"where")," are you")]


hd (a:x) = a
hd [] = error "hd []"


lexp :: [(Parser Char [Char], Tag)] -> Parser Char [Token]
lexp = manyp . (foldr op failp)
       where (p,t) `op` xs = (p `tokp` t) `altp` xs


lexerp :: Parser Char [Token]
lexerp = lexp [(somep (anyp literalp " \t\n"), Junk),
               (stringp "if" ,Symbol),
               (stringp "then" ,Symbol),
               (stringp "else" ,Symbol),
               (stringp "let" ,Symbol),
               (stringp "in" ,Symbol),
               (wordp, Ident),
               (numberp, Number),
               (anyp stringp ["==","/=","<=",">=","->","<",">",";","\\","(",")","+","-","*","/","^","="], Symbol)
              ]


--Removes items labeled Junk
stripp :: [Token] -> [Token]
stripp = filter ((/=Junk) . fst)
-- stripp [(Symbol,"where"),(Junk,"blahblah")]    returns    [(Symbol,"where")]


kindp :: Eq b => b -> Parser (b, t3) t3
kindp t = (satisfyp ((==t) . fst)) `usingp` snd


litp :: [Char] -> Parser Token [Char]
litp xs = literalp (Symbol, xs) `usingp` snd


optp :: Parser a b -> b -> Parser a b
p `optp` v = p `altp` (succeedp v)


-- Parser

data Expr = Letx [([Char], Expr)] Expr |
            Ifx Expr Expr Expr |
            Lambda [Char] Expr |
            Binop [Char] Expr Expr | 
            Unop [Char] Expr |
            App Expr Expr |
            Num Int |
            List [Expr] |
            Var [Char]
              deriving(Eq,Show)

ifsym     = litp "if"
thensym   = litp "then"
elsesym   = litp "else"
letsym    = litp "let"
eqsym     = litp "=="
nesym     = litp "/="
lesym     = litp "<="
ltsym     = litp "<"
gesym     = litp ">="
gtsym     = litp ">"
bindsym   = litp "="
insym     = litp "in"
lparensym = litp "("
rparensym = litp ")"
plussym   = litp "+"
minussym  = litp "-"
timessym  = litp "*"
divsym    = litp "/"
expsym    = litp "^"
lambdasym = litp "\\"
dotsym    = litp "->"
semisym   = litp ";"

someident = somep (kindp Ident)

number   = kindp Number `usingp` numFN
variable = kindp Ident `usingp` varFN
varFN xs = Var xs
numFN xs = Num (read xs)

topexpr :: Parser (Tag, [Char]) Expr
topexpr = ifexpr `altp`
          letexpr `altp`
          lambdaexpr `altp` 
          compexpr

ifexpr = (ifsym `xthenp` (topexpr `thenp` (thensym `xthenp` (topexpr `thenp` (elsesym `xthenp` topexpr))))) `usingp`
         \(fs,(f,p)) -> Ifx fs f p

lambdaexpr = (lambdasym `xthenp` (kindp Ident `thenp` (dotsym `xthenp` topexpr))) `usingp`
             uncurry Lambda

letexpr = (letsym `xthenp` (defns `thenp` (insym `xthenp` topexpr))) `usingp`
          uncurry Letx
                       
-- defn :: Parser (Tag, [Char]) ([Char], Expr)
defn = kindp Ident `thenp` (manyp (kindp Ident) `thenp` (bindsym `xthenp` topexpr)) `usingp`
       varorlambda

varorlambda (f, ([], e)) = (f,e)
varorlambda (f, (xs, e)) = (f, (lambdas xs e))

lambdas [x] e = Lambda x e
lambdas (x:xs) e = Lambda x (lambdas xs e)

defns = defn `thenp` (manyp (semisym `xthenp` defn)) `usingp` \(d,ds) -> (d:ds)

compexpr = (addexpr `thenp` comprhs) `usingp` foldbinl
comprhs = manyp ((eqsym `altp` nesym `altp` ltsym `altp` lesym `altp` gtsym `altp` gesym ) `thenp` addexpr)

addexpr = (multexpr `thenp` addrhs) `usingp` foldbinl

addrhs :: Parser Token [([Char], Expr)]
addrhs = manyp ((plussym `altp` minussym) `thenp` multexpr)

foldbinl :: (Expr, [([Char], Expr)]) -> Expr
foldbinl (e, oes) = foldl f e oes
	            where
                     f :: Expr -> ([Char],Expr) -> Expr
                     f e (o, e1) = Binop o e e1

multexpr = (powerexpr `thenp` multrhs) `usingp` foldbinl

multrhs = manyp ((timessym `altp` divsym) `thenp` powerexpr)

powerexpr = (appexpr `thenp` powerrhs) `usingp` \(e, oes) -> foldbinr e oes

foldbinr :: Expr -> [([Char], Expr)] -> Expr
foldbinr e [] = e
foldbinr e ((o,e1):oes) = Binop o e (foldbinr e1 oes)

powerrhs = manyp (expsym `thenp` appexpr)

appexpr = (somep factorexpr) `usingp` foldl1 App

-- unopexpr = maybep minussym `thenp` factorexpr `usingp`

factorexpr = variable `altp` (number `altp` parenexpr)

parenexpr = lparensym `xthenp` (topexpr `thenxp` rparensym)

tokenize = stripp.fst.(safehd "in tokenize").lexerp

parse = fst.(safehd "in parse").topexpr.tokenize

safehd msg [] = error ("safehd:  " ++ msg)
safehd msg (x:xs) = x


-- Evaluation

type Env = [([Char], Thunk)]

-- data Thunk = Evaluated Int | Unevaluated Expr Env deriving(Eq, Show)

data Thunk = EvalI Int | EvalL Nonstrictlist | Uneval Expr Env deriving(Eq,Show)

data Nonstrictlist = Nil | Cons Thunk Nonstrictlist deriving(Eq,Show)

evalexpr :: Expr -> Env -> Thunk

-- evalexpr (Letx binds e) env = evalexpr e (zip (map fst binds) ((map ((\e -> Unevaluated e env) . snd) binds) ++ env))

evalexpr (Letx binds e) env = evalexpr e (newenv ++ env)
                              where  -- note (newenv ++ env), this is for recursive let
                                newenv = zip (map fst binds) ((map ((\e -> Uneval e (newenv ++ env)) . snd) binds))

evalexpr (Ifx condit conseq alter) env = if ((evalexpr condit env) == EvalI 0) then (evalexpr alter env)
                                         else (evalexpr conseq env)

evalexpr (Lambda vs e) env = Uneval (Lambda vs e) env

evalexpr (Binop o e1 e2) env =
  let (EvalI i1) = evalexpr e1 env
      (EvalI i2) = evalexpr e2 env
  in case o of
    "+" -> EvalI (i1+i2)
    "-" -> EvalI (i1-i2)
    "*" -> EvalI (i1*i2)
    "/" -> EvalI (div i1 i2)
    "^" -> EvalI (i1^i2)
    "<" -> EvalI (if i1 < i2 then 1 else 0)
    "<=" -> EvalI (if i1 <= i2 then 1 else 0)
    ">" -> EvalI (if i1 > i2 then 1 else 0)
    ">=" -> EvalI (if i1 >= i2 then 1 else 0)
    "==" -> EvalI (if i1 == i2 then 1 else 0)
    "/=" -> EvalI (if i1 /= i2 then 1 else 0)
    o   -> error ("No such operator as '" ++ o ++ "'")

evalexpr (App e1 e2) env =
  let (Uneval (Lambda v b) envl) = evalexpr e1 env
  in evalexpr b ((v, Uneval e2 env):envl)

evalexpr (Num x) env = EvalI x
evalexpr (Var v) env = case head (findsnd v env) of
                                   EvalI i -> EvalI i
                                   Uneval e enve -> evalexpr e enve

evaluate xs = evalexpr(parse xs) []

findsnd v [] = error ("no bound variable " ++ v)
findsnd z ((x,y):xs) | (z == x)		= y:findsnd z xs
	  	     | otherwise	= findsnd z xs

rightof name ((x,y):xs) | (name == x)	    = xs
	     		| otherwise	    = rightof name xs


b = " \\x -> \\y -> \\z -> x (y z) "

m = " \\x -> x x "

n = " b m (b (b m) b) "

e0 = "let fact n = if n < 1 then 1 else n * fact (n-1)" ++
     " in " ++
     "    fact 5" 

e5 = "let b x y z = x (y z);" ++
     "    m x = x x;" ++
     "    fix = b m (b (b m) b);" ++
     "    g h x = if x < 1 then 1 else x * h (x-1)" ++
     " in " ++
     "    fix g 5"

e4 = "let b =" ++ b ++ ";" ++
     "    m =" ++ m ++ ";" ++
     "    n =" ++ n ++ ";" ++
     "    fix =" ++ n ++ ";" ++
     "    g = \\h -> \\x -> if x then x * h (x-1) else 1 " ++
     " in " ++
     "    fix g 5"



e2 = "let fix = \\f -> f (fix f) ; " ++
     "    g = \\h -> \\x -> if x then x * h (x-1) else 1 " ++
     " in " ++
     "    fix g 5"

y = " \\f -> (\\x -> f (x x)) (\\x -> f (x x)) "

e3 = "let fix =" ++ y ++ ";" ++
     "    g = \\h -> \\x -> if x then x * h (x-1) else 1 " ++
     " in " ++
     "    fix g 5"

e1 = "let fact = \\x -> if x then x * fact (x-1) else 1 " ++
     "in" ++
     "  fact 5"


{-
-- Code Gen
data Instructions = Push Int | Store [Char] [Instructions] | Load [Char] | Delete [Char] | Addop | Subop | Mulop | Divop | Powop | Ifop deriving(Show)

--let x = 5 + 3 in x * 2    becomes
--Push 5, Push 3, Add, Push 2, Mult 

gencodetop (Ifx condit conseq alter) = [Store "a1" (gencodetop conseq)] ++ [Store "a2" (gencodetop alter)] ++ gencodetop condit ++ [Ifop] ++ [Delete "a2"] ++ [Delete "a1"]
gencodetop (Letx name val x) = [Store (head name) (gencodetop val)] ++ gencodetop x ++ [Delete (head name)]
gencodetop (TopExprNoop x) = gencodeadd x

gencodeadd (Add x y) = (gencodeadd x) ++ (gencodemult y) ++ [Addop]
gencodeadd (Sub x y) = (gencodeadd x) ++ (gencodemult y) ++ [Subop]
gencodeadd (AddExprNoop x) = gencodemult x

gencodemult (Mult x y) = (gencodemult x) ++ (gencodepower y) ++ [Mulop]
gencodemult (Div x y) = (gencodemult x) ++ (gencodepower y) ++ [Divop]
gencodemult (MultExprNoop x) = gencodepower x

gencodepower (Power x y) = (gencodefactor x) ++ (gencodepower y) ++ [Powop]
gencodepower (PowerExprNoop x) = gencodefactor x

gencodefactor (Num x) = [Push x]
gencodefactor (Var x) = [Load x]
gencodefactor (FactorExprNoop x) = gencodetop x


gencode xs = gencodetop(parse xs)




-- Stack Calculator
calcstack (Ifop:s) (x:xs) vars = if (x == 0) then calcstack (Load)
calcstack ((Store y val):ys) xs vars = calcstack ys xs ((y, val):vars)
calcstack ((Load y):ys) xs vars = calcstack ys ((head (calcstack (head (findsnd y vars)) [] (rightof y vars))):xs) vars
calcstack ((Delete y):ys) xs vars = calcstack ys xs (removesnd y vars)
calcstack ((Push y):ys) xs vars = calcstack ys (y:xs) vars
calcstack (Addop:s) (x:y:xs) vars = calcstack s ((y + x):xs) vars
calcstack (Subop:s) (x:y:xs) vars = calcstack s ((y - x):xs) vars
calcstack (Mulop:s) (x:y:xs) vars = calcstack s ((y * x):xs) vars
calcstack (Divop:s) (x:y:xs) vars = calcstack s ((y `div` x):xs) vars
calcstack (Powop:s) (x:y:xs) vars = calcstack s ((y ^ x):xs) vars
calcstack [] xs _ = xs

calculate xs = head (calcstack (gencode xs) [] [])

removesnd _ [] = []
removesnd z ((x,y):xs) | (z == x)	= xs
	  	       | otherwise	= (x,y):removesnd z xs

-}
