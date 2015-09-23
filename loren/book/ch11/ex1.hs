data Op = Add | Sub | Mul | Div deriving (Show)

apply :: Op -> Int -> Int -> Int 
apply Add x y = x+y
apply Sub x y = x-y
apply Mul x y = x*y
apply Div x y = x`div`y

data Expr = Val Int | App Op Expr Expr deriving (Show)

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map(x:) yss 
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys):map (y:)(interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

{-
choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))
-}
--Exercise 1: construct choices using list comprehension

choices :: [a] -> [[a]]
choices xs  = [x | y <- subs xs, x <- perms y] 

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && (eval e) == [n]

split :: [a] -> [([a],[a])]
split [] = []
split [_] = []
split (x:xs) = ([x],xs):[(x:ls,rs)|(ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <-split ns, l<-exprs ls, r<-exprs rs, e<-combine l r]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e| ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

results :: [Int]-> [Result]
results [] = []
results [n] = [(Val n, n) | n >0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

combine' :: Result -> Result -> [Result]
combine' (l,x)(r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m==n]

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = (x /= 1) && (y /= 1) && (x <= y)
valid Div x y = (y /= 1) && (x `mod` y ==0)
