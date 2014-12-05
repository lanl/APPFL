Abstract machine example from section 10.5 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


Abstract machine
----------------

> data Expr                     =  Val Int | Add Expr Expr | Mul Expr Expr
> 
> type Cont                     =  [Op]
>
> data Op                       =  AEVAL Expr | ADD Int | MEVAL Expr | MUL Int
> 
> eval                          :: Expr -> Cont -> Int
> eval (Val n)   c              =  exec c n
> eval (Add x y) c              =  eval x (AEVAL y : c)
> eval (Mul x y) c              =  eval x (MEVAL y : c)
> 
> exec                          :: Cont -> Int -> Int
> exec []            n          =  n
> exec (AEVAL y : c) n          =  eval y (ADD n : c)
> exec (ADD n  : c) m           =  exec c (n+m)
> exec (MEVAL y : c) n          =  eval y (MUL n : c)
> exec (MUL n  : c) m           =  exec c (n*m)
> 
> value                         :: Expr -> Int
> value e                       =  eval e []
