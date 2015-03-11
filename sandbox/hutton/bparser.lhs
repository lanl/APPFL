Expression parser example from section 8.8 of Programming in Haskell,
Graham Hutton, Cambridge University Press, 2007.


Parser for simple logical expressions
----------------------------------------

> import Parsing
>
> isBool :: String -> Bool
> isBool xs | xs == "True" = True
>           | xs == "False" = False
>           | otherwise = error("non bool")
> 
> boo :: Parser Bool
> boo =  do x  <- upper
>           xs <- many letter
>           return (isBool (x:xs))
>
> bool :: Parser Bool
> bool = token boo
>
> imply :: Bool -> Bool -> Bool
> imply True False = False
> imply _    _     = True
>
> expr                          :: Parser Bool
> expr                          =  do t <- factor
>                                     do symbol "||"               
>                                        e <- expr
>                                        return (t||e)
>                                      +++ return t
>                                     do symbol "&&"               
>                                        e <- expr
>                                        return (t && e) 
>                                      +++ return t
>                                     do symbol "=>"               
>                                        e <- expr
>                                        return (imply t e)
>                                      +++ return t
>                                     do symbol "=="               
>                                        e <- expr
>                                        return (t == e)
>                                      +++ return t
>
> factor                        :: Parser Bool 
> factor                        =  do symbol "("
>                                     e <- expr
>                                     symbol ")"
>                                     return e
>                                   +++ 
>                                  do symbol "!"
>                                     e <- expr
>                                     return (not e)
>                                   +++ bool
>
> eval                          :: String -> Bool
> eval xs                       =  case (parse expr xs) of
>                                     [(n,[])]  -> n
>                                     [(_,out)] -> error ("unused input " ++ out)
>                                     []        -> error "invalid input"

