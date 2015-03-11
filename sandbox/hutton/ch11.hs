-- Ch11 exercises from Graham Hutton's Programming in Haskell

subs                          :: [a] -> [[a]]
subs []                       =  [[]]
subs (x:xs)                   =  yss ++ map (x:) yss
                                  where yss = subs xs

interleave                    :: a -> [a] -> [[a]]
interleave x []               =  [[x]]
interleave x (y:ys)           =  (x:y:ys) : map (y:) (interleave x ys)

perms                         :: [a] -> [[a]]
perms []                      =  [[]]
perms (x:xs)                  =  concat (map (interleave x) (perms xs))

choices                       :: [a] -> [[a]]
choices xs                    =  concat (map perms (subs xs))

--Ex 1

choices' :: [a] -> [[a]]
choices' xs = [y | ys <- [ perms x | x <- subs xs], y <- ys]

choices'' :: [a] -> [[a]]
choices'' xs = [ps | ss <- subs xs, ps <- perms ss] 

--Ex 2

removefirst :: Eq a => a -> [a] -> [a]
removefirst x [] = []
removefirst x (y:ys) | x == y = ys
                     | otherwise = y:removefirst x ys

isChoice ::  Eq a => [a] -> [a] ->Bool
isChoice xs [] = False
isChoice [] ys = True
isChoice (x:xs) ys = if elem x ys then isChoice xs (removefirst x ys) else False

--Ex 4: see ch11-4.lhs

--Ex 5: see ch11-5.lhs


