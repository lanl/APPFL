-- Ch9 exercises from Graham Hutton's Programming in Haskell

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x:xs) = do putChar x
                    putStr' xs

putStr'' :: String -> IO ()
putStr'' = foldr f (return ())
           where f x p = putChar x >>= \_ -> p
 
