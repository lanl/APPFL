-- learn you a haskell ch 9

putStr' :: String -> IO ()  
putStr' [] = return ()  
putStr' (x:xs) = do  
    putChar x  
    putStr' xs
  
