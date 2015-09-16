readLine1 xs =   do x <- getChar
                    case x of 
                         '\n' -> return xs
                         '\DEL' -> if null xs
                                      then 
                                          readLine1 xs
                                      else 
                                          do putStr "\ESC[1D \ESC[1D"
                                             readLine1(init xs)
                         _ -> readLine1 (xs ++ [x])
readLine = readLine1 "" 

