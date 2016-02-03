
prog argc =
    let fun = "f0 = FUN(" ++
              concat [" x" ++ show i | i <- [1..argc]] ++
              " -> x1 );\n\n"

        fs = concat [ "f" ++ show i ++ " = THUNK( f" ++ show (i-1) ++ " true ); "
                      | i <- [1..argc-1] ] ++ "\n\n"

        mp = "main = THUNK( f" ++ show (argc - 1) ++ " true );\n"

     in fun ++ fs ++ mp

doit = do writeFile ("../test/stg/manyargs1.stg") $ prog 1
          writeFile ("../test/stg/manyargs2.stg") $ prog 2
          writeFile ("../test/stg/manyargs3.stg") $ prog 3
          writeFile ("../test/stg/manyargs4.stg") $ prog 4
          writeFile ("../test/stg/manyargs56.stg") $ prog 56
          writeFile ("../test/stg/manyargs57.stg") $ prog 57
          return ()
      
    
