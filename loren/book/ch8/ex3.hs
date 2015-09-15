{-    (2+3)+4
                       expr
                   /     |    \
                 expr    +    term
             /    |   \        |
           expr   +   term    factor
            |          |       |
           term      factor   nat
            |          |       |
           factor     nat      4
            |          |  
           nat         3
            | 
            2
-}






{-    2+(3+4)
                       expr
                   /     |    \
                 term    +    expr
                  |         /   |   \
                 factor  term   +  expn
                  |        |        |      
                 nat     factor    term
                  |        |        | 
                  2      nat       factor
                           |        |
                           3       nat
                                    |
                                    4
-}
