{-

Show why outermost evaluation is preferable to innermost for the purposes of evaluating the expression  fst(1+2,2+3)

Answer: When outermost evaluation is implemented, the expression 

fst(1+2,2+3) 

evaluates to

1+2. 

If innermost evaluation was implemented, then both expressions 1+2 and 2+3 would need to evaluate, costing more time.

-}

