{-
1+(2*3) 

redexes: 2*3, innermost; 

(1+2)*(2+3)

redexes: 1+2, innermost; 2+3;

fst(1+2,2+3)

redexes: 1+2, innermost; 2+3; fst(1+2,2+3), outermost.

(\x -> 1 + x)(2*3)

redexes: (\x -> 1 + x)(2*3), outermost; 2*3, innermost
-}
