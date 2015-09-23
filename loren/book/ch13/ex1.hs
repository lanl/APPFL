{-
Give an example of a function from the standard library in appendix A that is defined using overlapping patterns. 

Answer: 

Exponentiation: 
(^) :: (Num a, Integral b) => a -> b -> a
_ ^ 0 = 1
x ^ (n+1) = x*(x^n)
-}
