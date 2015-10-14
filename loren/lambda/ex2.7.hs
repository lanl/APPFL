7. Translate these "let" expressions into lambda expressions and reduce them. Also, write the expressions using "where" instead of "let". 

a.) let x = 5 in let y = (add x 3) in (mul x y)

  (\y -> (\x -> (mul x y))) (add x 3) 5
= (\x -> (mul x (add x 3))) 5
= (mul 5 (add 5 3))
= 40

(mul x y) where y = (add x 3)
                x = 5

b.) let a = 7 in let g = \x -> (mul a x) in let a = 2 in (g 10)

  (a -> (g -> (a -> (g 10)))) 2 (\x -> (mul a x)) 7 
= (g -> (a -> (g 10))) (\x -> (mul a x)) 7
= (a -> ((\x -> (mul a x)) 10))  7
= ((\x -> (mul 7 x)) 10)
= ((mul 7 10))
= 70

(g 10) where a = 2
             g = (\x -> (mul a x)) 
             a = 7
