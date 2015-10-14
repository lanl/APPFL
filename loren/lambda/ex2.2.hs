2.) Use both normal order reduction and applicative order reduction to reduce the following lambda expressions. Reach a normal form representation if possible.

I will use normal order first and then applicative order second

a1.) (\g -> (g 5)) (\x -> (add x 3))
=  (((\x -> (add x 3)) 5)) 
=  ((((add 5 3))))
= 8

a2.) (\g -> (g 5)) (\x -> (add x 3))
=  (((\x -> (add x 3)) 5))
=  ((((add 5 3))))
= 8

b1.) (\x -> (\y -> (\z -> (z y))) x) p (\x -> x)
= (\y -> (\z -> (z y))) p (\x -> x)
= (\z -> (z p)) (\x -> x)
= (\x -> x) p
= p

b2.) (\x -> (\y -> (\z -> (z y))) x) p (\x -> x)
= (\x -> (\z -> (z x))) p (\x -> x)
= (\z -> (z p)) (\x -> x)
= (\x -> x) p 
= p 

c1.) (\x -> (x x x))(\x -> (x x x))
= (\x -> (x x x))(\y -> (y y y))(\z -> (z z z))
--This won't terminate

c2.) (\x -> (x x x))(\x -> (x x x))
= (\x -> (x x x))(\y -> (y y y))(\z -> (z z z))
--This won't terminate either

d1.) (\x -> (\y -> (add x ((\x -> (sub x 3)) y)))) 5 6
= (\y -> (add 5 ((\x -> (sub x 3)) y))) 6
= (add 5 ((\x -> (sub x 3)) 6))
= (add 5 ((sub 6 3)))
= (add 5 3)
= 8

d2.) (\x -> (\y -> (add x ((\x -> (sub x 3)) y)))) 5 6
= (\x -> (\y -> (add x ((sub y 3))))) 5 6
= (\y -> (add 5 ((sub y 3)))) 6
= (add 5 ((sub 6 3))) 
= (add 5 3) 
= 8

e1.) (\c -> c (\a -> (\b -> b))) ((\a -> (\b -> (\f -> (f a b)))) p q)
= ((\a -> (\b -> (\f -> (f a b)))) p q) (\a -> (\b -> b)) 
= ((\b -> (\f -> (f p b))) q) (\a -> (\b -> b))
= ((\f -> (f p q))) (\a -> (\b -> b))
= (((\a -> (\b -> b)) p q))  
= ((\b -> b) q))  
= (q)
= q

e2.) (\c -> c (\a -> (\b -> b))) ((\a -> (\b -> (\f -> (f a b)))) p q)
= ((\a -> (\b -> (\f -> (f a b)))) p q) (\a -> (\b -> b)) 
= ((\b -> (\f -> (f p b))) q) (\a -> (\b -> b))
= ((\f -> (f p q))) (\a -> (\b -> b))
= (((\a -> (\b -> b)) p q))  
= ((\b -> b) q))  
= (q)
= q

f1.) Twice (\n -> (mul 2 (add n 1))) 5
= (\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) 5)
= (mul 2 (add ((\n -> (mul 2 (add n 1))) 5) 1)) 
= (mul 2 (add ((mul 2 (add 5 1))) 1))
= 26

f2.) Twice (\n -> (mul 2 (add n 1))) 5
= (\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) 5)
= (\n -> (mul 2 (add n 1))) ((mul 2 (add 5 1)))
= (mul 2 (add ((mul 2 (add 5 1))) 1))
= 26

g1.) Twice (Twice (\n -> (mul 2 (add n 1)))) 5
= Twice (\n -> (mul 2 (add n 1))) (Twice (\n -> (mul 2 (add n 1))) 5)
= (\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) (Twice (\n -> (mul 2 (add n 1))) 5))
= (mul 2 (add ((\n -> (mul 2 (add n 1))) (Twice (\n -> (mul 2 (add n 1))) 5)) 1))
= (mul 2 (add ((mul 2 (add (Twice (\n -> (mul 2 (add n 1))) 5) 1))) 1))
= (mul 2 (add ((mul 2 (add ((\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) 5)) 1))) 1))
= (mul 2 (add ((mul 2 (add ((mul 2 (add ((\n -> (mul 2 (add n 1))) 5) 1)) ) 1))) 1))
= (mul 2 (add ((mul 2 (add ((mul 2 (add ((mul 2 (add 5 1))) 1))) 1))) 1))
= 110

g2.) Twice (Twice (\n -> (mul 2 (add n 1)))) 5
= Twice (\n -> (mul 2 (add n 1))) (Twice (\n -> (mul 2 (add n 1))) 5)
= Twice (\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) 5))
= Twice (\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) ((mul 2 (add 5 1))))
= Twice (\n -> (mul 2 (add n 1))) ((mul 2 (add ((mul 2 (add 5 1))) 1)))
= (\n -> (mul 2 (add n 1))) ((\n -> (mul 2 (add n 1))) ((mul 2 (add ((mul 2 (add 5 1))) 1))))
= (\n -> (mul 2 (add n 1))) ((mul 2 (add ((mul 2 (add ((mul 2 (add 5 1))) 1))) 1)))
= (mul 2 (add ((mul 2 (add ((mul 2 (add ((mul 2 (add 5 1))) 1))) 1))) 1)) 
=110

h1.) Twice Twice sqr 2
= (\f -> (\x -> f (f x))) (\g -> (\y -> g (g y))) sqr 2
= (\x -> (\g -> (\y -> g (g y))) ((\h -> (\z -> h (h z))) x)) sqr 2
= (\g -> (\y -> g (g y))) ((\h -> (\z -> h (h z))) sqr) 2
= (\y -> ((\h -> (\z -> h (h z))) sqr) (((\i -> (\w -> i (i w))) sqr) y)) 2
= ((\h -> (\z -> h (h z))) sqr) (((\i -> (\w -> i (i w))) sqr) 2)
= (\z -> sqr (sqr z)) (((\i -> (\w -> i (i w))) sqr) 2)
=  sqr (sqr (((\i -> (\w -> i (i w))) sqr) 2)) 
=  sqr (sqr (((\w -> sqr (sqr w)))2))
=  sqr (sqr (sqr (sqr 2)))
= 2^16

h2.) Twice Twice sqr 2
= (\f -> (\x -> f (f x))) (\g -> (\y -> g (g y))) sqr 2 
= (\x -> (\g -> (\y -> g (g y))) (\z -> x (x z))) sqr 2
= (\x -> (\y -> (\z -> x (x z)) ((\z -> x (x z)) y))) sqr 2
= (\x -> (\y ->  x (x ((\z -> x (x z)) y)))) sqr 2
= (\x -> (\y ->  x (x (x (x y))))) sqr 2
= (\y ->  sqr (sqr (sqr (sqr y)))) 2
= sqr (sqr (sqr (sqr 2)))
= 2^16

i1.) (\x -> ((\z -> (add x x)) ((\x -> (\z -> (z 13 x)) 0 div)))) ((\x -> (x 5)) sqr)
= ((\z -> (add ((\x -> (x 5)) sqr) ((\x -> (x 5)) sqr))) ((\x -> \z -> (z 13 x)) 0 div)) 
= ((add ((\x -> (x 5)) sqr) ((\x -> (x 5)) sqr))) 
= ((add ((sqr 5)) ((\x -> (x 5)) sqr)))
= ((add ((sqr 5)) ((sqr 5))))
= 50

i2.) (\x -> ((\z -> (add x x)) ((\x -> (\z -> (z 13 x)) 0 div)))) ((\x -> (x 5)) sqr)
= (\x -> ((\z -> (add x x)) ((\z -> (z 13 0)) 0 div))) ((\x -> (x 5)) sqr)
= (\x -> ((\z -> (add x x)) ((div 13 0)))) ((\x -> (x 5)) sqr)
= (\x -> ((add x x))) ((\x -> (x 5)) sqr)
= ((add ((\x -> (x 5)) sqr) ((\x -> (x 5)) sqr)))
= ((add ((sqr 5)) ((\x -> (x 5)) sqr)))
= ((add ((sqr 5)) ((sqr 5))))
= 50
