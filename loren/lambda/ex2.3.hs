3 Use call by value semantics to reduce the following lambda expressions:

a)(\f -> f add (f mul (f add 5))) (\g -> (\x -> g x x))
= (\g -> (\x -> g x x)) add ((\g -> (\x -> g x x)) mul ((\g -> (\x -> g x x)) add 5)) 
= (\g -> (\x -> g x x)) add ((\g -> (\x -> g x x)) mul ((\x -> add x x) 5)) 
= (\g -> (\x -> g x x)) add ((\g -> (\x -> g x x)) mul (add 5 5)) 
= (\g -> (\x -> g x x)) add ((\g -> (\x -> g x x)) mul 10) 
= (\g -> (\x -> g x x)) add ((\x -> mul x x) 10)
= (\g -> (\x -> g x x)) add (mul 10 10)
= (\g -> (\x -> g x x)) add 100
= (\x -> add x x) 100
= add 100 100
= 200


b)(\x->(\f -> f (f x))) ((\y->(add y 2))((\z->(sqr z))((\y->(succ y))1))) sqr
= (\x->(\f -> f (f x))) ((\y->(add y 2))((\z->(sqr z))((succ 1)))) sqr
= (\x->(\f -> f (f x))) ((\y->(add y 2))((\z->(sqr z))(2))) sqr
= (\x->(\f -> f (f x))) ((\y->(add y 2))(sqr 2)) sqr
= (\x->(\f -> f (f x))) ((\y->(add y 2))4) sqr
= (\x->(\f -> f (f x))) ((add 4 2)) sqr
= (\x->(\f -> f (f x))) 6 sqr
= (\f -> f (f 6)) sqr
= sqr (sqr 6)
= sqr 36
= 2196
