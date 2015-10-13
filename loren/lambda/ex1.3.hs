3. Carry out the following substitutions:

a.) 
  (f (\x -> x y ) \z -> x y z)[x -> g]
= (f (\x -> x y )[x->g]) ((\z -> x y z)[x -> g])
= (f ((\x -> x y )[x->g])) ((\z -> ((x y z)[x -> g])))
= (f ((\x -> x y ))) ((\z -> ((x y)[x -> g](z)[x -> g])))
= (f (\x -> x y )) ((\z -> (((x)[x-> g] (y)[x -> g])(z))))
= (f (\x -> x y )) ((\z -> (((g) (y))(z))))
= (f (\x -> x y )) ((\z -> ((g y)(z))))

b.) 
  (\x -> \y -> f x y)[y -> x]
= (\x -> \y -> f x y)[y -> x]
= (\z -> ((\y -> f x y)[x -> z][y -> x]))
= (\z -> ((\y -> ((f x y)[x -> z]))[y -> x]))
= (\z -> ((\y -> (f z y))[y -> x]))
= (\z -> ((\y -> f z y)))

c.) 
  ((\x -> f x) \f -> f x)[f -> g x]
= (\x -> f x)[f -> g x] (\f -> f x)[f -> g x]
= (\z -> ((f x)[x -> z][f -> g x])) (\f -> f x)
= (\z -> ((f z)[f -> g x])) (\f -> f x)
= (\z -> (((g x) z))) (\f -> f x)

d.) 
  (\f -> \y -> f x y)[y -> x]
= (\f -> (\y -> f x y)[y -> x])
= (\f -> (\y -> f x y))
