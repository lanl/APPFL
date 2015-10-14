5.) Verify the reductions

Succ 0 
= (\n -> (\f -> (\x -> f (n f x)))) (\g -> (\y -> y))
= (\f -> (\x -> f ((\g -> (\y -> y)) f x))) 
= (\f -> (\x -> f ((\y -> y) x)))
= (\f -> (\x -> f x))
= 1

Add 2 3 
= (\m -> (\n -> (\f -> (\x -> m f (n f x))))) (\g -> (\y -> g (g y))) (\h -> (\z -> h (h (h z))))
= (\n -> (\f -> (\x -> (\g -> (\y -> g (g y))) f (n f x))))  (\h -> (\z -> h (h (h z))))
= (\f -> (\x -> (\g -> (\y -> g (g y))) f ((\h -> (\z -> h (h (h z)))) f x)))  
= (\f -> (\x -> (\g -> (\y -> g (g y))) f ((\z -> f (f (f z))) x)))
= (\f -> (\x -> (\g -> (\y -> g (g y))) f (f (f (f x))))) 
= (\f -> (\x -> (\y -> f (f y)) (f (f (f x))))) 
= (\f -> (\x -> f (f (f (f (f x)))))) 
= 5
