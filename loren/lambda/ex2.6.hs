6. Carry out the following reductions

a) Curry (Uncurry h)
= (\f -> (\x -> (\y -> f (Pair x y)))) ((\f -> (\p -> f (Head p) (Tail p))) h) 
= (\f -> (\x -> (\y -> f (Pair x y)))) ((\p -> h (Head p) (Tail p))) 
= (\x -> (\y -> ((\p -> h (Head p) (Tail p))) (Pair x y))) 
= (\x -> (\y -> (h (Head (Pair x y)) (Tail (Pair x y))))) 
= (\x -> (\y -> (h x y))) 
= h

b) Uncurry (Curry h) (Pair r s) 
= (\f -> (\p -> (f (Head p) (Tail p)))) ((\f -> (\x -> (\y -> (f (Pair x y))))) h) (Pair r s)
= (\f -> (\p -> (f (Head p) (Tail p)))) ((\x -> (\y -> (h (Pair x y))))) (Pair r s)
= (\p -> (((\x -> (\y -> (h (Pair x y))))) (Head p) (Tail p))) (Pair r s)
= (((\x -> (\y -> (h (Pair x y))))) (Head (Pair r s)) (Tail (Pair r s)))
= (((\x -> (\y -> (h (Pair x y))))) r (Tail (Pair r s)))
= (((\x -> (\y -> (h (Pair x y))))) r s)
= (\y -> (h (Pair r y))) s
= h (Pair r s) 
