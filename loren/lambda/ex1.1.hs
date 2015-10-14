1.) Parenthesize the following expressions:
a.)
  \x -> x \y -> y x
=(\x -> x \y -> y x)
=(\x -> (x \y -> y x))
=(\x -> (x (\y -> y x)))
=(\x -> (x (\y -> (y x))))

b.) 
  (\x -> x) (\y -> y) \x -> x (\y -> y) z
=((\x -> x) (\y -> y) \x -> x (\y -> y) z)
=((\x -> x) ((\y -> y) \x -> x (\y -> y) z))
=((\x -> x) ((\y -> y) (\x -> x (\y -> y) z)))

c.) 
  (\f -> \y -> \z -> f z y z) p x
=((\f -> \y -> \z -> f z y z) p x)
=((\f -> (\y -> \z -> f z y z)) (p x))
=((\f -> (\y -> (\z -> f z y z)))(p x))
=((\f -> (\y -> (\z -> (f z y z))))(p x))
=((\f -> (\y -> (\z -> ((f z y) z))))(p x))
=((\f -> (\y -> (\z -> (((f z) y) z)))) (p x))

d.) 
  \x -> x \y -> y \z -> z \w -> w z y x
=(\x -> x \y -> y \z -> z \w -> w z y x)
=(\x -> (x \y -> y \z -> z \w -> w z y x))
=(\x -> (x (\y -> y \z -> z \w -> w z y x)))
=(\x -> (x (\y -> (y \z -> z \w -> w z y x))))
=(\x -> (x (\y -> (y (\z -> z \w -> w z y x)))))
=(\x -> (x (\y -> (y (\z -> (z \w -> w z y x))))))
=(\x -> (x (\y -> (y (\z -> (z (\w -> w z y x)))))))
=(\x -> (x (\y -> (y (\z -> (z (\w -> (w z y x))))))))
=(\x -> (x (\y -> (y (\z -> (z (\w -> ((w z y) x))))))))
=(\x -> (x (\y -> (y (\z -> (z (\w -> (((w z) y) x))))))))

