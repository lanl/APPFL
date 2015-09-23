-- Induction on the length of xs
{-Base Case: length xs = 0
  map f (map g xs)
   {substitution}
= map f (map g [])
   {applying map}
= map f []
   {applying map}
= []
   {applying map}
= map (f . g) []
-}

{-Inductive Step: Assume map f (map g xs) = map (f . g) xs for length xs = n, prove for length xs = n+1
  map f (map g (x:xs))
   {applying map}
= map f (g x : map g xs)
   {applying map}
= f (g x) : f (map g xs)
   {applying inductive hypothesis}
= f (g x) : (f . g) xs
   {applying map}
= (f . g) x : (f . g) xs
   {applying map}
= map (f . g) (x:xs)
-}
