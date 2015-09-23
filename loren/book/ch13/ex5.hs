{-Induct on the length of xs
Base Case: length xs = 0
  xs ++ []
   {substitution}
= [] ++ []
   {definition of ++}
= []
  {substitution}
= xs

-}

{-
Inductive Step: Assuming xs ++ [] = xs for length xs = n, prove for length xs = n+1
  (x:xs) ++ []
   {applying definition of ++}
= x:(xs ++ [])
   {applying inductive step}
= x:(xs)
   {dropping parentheses}
= x:xs
-}

{-Induct on length of xs
Base Case: length xs = 0
  xs ++ (ys ++ zs)
   {substitution}
= [] ++ (ys ++ zs)
   {applying property of ++}
= (ys ++ zs)
   {applying property of ++ proved above}
= (ys ++ zs) ++ []
   {substitution}
= (ys ++ zs) ++ xs
-}

{-
Inductive Step: Assuming xs ++ (ys ++ zs) = (xs ++ ys) ++ zs for length xs = n, prove for length xs = n+1
 (x:xs) ++ (ys ++ zs) 
   {applying property of ++}
= x:(xs ++ (ys ++ zs))
   {applying inductive hypothesis}
= x:((xs ++ ys) ++ zs)
   {applying property of ++}
= (x:(xs++ys)) ++ zs
   {applying property of ++}
= ((x:xs) ++ ys) ++ zs)
-}
