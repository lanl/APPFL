--Show that addition is commutative: add n m = add m n
{-
Base Case: 
  add Zero m 
   {applying add}
= m
   {result proven earlier about add}
= add m Zero
-}

{- 
Inductive Case: 
  add (Succ n) m 
   {applying property from exercise 2}
= Succ (add n m)
   {using induction hypothesis}
= Succ (add m n)
   {undoing property from exercise 2}
= add (Succ m) n
-}
