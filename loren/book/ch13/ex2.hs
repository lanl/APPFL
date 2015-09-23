--Show add n (Succ m) = Succ (add n m) by induction on n

{-
Base Case: Show add Zero (Succ m) = Suc (add Zero m)
 add Zero (Succ m) 
   {applying add}
= Succ m
   {unapplying add}=
= Succ (add Zero m)
-}

{-
Inductive Step: Show add n (Succ m) = Succ (add n m)
 add (Succ n) (Succ m)
   {applying add}
= Succ (add n (Succ m))
   {inductive step}
= Succ (Succ (add n m))
   {undo add}
= Succ (add (Succ n) m)
-}

