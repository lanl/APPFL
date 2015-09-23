{- 
Base Case: n = 0
  all (==x) replicate n x
   {substitute}
  all (==x) replicate 0 x 
   {apply replicate}
= all (==x) []
   {apply all}
= True
-}

{-
Inductive Case: Assume all (==x) replicate n x = True. Prove for n+1
  all (==x) replicate (n+1) x
   {apply replicate}
= all (==x) (x:replicate n x)
   {apply all}
= (==x) x && all (==x) replicate n x
   {apply (==x)}
= True && all (==x) replicate n x
   {inductive step}
= True && True
   {apply &&}
= True
-}
