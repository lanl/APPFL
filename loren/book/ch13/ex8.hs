{- Induction on n and length of xs
{-Base Case: n = 0, length xs = 0
  take n xs ++ drop n xs
   {substitution}
= take 0 [] ++ drop 0 []
   {apply take and drop}
= [] ++ []
   {apply ++}
= []
   {substitute}
= xs
-}

{-Inductive Case 1: n=0, induct on length of xs = k
  take n xs ++ drop n xs
   {substitution}  
= take 0 xs ++ drop 0 xs
   {apply take and drop}
= [] ++ xs 
   {apply ++}
= xs
-}

{-Inductive Case 2: induct on n, length of xs = 0
  take n xs ++ drop n xs
   {substitution}
= take n [] ++ drop n []
   {apply take and drop}
= [] ++ []
   {apply ++}
= []
   {substitute}
= xs
-}

{-Inductive Case 3: Assume take n xs ++ drop n xs = xs for arbitrary n, xs
  We will show it holds for n+1, x:xs
  
  take (n+1) (x:xs) ++ drop (n+1) (x:xs)
   {apply take and drop}
=(x: take n xs) ++ drop n xs
  {apply ++ property}
= x:(take n xs ++ drop n xs)
  {apply inductive hypothesis}
= x:xs
-}
