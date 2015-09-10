qsort[] = []
qsort (x:xs) = qsort smaller ++[x]++qsort larger
             where 
                 smaller = [a|a <- xs, a < x]
                 larger = [b| b<- xs, b>x]

{- 
Numbers that are duplicates will ALWAYS be lost. This is because duplicates will
be compared against each other eventually, and they won't be placed in the 
'smaller', 'larger', or [x] lists. The remaining list is essentially the set 
formed by taking elements from the list, leaving no duplicates.
-}
