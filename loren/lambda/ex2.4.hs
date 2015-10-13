4. Show that Tail (Pair p q) => q

Tail (Pair p q)
= Pair p q (\a -> (\b -> b))
= (\a -> (\b -> b)) p q
= (\b -> b) q
= q
