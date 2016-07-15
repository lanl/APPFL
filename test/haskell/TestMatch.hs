module TestMatch where

import AppflPrelude

data T = A | B | C Int


m1 t = case t of
  A -> B
  B -> A
  C i -> case i of
    10 -> A
    20 -> B

-- Produces a DataAlt match on B with a 'patError' RHS
m2 t = case t of
  A -> 23
  C i -> i + 4


-- Produces a DEFAULT with a 'irrefutPatError' RHS
m3 ~(C i) = 5 + i

    
m4 A = B
m4 _ = C 0
m4 B = A -- GHC cuts this out entirely


-- GHC seems to always generate nested equality tests for this sort of
-- thing For large builtin types like Ints, it does generate a
-- DEFAULT, but for smaller types (like T above), unmatched patterns
-- are (at least sometimes) matched explicitly with a 'patError' RHS
m5 1 = True
m5 34 = True
m5 18234 = True
m5 0 = True
m5 (-3) = True
m5 7 = True
m5 38832 = True
m5 349 = True
m5 6 = True
m5 444 = True
m5 101 = True

