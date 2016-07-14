module Test where


import AppflPrelude


data C = C Int Bool

c i b =
  let f = C i
      g = C 15
  in if b
     then
       case i of
         2 -> f True
         _ -> f False
     else g False

t = (2,3)
t1 = (,) 2

main = 3

