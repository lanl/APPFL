q = "abc"


n = a `div` length xs
    where 
      a = 10
      xs = [1,2,3,4,5]

last2 s = s!!(length s - 1)

last3 s = head(reverse s)

init2 s = reverse(tail(reverse s))

init3 s = take (length s - 1) s

