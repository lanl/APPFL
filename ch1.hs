-- Ch1 exercises from Graham Hutton's Programming in Haskell

s = [2,3,4]
q = [5,9,1,8,2,7,3,6,4]
w = [2,2,3,1,1]

product' [] = 1
product' (x:xs) = x * product' xs

qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
		where 
               		smaller = [a | a <- xs, a <= x]
               		larger =  [b | b <- xs, b > x]

qsortu [] = []
qsortu (x:xs) = qsortu smaller ++ [x] ++ qsortu larger
		where 
               		smaller = [a | a <- xs, a < x]
               		larger =  [b | b <- xs, b > x]

qsortr [] = []
qsortr (x:xs) = qsortr larger ++ [x] ++ qsortr smaller 
		where 
               		smaller = [a | a <- xs, a <= x]
               		larger =  [b | b <- xs, b > x]
