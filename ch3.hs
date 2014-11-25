-- Ch3 exercises from Graham Hutton's Programming in Haskell

q1 = ['a', 'b', 'c']
q2 = ('a', 'b', 'c')
q3 = [(False, '0'), (True, '1')]
q4 = ([False, True], ['0', '1'])
q5 = [tail, init, reverse]

second xs = head(tail xs)
swap (x,y) = (y,x)
pair x y = (x,y)
double x = x*2
palindrome xs = reverse xs == xs
twice f x = f(f x)
