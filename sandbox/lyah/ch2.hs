-- learn you a haskell ch2

doubleMe x = x + x  

doubleUs x y = x*2 + y*2   

doubleUs' x y = x + x + y + y

doubleUs'' x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2 

doubleSmallNumber' x = (if x > 100 then x else x*2) + 1 

conanO'Brien = "It's a-me, Conan O'Brien!"

replicate' x y = take x (repeat y)

lc1 = [x*2 | x <- [1..10]]  

lc2 =  [x*2 | x <- [1..10], x*2 >= 12]  

lc3 = [ x | x <- [50..100], x `mod` 7 == 3]  

boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x] 

lc4 =  [ x | x <- [10..20], x /= 13, x /= 15, x /= 19] 

lc5 = [ x*y | x <- [2,5,10], y <- [8,10,11]]  

-- all combos
pall xs ys = [ x*y | x <- xs, y <-ys] 

-- pairwise 
ppairs xs     []     = []
ppairs []     ys     = [] 
ppairs (x:xs) (y:ys) = x*y : ppairs xs ys 

length' xs = sum [1 | _ <- xs]  

removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z']]

xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]  

nested = [ [ x | x <- xs, even x ] | xs <- xxs] 

-- apply function to each pair in list
pairf f [] = []
pairf f ((x,y):xs) = f x y:pairf f xs

ppairs' xs ys = pairf (*) (zip xs ys)

triangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10] ] 

rightTriangles = [ (a,b,c) | c <- [1..10], b <- [1..10], a <- [1..10], 
	a^2 + b^2 == c^2] 

rightTriangles' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], 
	a^2 + b^2 == c^2] 

rightTriangles'' = [ (a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], 
	a^2 + b^2 == c^2, a+b+c == 24] 


