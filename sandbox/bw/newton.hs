-- exercise 2.1.7
 
deriv f x = (f (x + dx) - f x)/dx
            where dx = 0.0001

newton f = until satis improve
           where satis y   = abs (f y) < eps
                 improve y = y - (f y/deriv f y)
                 eps       = 0.0001 

sqrtFn g x = g f x
          where f y = y * y - x

asqrt x = sqrtFn newton x

newton' f x = until satis improve x
            where satis y   = abs (f y) < eps * x
                  improve y = y - (f y/deriv f y)
                  eps       = 0.0001 

asqrt' x = sqrtFn newton' x

newton'' f x = snd $ until satis improver (x, improve x)
             where satis (y, y') =  abs (y - y') < eps * abs y 
                   improver (y, y') = (y', improve y') 
                   improve y = y - (f y/deriv f y)
                   eps       = 0.0001 

asqrt'' x = sqrtFn newton'' x
