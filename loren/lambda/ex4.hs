--4.) Define functions that add 4 and 16 to their arguments using twice and increase

increase :: Integer -> Integer
increase = \x -> (x+1)

twice :: (t -> t) -> t -> t
twice = \f -> (\x -> (f (f x)))

addfour = twice (twice increase)

addSixteen = twice (twice addfour)
