or1 :: Bool -> Bool -> Bool
or1 True True = True
or1 False True  = True
or1 True False = True
or1 False False = False


or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _ _         = True

or3 :: Bool -> Bool -> Bool
or3 False b = b
or3 True _  = True


or4 :: Bool -> Bool -> Bool
or4 b c |b==c = b
or4 _  _|otherwise = True

