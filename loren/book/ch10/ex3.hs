data Tree = Leaf Int | Node Tree Tree

unbalTree :: Tree
unbalTree = (Node (Leaf 1) (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))

balTree :: Tree
balTree = (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))

leaves :: Tree -> Int
leaves (Leaf m) = 1
leaves (Node m n) = (leaves m) + (leaves n) 

oneOff :: Int -> Int -> Bool
oneOff x y | abs (x-y) <= 1 = True
           | otherwise    = False

balanced :: Tree -> Bool
balanced (Leaf m) = True
balanced (Node m n) =  case oneOff (leaves m) (leaves n) of
                            True -> balanced m && balanced n
                            False -> False
