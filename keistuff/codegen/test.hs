
data T = T {name :: String} deriving(Show)

f x name = x {name = name}

y = T {}
