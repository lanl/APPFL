-- From "Monadic Parser Combinations" Graham Hutton & Erik Meijer

-- 2.1 The type of parsers

type Parser a = String -> [(a, String)]

-- 2.2 Primitive parsers

-- always succeeds
result :: a -> Parser a
result v = \inp -> [(v,inp)]

result' :: a -> Parser a
result' v inp = [(v,inp)]

-- always fails
zero :: Parser a 
zero = \imp -> []

-- returns first char
item :: Parser Char
item = \inp -> case inp of
	[] -> []
	(x:xs) -> [(x,xs)]

item' :: Parser Char
item' [] = []
item' (x:xs) = [(x,xs)]


