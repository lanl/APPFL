

data Function2 a = F (a->a->a)


addNums = F (+)

appl :: Function2 a -> a -> a -> a
appl (F f) a b = f a b
