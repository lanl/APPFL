--5.) Give a definition of the set of bound variables in a lambda expression E, denoted by BV(E).

BV(c) = {} for any constant c
BV(x) = {x} for any variable x
BV(E1 E2) = BV(E1) ^ BV(E2)
BV(\x -> E) = BV(E) ^ {x}

