2. Find the set of free variables for each of the following lambda expressions:

a.) 
  FV(\x -> x y \z -> x z)
= FV(x y \z -> x z) - {x}
= (FV(x) U FV(y \z -> x z)) - {x}
= ({x} U FV(y) U FV(\z -> x z)) - {x}
= ({x} U {y} U FV(x z) - {z}) -{x}
= (({x} U {y} U FV(x) U FV(z)) - {z}) - {x}
= (({x} U {y} U {x} U {z}) - {z}) - {x}
= (({x,y,z}) - {z}) - {x}
= ({x,y,z} - {z}) - {x}
= ({x,y}) - {x}
= {x,y} - {x}
= {y}

b.) 
  FV((\x -> x y) \z -> w \w -> w z y x)
= (FV((\x -> x y))) U (FV(\z -> w \w -> w z y x))
= (FV(x y) -{x}) U (FV(w \w -> w z y x) - {z})
= ((FV(x) U FV(y)) -{x}) U ((FV(w) U FV(\w -> w z y x)) - {z})
= (({x} U {y}) -{x}) U (({w} U (FV(w z y x) - {w})) - {z})
= (({x,y}) - {x}) U (({w} U ((FV(w z y) U FV(x)) - {w})) - {z})
= ({x,y} - {x}) U (({w} U (((FV(w z) U FV(y) U {x}) - {w})) - {z})
= ({y}) U (({w} U ((((FV(w) U FV(z)) U {y}) U {x}) - {w})) - {z})
= {y} U (({w} U (((({w} U {z}) U {y}) U {x}) - {w})) - {z})
= {y} U (({w} U ((({w,z} U {y}) U {x}) - {w})) - {z})
= {y} U (({w} U (({w,y,z} U {x}) - {w})) - {z})
= {y} U (({w} U ({w,x,y,z} - {w})) - {z})
= {y} U (({w} U {x,y,z}) - {z})
= {y} U ({w,x,y,z} - {z})
= {y} U {w,x,y}
= {w,x,y}

c.) 
  FV(x \z -> x \w -> w z y) 
= FV(x) U FV(\z -> x \w -> w z y)
= {x} U ((FV(x \w -> w z y) - {z})
= {x} U ((FV(x) U FV(\w -> w z y))-{z})
= {x} U (({x} U (FV(w z y) -{w}))-{z})
= {x} U (({x} U ((FV (w z) U FV(y)) - {w}))-{z})
= {x} U (({x} U (((FV(w) U FV(z)) U {y}) - {w}))-{z})
= {x} U (({x} U ((({w} U {z}) U {y}) - {w}))-{z})
= {x} U (({x} U ((({w,z}) U {y}) - {w}))-{z})
= {x} U (({x} U (({w,z} U {y}) - {w}))-{z})
= {x} U (({x} U (({w,y,z}) - {w}))-{z})
= {x} U (({x} U ({w,y,z} - {w}))-{z})
= {x} U (({x} U ({y,z}))-{z})
= {x} U (({x} U {y,z})-{z})
= {x} U (({x,y,z})-{z})
= {x} U ({x,y,z}-{z})
= {x} U ({x,y})
= {x} U {x,y}
= {x,y}

d.) 
  FV(\x -> x y \x -> y x)
= FV(x y \x -> y x) - {x}
= (FV(x) U FV(y \x -> y x)) - {x}
= ({x} U (FV(y) U FV(\x -> y x))) - {x}
= ({x} U ({y} U (FV(y x)-{x}))) - {x}
= ({x} U ({y} U ((FV(y) U FV(x))-{x}))) - {x}
= ({x} U ({y} U (({y} U {x})-{x}))) - {x}
= ({x} U ({y} U (({x,y})-{x}))) - {x}
= ({x} U ({y} U ({x,y}-{x}))) - {x}
= ({x} U ({y} U ({y}))) - {x}
= ({x} U ({y} U {y})) - {x}
= ({x} U ({y})) - {x}
= ({x} U {y}) - {x}
= ({x,y}) - {x}
= {x,y} - {x}
= {y}
