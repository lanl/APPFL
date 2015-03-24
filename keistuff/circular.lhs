Circular Programming Example

Suppose you have a recursive let, that is

  let { x = "x yz yyy";
        y = "zy x z z";
        z =  "yyz x"
      } in ...


inp = [('z', "x yz yyy"),
       ('y', "zy x z z"),
       ('x', "yyz x")]

-- cgobj env s
-- cgobj returns the the size of s (its length) and the codegen of s
-- env is [(var, index of var from top of stack)], stack grows up
-- s is traversed only once

cgobj env [] = (0, [])

-- ' ' adds to size but doesn't generate code
cgobj env (' ':e) = let (n, c) = cgobj env e
                    in (n + 1, c)

-- cgobj env
cgobj env (v:e) = let (n, c) = cgobj env e
                  in (n + 1, cgvar v env : c)

cgvar env v = "SP(" ++ show (indexof env v) ++ ")"

indexof v []       = error "env lookup fail"
indexof v ((v',i):env) | v==v'     = -i
                       | otherwise = -i + indexof v env

-- note that env is needed before it's defined (so to speak)

cgletrec defs = let (vars, objs) = unzip defs
                    env = zip vars sizes
                    (sizes, objcodes) = unzip( map (cgobj env) objs)
                in (env, zip vars objcodes)
