
inp = [('x', "xyzyyy"),
       ('y', "zyxzz"),
       ('z', "yyzx")]

cgobj env s = (length s, [indexof v env | v <- s])

indexof v []       = error "env lookup fail"
indexof v ((v',i):env) | v==v'     = -i
                       | otherwise = -i + indexof v env

cgletrec defs = let (vars, objs) = unzip defs
                    env = zip vars sizes
                    (sizes, objcodes) = unzip( map (cgobj env) objs)
                in zip vars objcodes
