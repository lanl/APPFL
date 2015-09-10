init1 xs = take((length xs)-1) xs

init2 xs = reverse(drop 1 (reverse xs))
