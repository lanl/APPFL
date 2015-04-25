
repminlist xs =
    let (m, list) = rep m xs
    in list

rep m [] = (error "", [])

rep m [x] = (x, [m])

rep m (x:xs) = let (m', list) = rep m xs
               in (min m' x, m : list)


repminlist2 xs =
    let mlist = rep2 m xs
        m = case mlist of
              (m, xxx) -> m
        list = case mlist of
                 (xxx_, list) -> list
    in list

rep2 m xs = 
    case xs of
      [] -> (error "", [])
      (y : ys) -> case ys of
                    []   -> (y , [m])
                    xxx  -> case rep2 m ys of
                              (m', list) -> (min m' y, m : list)
