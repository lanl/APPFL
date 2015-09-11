sumsqreven :: [Int] -> Int
sumsqreven  = sum . map (^2) . filter even
