module Util (
  indent,
  maxPayload
) where



indent :: Int -> String -> String
indent i xs = (take i $ repeat ' ') ++ indent' i xs
    where
      indent' i ('\n':x:xs) = '\n' : (take i $ repeat ' ') ++ indent' i (x:xs)
      indent' i "\n"        = "\n"
      indent' i (x:xs)      = x : indent' i xs
      indent' i ""          = ""
      
      
maxPayload :: Int     
maxPayload = 32