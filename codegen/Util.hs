module Util
(
  mapFst,
  mapSnd,
  deleteAll,
  indent,
  maxPayload
)
where


import qualified Data.Map as Map

mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a, b)

mapSnd :: (c -> b) -> (a,c) -> (a,b)
mapSnd f (a,b) = (a, f b)


deleteAll :: Ord k => [k] -> Map.Map k v -> Map.Map k v
deleteAll vs env = foldr (Map.delete) env vs



indent :: Int -> String -> String
indent i xs = (take i $ repeat ' ') ++ indent' i xs
    where
      indent' i ('\n':x:xs) = '\n' : (take i $ repeat ' ') ++ indent' i (x:xs)
      indent' i "\n"        = "\n"
      indent' i (x:xs)      = x : indent' i xs
      indent' i ""          = ""
      
      
maxPayload :: Int     
maxPayload = 32
