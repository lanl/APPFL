module Util
(
  mapFst,
  mapSnd,
  deleteAll,
  indent,
  lookupOrElse,
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

lookupOrElse :: (Ord k) => k -> Map.Map k v -> v      
lookupOrElse k map = case Map.lookup k map of
                      Just k -> k
                      Nothing -> error "lookupOrElse failed!"
      
maxPayload :: Int     
maxPayload = 32
