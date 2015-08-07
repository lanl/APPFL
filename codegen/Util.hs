module Util
(
  mapFst,
  mapSnd,
  deleteAll,
  indent,
  groupAllBy,
  lookupOrElse,
  maxPayload
)
where


import qualified Data.Map as Map
import Data.List (nubBy, partition)

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


lookupBy :: (a -> b -> Bool) -> a -> [(b,c)] -> Maybe c
lookupBy f it as = case [v | (k,v) <- as, f it k] of
                    [] -> Nothing
                    v:vs -> Just v

-- first-occurrence-order-preserving grouping of similar elements in a list
groupAllBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupAllBy f xs =
  let nubbed = nubBy f xs
  in if (length nubbed == length xs)
-- if list is full of uniques, package them in lists
     then map (:[]) xs 
-- else fold on the uniques, accumulating the groups (hence reverse) and the unsorted
     else reverse $ fst $ foldl ffun ([],xs) nubbed
  where ffun (groups, xs) it = let (matches, rest) = partition (f it) xs
                               in (matches:groups, rest)
