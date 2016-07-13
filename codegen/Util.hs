module Util
(
  mapFst,
  mapSnd,
  deleteAll,
  indent,
  maxPayload,
  zzip,
  precalate,
  partPerm,
  groupAllBy,
  lookupOrElse,
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
indent i xs = space ++ indent' i xs
    where
      space = take i $ repeat ' '
      indent' i ('\n':x:xs) = '\n' : space ++ indent' i (x:xs)
      indent' i "\n"        = "\n"
      indent' i (x:xs)      = x : indent' i xs
      indent' i ""          = ""

lookupOrElse :: (Ord k) => k -> Map.Map k v -> v
lookupOrElse k map = case Map.lookup k map of
                      Just k -> k
                      Nothing -> error "lookupOrElse failed!"
      
maxPayload :: Int
maxPayload = 32

-- sanity-checking zip
zzip [] [] = []
zzip (a:as) (b:bs) = (a,b) : zzip as bs -- changed to zzip here
zzip _ _ = error "zzip on lists of differing lengths"

-- this is probably not very efficient
precalate s ss = concatMap (s++) ss

-- given a predicate and a list
--   stably permute list according to the predicate, satisfies first
-- return
--   new - permuted list
--   permutation lists such that
--     old[i] = new[perm[i]]
--   #elements satisfying the predicate

partPerm pred xs =
    let (ts,fs,p) = g [] [] 0 0 [] xs
        tc = length ts -- circular
        g ts fs i j p [] = (ts, fs, p)
        g ts fs i j p (x:xs) | pred x = g (x:ts) fs     (i+1) j     (i:p)      xs
                             | True   = g ts     (x:fs) i     (j+1) ((j+tc):p) xs
    in (length ts, reverse ts ++ reverse fs, reverse p)


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
