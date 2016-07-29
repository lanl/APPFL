{-# LANGUAGE ScopedTypeVariables #-}

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
  lookupBy, lookupOrElse,
  splits, splitsBy,
  toInt, toInt64, safeIntegerConvert,
  unreachable,
  _TODO
)
where

import qualified Data.Map as Map
import Data.List (nubBy, partition)
import Data.Int (Int64)

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



-- | Given a predicate on a single element of a list, split the list
-- into sublists, breaking on (and omitting) all non-empty sequences
-- of elements that satisfy the predicate.
splitsBy :: (a -> Bool) -> [a] -> [[a]]
splitsBy p xs = case break p xs of
                  ([],[])   -> []
                  ([],_:bs) -> splitsBy p bs
                  (as,[])   -> [as]
                  (as,_:bs) -> as : splitsBy p bs

-- | Special case of splitsBy in which the predicate is (== e)
splits :: Eq a => a -> [a] -> [[a]]
splits e = splitsBy (== e)



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

-- | Maybe convert and Integer to an Int (if it's within Int bounds)
toInt :: Integer -> Maybe Int
toInt = safeIntegerConvert

-- | Is an Integer within Int64 bounds?
toInt64 :: Integer -> Maybe Int64
toInt64 = safeIntegerConvert

-- | More general implementation of safely converting an Integer to a
-- Bounded Integral type.
safeIntegerConvert :: forall a . (Bounded a, Integral a) => Integer -> Maybe a
safeIntegerConvert i
  | i > maxVal || i < minVal  = Nothing 
  | otherwise = Just $ fromInteger i
  -- ScopedTypeVariables lets us refer to the type variable 'a' from above.
  -- The type annotation *is* necessary.
  where maxVal = toInteger (maxBound :: a)
        minVal = toInteger (minBound :: a)



-- If I knew some Template Haskell and we wanted to include the TH
-- Haskell package, these could be cleaner.  As it is, the way they're
-- often used is with a _HERE macro:
-- #define _HERE ( __FILE__ ++ ":" ++ show ( __LINE__ :: Int ) )

-- This may be bad practice, or an indicator of bad design.
-- It is, however, more convenient than manually typing a message
-- for every non-provably exhaustive pattern match
unreachable, _TODO :: String -> error
unreachable str = error ("Should not be reached: " ++ str)
_TODO str = error ("Definition incomplete: " ++ str)


