-- build a map from constructor names -> (arity,tag)

{-# LANGUAGE FlexibleInstances #-}

module ConMap (
  getConMap
) where

import qualified Data.Map as Map

import Parser

type ConMap = Map.Map Con (Int, Int)

getConMap :: [Obj a] -> ConMap
getConMap objs = tagit $ build objs Map.empty

-- add increasing tags to map entries
tagit :: ConMap -> ConMap
tagit map = snd $ Map.mapAccum f 0 map
          where f s (a,t) = (s+1, (a, s));

-- insert map entry and check arity
insert :: Con -> Int -> ConMap -> ConMap
insert c arity map = if arityOk (Map.lookup c map) arity
                     then (Map.insert c (arity, undefined) map)
                     else error "CON arity mismatch"

-- check that either there is no entry for this CON yet
-- or that its arity matches the previous entry
arityOk :: Maybe (Int,Int) -> Int -> Bool
arityOk (Nothing) _ = True
arityOk (Just (arity,tag )) arity' | arity == arity' = True
                                   | otherwise = False

class BuildConMap t where build :: t -> ConMap -> ConMap

instance BuildConMap [Obj a] where
  build (o:os) map = build os map'
                   where map' = build o map
  build [] map = map

instance BuildConMap (Obj a) where
  build (FUN _ vs e _) map = build e map
  build (PAP _ f as _) map = map
  build (CON _ c as _) map = insert c (length as) map
  build (THUNK _ e _) map = build e map
  build (BLACKHOLE _ _) map = map

instance BuildConMap (Expr a) where
  build (EAtom _ ea) map = map
  build (EFCall _ ev eas) map = map
  build (EPrimop _ eprimop eas) map = map
  build (ELet _ objs ee) map = build objs map
  build (ECase _ ee ealts) map = build ealts (build ee map)
            
instance BuildConMap [Alt a] where
  build (alt:alts) map = build alts map'
                       where map' = build alt map
  build [] map = map

instance BuildConMap (Alt a) where
  build (ACon _ c vs e) map = build e (insert c (length vs) map)
  build (ADef _ v e) map = build e map
 