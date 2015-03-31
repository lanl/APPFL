-- build a map from constructor names -> (arity,tag)

{-# LANGUAGE FlexibleInstances #-}

module ConMap (
  getConMap
) where


import qualified Data.Map as Map 

import Parser

type ConMap = Map.Map Con (Int, Int)

-- type Def a = (Var, Obj a)

getConMap :: [Def a] -> ConMap
getConMap defs = tagit $ build objs Map.empty
   where (names, objs) = unzip defs

-- add increasing tags to map entries
tagit :: ConMap -> ConMap
tagit map = snd $ Map.mapAccum f 0 map  
          where f s (a,t) = (s+1, (a, s)); 

-- insert map entry and check arity
insert :: Con -> Int -> ConMap -> ConMap
insert c arity map = if arityOk (Map.lookup c map) arity 
                     then (Map.insert c (arity, undefined) map)  
                     else error "arity mismatch"            
          
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
  build (CON _ c as) map = insert c arity map
                         where arity = length as
  