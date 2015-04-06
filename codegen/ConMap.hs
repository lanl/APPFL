-- build a map from constructor names -> (arity,tag)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ConMap (
  getConMap
) where

import Control.Monad.State
import qualified Data.Map as Map

import Parser

type ConMap = Map.Map Con (Int, Int)

getConMap :: [Obj a] -> ConMap
getConMap objs = snd $ runState tagit map
              where map = build objs Map.empty
              
              
getConMap' :: [Obj a] -> ConMap
getConMap' objs = snd $ runState (getConMapState objs) Map.empty           
              
getConMapState :: [Obj a] -> State ConMap ()
getConMapState objs = do
                        build' objs
                        tagit
               

-- add increasing tags to map entries
tagit :: State ConMap ()
tagit = do
          map <- get
          let f s (a,t) = (s+1, (a, s));
          put $ snd $ Map.mapAccum f 0 map
      

-- insert map entry if it does not exist, if it exists check arity
insert :: Con -> Int -> ConMap -> ConMap
insert c arity map = case Map.lookup c map of
                        Nothing -> Map.insert c (arity, undefined) map
                        (Just (arity',tag)) -> if (arity == arity') 
                                               then map 
                                               else error "CON arity mismatch!"


-- insert map entry if it does not exist, if it exists check arity
insertM :: Con -> Int -> State ConMap ()
insertM c arity = do 
                    map <- get
                    case Map.lookup c map of
                      Nothing -> put $ Map.insert c (arity, undefined) map
                      (Just (arity',tag)) -> if (arity == arity') 
                                             then return () 
                                             else error "CON arity mismatch!"

insert' :: Con -> Int -> ConMap -> ConMap
insert' c arity map = snd $ runState (insertM c arity) map

class BuildConMap t where build :: t -> ConMap -> ConMap

instance BuildConMap [Obj a] where
  build (o:os) map = build os (build o map)
  build [] map = map

instance BuildConMap (Obj a) where
  build (FUN {vs, e}) map = build e map
  build (CON {c, as}) map = insert' c (length as) map
  build (THUNK {e}) map = build e map
  build _ map = map

instance BuildConMap (Expr a) where
  build (ELet {edefs, ee}) map = build edefs map
  build (ECase {ee, ealts}) map = build ealts (build ee map)
  build _ map = map

instance BuildConMap [Alt a] where
  build (alt:alts) map = build alts (build alt map)
  build [] map = map

instance BuildConMap (Alt a) where
  build (ACon _ c vs e) map = build e (insert' c (length vs) map)
  build (ADef _ v e) map = build e map
  
  
class BuildConMap' t where build' :: t -> State ConMap ()

instance BuildConMap' [Obj a] where
  build' (o:os) = do 
                   build' o
                   build' os
  build' [] = return ()

instance BuildConMap' (Obj a) where
  build' (FUN {vs, e}) = build' e
  build' (CON {c, as}) = insertM c (length as)
  build' (THUNK {e}) =  build' e
  build' _ = return ()

instance BuildConMap' (Expr a) where
  build' (ELet {edefs, ee}) =  build' edefs 
  build' (ECase {ee, ealts})= do
                                build' ee
                                build' ealts
  build' _ = return ()

instance BuildConMap' [Alt a] where
  build' (alt:alts) = do 
                        build' alt
                        build' alts
  build' [] = return ()

instance BuildConMap' (Alt a) where
  build' (ACon _ c vs e) = do
                             insertM c (length vs)
                             build' e
  build' (ADef _ v e) = build' e 

