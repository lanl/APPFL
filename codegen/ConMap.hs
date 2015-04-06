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
getConMap objs = snd $ runState (build objs) Map.empty           
              
-- insert map entry if it does not exist, if it exists check arity
insert :: Con -> Int -> State ConMap ()
insert c arity = do 
                    map <- get
                    case Map.lookup c map of
                      Nothing -> put $ Map.insert c (arity, Map.size map) map
                      (Just (arity',tag)) -> if (arity == arity') then return () 
                                             else error "CON arity mismatch!"

class BuildConMap t where build :: t -> State ConMap ()

instance BuildConMap [Obj a] where
  build (o:os) =  build o >> build os 
  build [] = return ()

instance BuildConMap (Obj a) where
  build (FUN {vs, e}) = build e
  build (CON {c, as}) = insert c (length as)
  build (THUNK {e}) =  build e
  build _ = return ()

instance BuildConMap (Expr a) where
  build (ELet {edefs, ee}) =  build edefs 
  build (ECase {ee, ealts}) = build ee >> build ealts
  build _ = return ()

instance BuildConMap [Alt a] where
  build (alt:alts) = build alt >> build alts
  build [] = return ()

instance BuildConMap (Alt a) where
  build (ACon _ c vs e) = insert c (length vs) >> build e
  build (ADef _ v e) = build e
 
