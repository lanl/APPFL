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
getConMap objs = execState (build objs) Map.empty           
              
-- insert map entry if it does not exist, if it exists check arity
insert :: Con -> Int -> State ConMap ()
insert c arity = do 
                    cmap <- get
                    case Map.lookup c cmap of
                      Nothing -> put $ Map.insert c (arity, Map.size cmap) cmap
                      (Just (arity',_)) -> if arity == arity' then return () 
                                             else error "CON arity mismatch!"

class BuildConMap t where build :: t -> State ConMap ()

instance BuildConMap [Obj a] where
  build (o:os) =  build o >> build os 
  build [] = return ()

instance BuildConMap (Obj a) where
  build (FUN {e}) = build e
  build (CON {c, as}) = insert c (length as)
  build (THUNK {e}) =  build e
  build _ = return ()

instance BuildConMap (Expr a) where
  build (ELet {edefs, ee}) =  build edefs >> build ee
  build (ECase {ee, ealts}) = build ee >> build ealts
  build _ = return ()

instance BuildConMap [Alt a] where
  build (alt:alts) = build alt >> build alts
  build [] = return ()

instance BuildConMap (Alt a) where
  build (ACon {ac, avs, ae}) = insert ac (length avs) >> build ae
  build (ADef {ae}) = build ae
 
