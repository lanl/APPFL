-- build a map from constructor names -> (arity,tag)

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ConMap2 (
  setConMap
) where

import Control.Monad.State
import qualified Data.Map as Map

import Parser
import InfoTab

type ConMap = Map.Map Con (Int, Int)
 
setConMap objs =
    let (objs', (_ , conmap)) = runState (build objs) (conmap, Map.empty)
    in objs'

insert c arity cmap =
    case Map.lookup c cmap of
      Nothing -> Map.insert c (arity, Map.size cmap) cmap
      Just (arity',tag) -> if (arity == arity') then cmap 
                           else error "CON arity mismatch!"

-- the first ConMap is read-only
class BuildConMap t where 
    build :: t -> State (ConMap,ConMap) t

instance BuildConMap [Obj InfoTab] where
    build = mapM build

instance BuildConMap (Obj InfoTab) where
    build o@(FUN {e}) = do
      e' <- build e
      return o{e = e'}

    build o@(THUNK {e}) = do
      e' <- build e
      return o{e = e'}

    build o@(CON {c, as}) = do
      (mapread, mapwrite) <- get
      let mapwrite' = insert c (length as) mapwrite
      put (mapread, mapwrite')
      let Just (a,t) = Map.lookup c mapread
      let md = omd o  -- could do a one-liner...
      let md' = md{tag = t}
      let o' = o{omd = md'}
      return o'

    build o = return o     -- PAP, BLACKHOLE

instance BuildConMap (Expr InfoTab) where
    build e@(ELet {edefs, ee}) = do
      edefs' <- build edefs
      ee' <- build ee
      return e{edefs = edefs', ee = ee'}

    build e@(ECase {ee, ealts}) = do
      ee' <- build ee
      ealts' <- build ealts
      return e{ee = ee', ealts = ealts'}

    build o = return o -- EAtom, EFCall, EPrimop

instance BuildConMap [Alt InfoTab] where
    build = mapM build

instance BuildConMap (Alt InfoTab) where
  build a@(ACon {ac, avs, ae}) = do
      (mapread, mapwrite) <- get
      let mapwrite' = insert ac (length avs) mapwrite
      put (mapread, mapwrite')
      ae' <- build ae
      let md = amd a -- yes we could roll 3 lines into 1, but we won't
      let md' = md{conMap = mapread}
      return a{amd = md', ae = ae'}

  build a@(ADef {ae}) = do
      ae' <- build ae
      return a{ae = ae'}

