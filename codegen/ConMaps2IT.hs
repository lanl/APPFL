
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ConMaps2IT (
  conmaps2IT
) where

import Control.Monad.State
import qualified Data.Map as Map

import AST
import ADT
import InfoTab
import Parser

conmaps2IT defs = let (defs',conmaps) = updatedata defs
                      (ts, os) = splitDefs defs'
                      os' = fst $ updateIT os conmaps
                  in unsplitDefs(ts,os')
                         
updateIT :: ConMaps2IT a => a -> ConMaps -> (a, ConMaps)
updateIT objs = runState (update objs) 
     
check c arity cmap =
    case Map.lookup c cmap of
      Nothing -> error "use of unknown constructor"
      Just (arity',_,_,_) -> if arity == arity' then cmap  
                             else error "CON arity mismatch!"

class ConMaps2IT t where 
    update :: t -> State ConMaps t
     
instance ConMaps2IT [Obj InfoTab] where
    update = mapM update

instance ConMaps2IT (Obj InfoTab) where
    update o@(FUN {e}) = do
      e' <- update e
      return o{e = e'}

    update o@(THUNK {e}) = do
      e' <- update e
      return o{e = e'}

    update o@(CON {c, as}) = do
      (tmap,dmap) <- get
      let dmap' = check c (length as) dmap
      let Just (a,t,_,_) = Map.lookup c dmap'
      let md = omd o  -- could do a one-liner...
      let md' = md{tag = t}
      let o' = o{omd = md'}
      return o'

    update o = return o     -- PAP, BLACKHOLE

instance ConMaps2IT (Expr InfoTab) where
    update e@(ELet {edefs, ee}) = do
      edefs' <- update edefs
      ee' <- update ee
      return e{edefs = edefs', ee = ee'}

    update e@(ECase {ee, ealts}) = do
      ee' <- update ee
      ealts' <- update ealts
      return e{ee = ee', ealts = ealts'}

    update o = return o -- EAtom, EFCall, EPrimop


instance ConMaps2IT (Alts InfoTab) where
    update a@(Alts {alts}) = do
       alts' <- update alts
       return a{alts = alts'}

instance ConMaps2IT [Alt InfoTab] where
    update = mapM update

instance ConMaps2IT (Alt InfoTab) where
  update a@(ACon {ac, avs, ae}) = do
      (tmap, dmap) <- get
      let dmap' = check ac (length avs) dmap 
      ae' <- update ae
      let md = amd a -- yes we could roll 3 lines into 1, but we won't
      let md' = md{dconMap = dmap', tconMap = tmap}
      return a{amd = md', ae = ae'}

  update a@(ADef {ae}) = do
      ae' <- update ae
      return a{ae = ae'}

