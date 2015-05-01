
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module CheckCon (
  checkCon
) where

import Control.Monad.State
import qualified Data.Map as Map

import AST
import ADT
import InfoTab
import Parser

checkCon (defs, (tyconmap, dataconmap)) = 
  runState (check defs) dataconmap
     
checkit c arity cmap =
    case Map.lookup c cmap of
      Nothing -> error "use of unknown constructor"
      Just (arity',_,_,_) -> if arity == arity' then cmap  
                             else error "CON arity mismatch!"

class CheckCon t where 
    check :: t -> State DataConMap t

instance CheckCon [Defs InfoTab] where
    check = mapM check
    
instance CheckCon (Defs InfoTab) where
    check (ObjDefs o) = error ""
    
    
instance CheckCon [Obj InfoTab] where
    check = mapM check

instance CheckCon (Obj InfoTab) where
    check o@(FUN {e}) = do
      e' <- check e
      return o{e = e'}

    check o@(THUNK {e}) = do
      e' <- check e
      return o{e = e'}

    check o@(CON {c, as}) = do
      cmap <- get
      let cmap' = checkit c (length as) cmap
      let Just (a,t,_,_) = Map.lookup c cmap'
      let md = omd o  -- could do a one-liner...
      let md' = md{tag = t}
      let o' = o{omd = md'}
      return o'

    check o = return o     -- PAP, BLACKHOLE

instance CheckCon (Expr InfoTab) where
    check e@(ELet {edefs, ee}) = do
      edefs' <- check edefs
      ee' <- check ee
      return e{edefs = edefs', ee = ee'}

    check e@(ECase {ee, ealts}) = do
      ee' <- check ee
      ealts' <- check ealts
      return e{ee = ee', ealts = ealts'}

    check o = return o -- EAtom, EFCall, EPrimop


instance CheckCon (Alts InfoTab) where
    check a@(Alts {alts}) = do
       alts' <- check alts
       return a{alts = alts'}

instance CheckCon [Alt InfoTab] where
    check = mapM check

instance CheckCon (Alt InfoTab) where
  check a@(ACon {ac, avs, ae}) = do
      cmap <- get
      let cmap' = checkit ac (length avs) cmap 
      ae' <- check ae
      let md = amd a -- yes we could roll 3 lines into 1, but we won't
      let md' = md{dconMap = cmap'}
      return a{amd = md', ae = ae'}

  check a@(ADef {ae}) = do
      ae' <- check ae
      return a{ae = ae'}

