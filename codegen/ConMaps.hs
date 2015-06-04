
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}

module ConMaps (
  setConmaps,
  buildConmaps
) where

import Data.Maybe
import Control.Monad.State
import qualified Data.Map as Map

import AST
import ADT
import InfoTab

-- starting tycon map
tyconmap :: TyConMap
tyconmap = Map.insert "Bool#" (TyConParam 0 0 False ["False_h","True_h"])
         $ Map.insert "Int_h"  (TyConParam 0 1 False [])
         $ Map.insert "Bool"  (TyConParam 0 2 True ["B"]) -- data Bool = B Bool#
         $ Map.insert "Int"   (TyConParam 0 3 True ["I"]) -- data Int = I Int#
         $ Map.insert "List"  (TyConParam 1 4 True ["Nil","Cons"]) -- data List a = Nil | Cons a (List a)
           Map.empty

-- starting datacon map
-- these tags must match what is in stg_header.h
dataconmap :: DataConMap
dataconmap = Map.insert "False_h" (DataConParam 0 0 False "Bool#") 
           $ Map.insert "True_h"  (DataConParam 0 1 False "Bool#") 
           $ Map.insert "B"     (DataConParam 1 2 True "Bool") 
           $ Map.insert "I"     (DataConParam 1 3 True "Int") 
           $ Map.insert "Unit"  (DataConParam 0 4 True "Unit")
           $ Map.insert "Nil"   (DataConParam 0 5 True "List") 
           $ Map.insert "Cons"  (DataConParam 2 6 True "List")
             Map.empty

setConmaps :: ConMaps2IT t => ([TyCon], t) -> ([TyCon], t)
setConmaps (tycons, objs) = let (tycons', conmaps) = buildConmaps tycons
                                objs' = fst $ runState (updateit objs) conmaps
                            in (tycons', objs')

 -- from TyCons build the ConMaps                                       
buildConmaps :: [TyCon] -> ([TyCon], ConMaps)                   
buildConmaps inp = (update (fst conmaps) inp, conmaps)
                 where conmaps = execState (build [] inp) (tyconmap, dataconmap)

class BuildConMaps t where build :: String -> t -> State ConMaps()
                                                    
instance BuildConMaps a => BuildConMaps [a] where
  build = mapM_ . build

instance BuildConMaps TyCon where
  build _ (TyCon boxed con tyvars dcons) 
    = tyconinsert con (length tyvars) boxed >> 
      build con dcons
     
instance BuildConMaps DataCon where
  build tycon (DataCon boxed con mts) = 
    dataconinsert con (length mts) boxed tycon
  
dataconinsert :: String -> Int -> Bool -> Con -> State ConMaps ()
dataconinsert con arity boxed tycon
  = do 
      (tmap, dmap) <- get
      let dmap' = Map.insert con DataConParam{darity = arity, 
                                             dtag = Map.size dmap, 
                                             dboxed = boxed, 
                                             dtycon = tycon} 
                                             dmap
      -- update tycon w/ this data con                                       
      let Just (TyConParam {tarity, ttag, tboxed, tdatacons}) = Map.lookup tycon tmap                                        
      let tmap' = Map.insert tycon TyConParam{tarity = tarity, 
                                     ttag = ttag, 
                                     tboxed = tboxed,
                                     tdatacons = (con:tdatacons)}
                                     tmap
      put(tmap',dmap')
    
    
tyconinsert :: String -> Int -> Bool -> State ConMaps ()
tyconinsert con arity boxed 
  = do 
      (tmap, dmap) <- get
      put (Map.insert con TyConParam{tarity = arity, 
                                     ttag = Map.size tmap, 
                                     tboxed = boxed,
                                     tdatacons = []}
                                     tmap, dmap)     

--update boxedness of MCons                                     
class Update t where update :: TyConMap -> t -> t              

instance Update a => Update [a] where
  update = map . update

instance Update (Def a) where
  update m (DataDef tycon) = DataDef(update m tycon)
  update _ x  = x

instance Update TyCon where
  update m (TyCon boxed c tvs dcs) = 
    TyCon boxed c tvs (update m dcs)
 
instance Update DataCon where 
  update m (DataCon boxed c mts) = 
    DataCon boxed c (update m mts)

instance Update Monotype where
  update m (MVar x) = MVar x
  update m (MFun x y) = MFun (update m x) (update m y)
  update m (MCon _ c xs) = MCon (isboxed m c) c (update m xs)

isboxed :: TyConMap -> Con -> Bool
isboxed m c  = let lookup = Map.lookup c m
                           in if (isNothing lookup) then 
                                error ("unknown type constructor " ++ c ++ " used")
                              else (\(TyConParam{tboxed=b}) -> b) (fromJust lookup)   

                 
check :: String -> Int -> DataConMap -> DataConMap
check c arity cmap =
    case Map.lookup c cmap of
      Nothing -> error ("use of unknown constructor " ++ c)
      Just (DataConParam{darity}) -> if arity == darity then cmap  
                                     else error ("CON arity mismatch! for " 
                                     ++ c ++ " " ++ show arity ++ " != " ++ show arity)

class ConMaps2IT t where 
    updateit :: t -> State ConMaps t
     
instance ConMaps2IT [Obj InfoTab] where
    updateit = mapM updateit

instance ConMaps2IT (Obj InfoTab) where
    updateit o@(FUN {e}) = do
      e' <- updateit e
      return o{e = e'}

    updateit o@(THUNK {e}) = do
      e' <- updateit e
      return o{e = e'}

    updateit o@(CON {c, as}) = do
      (tmap,dmap) <- get
      let dmap' = check c (length as) dmap
      let Just (DataConParam{dtag}) = Map.lookup c dmap'
      let md = omd o  -- could do a one-liner...
      let md' = md{tag = dtag}
      let o' = o{omd = md'}
      return o'

    updateit o = return o     -- PAP, BLACKHOLE

instance ConMaps2IT (Expr InfoTab) where
    updateit e@(ELet {edefs, ee}) = do
      edefs' <- updateit edefs
      ee' <- updateit ee
      return e{edefs = edefs', ee = ee'}

    updateit e@(ECase {ee, ealts}) = do
      ee' <- updateit ee
      ealts' <- updateit ealts
      return e{ee = ee', ealts = ealts'}

    updateit o = return o -- EAtom, EFCall, EPrimop

instance ConMaps2IT (Alts InfoTab) where
    updateit a@(Alts {alts}) = do
       alts' <- updateit alts
       return a{alts = alts'}

instance ConMaps2IT [Alt InfoTab] where
    updateit = mapM updateit

instance ConMaps2IT (Alt InfoTab) where
  updateit a@(ACon {ac, avs, ae}) = do
      (tmap, dmap) <- get
      let dmap' = check ac (length avs) dmap 
      ae' <- updateit ae
      let md = amd a -- yes we could roll 3 lines into 1, but we won't
      let md' = md{dconMap = dmap', tconMap = tmap}
      return a{amd = md', ae = ae'}

  updateit a@(ADef {ae}) = do
      ae' <- updateit ae
      return a{ae = ae'}

