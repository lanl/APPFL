{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}

module DAnalysis
(exhaustCases,
 propKnownCalls,
 addDefsToEnv)
where

import ADT
import AST
import InfoTab
import CMap
import PPrint
import qualified Data.Map as Map
import Debug.Trace



type Env = Map.Map Var InfoTab

isFUN x = case x of
           FUN{} -> True
           _     -> False
isPAP x = case x of
           PAP{} -> True
           _     -> False

propKnownCalls :: [Obj InfoTab] -> [Obj InfoTab]
propKnownCalls objs =
  let
    env = addDefsToEnv objs Map.empty
  in
    map (propCallsO env) objs

propCallsO :: Env -> Obj InfoTab -> Obj InfoTab
propCallsO env o = case o of

  FUN{e, vs}    -> o { e = propCallsE env e }

  PAP{omd, f}   -> o {omd = omd{ knownCall = Map.lookup f env} }

  THUNK{omd, e} -> o { e = propCallsE env e }

  _ -> o
    

propCallsE :: Env -> Expr InfoTab -> Expr InfoTab
propCallsE env e = case e of

  ELet{edefs, ee} ->
    let
      env' = addDefsToEnv edefs env
      edefs' = map (propCallsO env') edefs
      ee' = propCallsE env' ee
    in  e{ee = ee', edefs = edefs'}

  ECase{ealts, ee} ->
    let ealts' = ealts{ alts = map (propCallsA env) (alts ealts) }
        ee' = propCallsE env ee
    in e{ealts = ealts', ee = ee' }

  EFCall{emd, ev} ->
    let emd' = emd{ knownCall = Map.lookup ev env }
    in  e{ emd = emd' }
       
  _ -> e

showEnv env = show $ vcat $ map (\(v,it) -> braces (text v $+$ toDoc it)) (Map.toList env)

propCallsA :: Env -> Alt InfoTab -> Alt InfoTab
propCallsA env a = case a of
  
  ACon{amd, avs, ae} ->
    let env' = foldr Map.delete env avs
    in a{ ae = propCallsE env' ae }
  
  ADef{amd, av, ae}  ->
    let env' = Map.delete av env
    in a { ae = propCallsE env' ae}
       

addDefsToEnv defs env =
  let
    filtered = filter (\o-> isCallable o) defs
    defMap   = Map.fromList $ map (\o-> (oname o, omd o)) filtered
  in
   Map.union defMap env

isCallable o = case o of
  PAP{}    -> True
  
  FUN{}    -> True
  
  THUNK{e} -> case typ (emd e) of
               MFun _ _ -> True
               _ -> False
               
  _        -> False




exhaustCases :: CMap -> [Obj a] -> [Obj a]
exhaustCases cmap = map (exhaustObj cmap)

exhaustObj :: CMap -> Obj a -> Obj a
exhaustObj cmap obj =
  case obj of
   ff@FUN{e} -> ff{e = exhaustExpr cmap e}
   tt@THUNK{e} -> tt{e = exhaustExpr cmap e}
   _ -> obj
   
exhaustExpr :: CMap -> Expr a -> Expr a
exhaustExpr cmap expr =
  case expr of
   el@ELet{edefs, ee} -> el{edefs = exhaustCases cmap edefs, ee = exhaustExpr cmap ee}
   ec@ECase{ealts} -> ec{ealts = exhaustAlts cmap ealts}
   _ -> expr

exhaustAlts :: CMap -> Alts a -> Alts a
exhaustAlts cmap aa@Alts{alts, aname} =
  let acons = filter isACon alts
      adefs = filter (not.isACon) alts
      isACon x = case x of ACon{} -> True; _ -> False
      newAlts = if not (null adefs) || consExhaust (map ac acons) cmap
                then alts
                else alts ++ [defAult "exh"]  --get it?
  in aa{alts = newAlts}


defAult :: String -> Alt a
defAult name =
  let
    mdErr s = error $ s ++ " metadeta not set!"
    fcal = EFCall{emd   = mdErr "EFCall",
                  ev    = "stg_case_not_exhaustive",
                  eas   = [Var "x"]}
    eatm = EAtom {emd   = mdErr "EAtom",
                  ea    = Var (name ++ "_exhaust")}
    thnk = THUNK {omd   = mdErr "THUNK",
                  e     = fcal,
                  oname = name ++ "_exhaust"}
    letx = ELet  {emd   = mdErr "ELet",
                  edefs = [thnk],
                  ee    = eatm}
  in ADef{amd = mdErr "ADef",
          av  = "x",
          ae  = letx}
