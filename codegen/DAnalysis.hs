{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}

module DAnalysis
(exhaustCases,
 propKnownCalls,
 setHeapAllocs,
 addDefsToEnv)
where

import ADT
import AST
import InfoTab
import CMap
import PPrint
import qualified Data.Map as Map
import Debug.Trace


-- known function analysis identifies PAPs and EFCalls that reference
-- known functions for the purpose of estimating whether an expression
-- grows the heap.
-- The analysis also examines expressions to see if they evaluate to
-- a known function. This may be detrimental to the end goal though:
-- Consider
-- f = FUN (a_0 .. a_n -> e); -- f is known top level function
-- g = THUNK(let { x = THUNK(f) } in x); -- g is considered known call to f
-- h = THUNK(case g a_0 ..  a_n of { x -> e });
-- scrutinee of case expression is a known call to f.  If f does not grow
-- the heap, naive analysis would suggest the scrutinee does not either,
-- but that would be wrong, due to the letexpr in g

type Env = Map.Map Var InfoTab


-- remove entries from the Env matching a list of Var keys
deleteAll :: Ord k => [k] -> Map.Map k v -> Map.Map k v
deleteAll vs env = foldr (Map.delete) env vs


-- Check an expression to see if it evaluates to a known function and return
-- that function's InfoTab in Maybe form, if so.
-- This could use some reorganization, probably
knownFunExprIT :: Env -> Expr InfoTab -> Maybe InfoTab
knownFunExprIT env e =
  let
    -- check Alts in Alts block with only one alternative, bind ADef var if
    -- necessary in Env and examine Alt expression for known function
    checkAlts env e [ADef{av, ae}] = case knownFunExprIT env e of
      Just it -> knownFunExprIT (Map.insert av it env) ae
      Nothing -> knownFunExprIT env ae
    -- must delete bindings in Con from Env before lookup
    checkAlts env _ [ACon{avs, ae}] = knownFunExprIT (deleteAll avs env) ae

    -- remove let bindings from env. Add bindings for any object that
    -- evaluates to a known function f, e.g. PAP(f) or THUNK(e) where e == f
    -- Can't use addDefsToEnv, because we don't want local FUN definitions
    -- added. They would then be considered known functions and could misrepresent
    -- the letexpr as a known function.
    -- Not currently in use. Let exprs not examined.
    addLetDefs env defs =
      let env' = deleteAll (map oname defs) env
          fun THUNK{e, oname} env = case knownFunExprIT env e of
            Just it -> Map.insert oname it env
            Nothing -> env
          fun PAP{f, as = [], oname} env = case Map.lookup f env of
            Just it -> Map.insert oname it env
            Nothing -> env
          fun _ env = env
      in
       foldr fun env' defs
  in
   case e of
    EAtom {ea = Var v} -> Map.lookup v env
   
    ECase {ee, ealts = Alts{alts}} -> checkAlts env ee alts

    EFCall {emd, ev, eas} -> case Map.lookup ev env of
                              Just i@Fun{arity} | length eas < arity -> Just i
                                                | otherwise -> Nothing
                              _ -> Nothing

--    ignore Let exprs, since it's self defeating given the purpose of
--    identifying known functions (let exprs always grow heap, regardless
--    of what they evaluate to)
--    ELet  {edefs, ee} ->  knownFunExprIT (addLetDefs env edefs) ee
                                                     
    _ -> Nothing


propKnownCalls :: [Obj InfoTab] -> [Obj InfoTab]
propKnownCalls objs =
  let
    env = addDefsToEnv objs Map.empty
  in
    map (propCallsObj env) objs
  
addDefsToEnv defs env =
  let
    f FUN{omd, oname} env = Map.insert oname omd env
    f THUNK{e, oname} env = case knownFunExprIT env e of
                             Just it -> Map.insert oname it env
                             Nothing -> env
                         
    f PAP{omd, f, oname} env = case Map.lookup f env of
                                Just it -> Map.insert oname it env
                                Nothing -> env
    f _ env = env
    env' = foldr f env defs
  in
    -- fix it
    case env == env' of
     True -> env
     False -> addDefsToEnv defs env'


propCallsObj :: Env -> Obj InfoTab -> Obj InfoTab
propCallsObj env o = case o of

  FUN{e, vs}    -> o { e = propCallsExpr env e }

  PAP{omd, f}   -> let omd' = omd { knownCall = Map.lookup f env }
                   in o { omd = omd'}

  THUNK{omd, e} -> o { e = propCallsExpr env e }

  _ -> o
    

propCallsExpr :: Env -> Expr InfoTab -> Expr InfoTab
propCallsExpr env e = case e of

  ELet{edefs, ee} ->
    let
      env' = addDefsToEnv edefs env
      edefs' = map (propCallsObj env') edefs
      ee' = propCallsExpr env' ee
    in  e{ee = ee', edefs = edefs'}

  ECase{ealts, ee} ->
    let ealts' = ealts{ alts = map (propCallsAlt env ee) (alts ealts) }
        ee' = propCallsExpr env ee
    in e{ealts = ealts', ee = ee' }

  EFCall{emd, ev} ->
    let emd' = emd{ knownCall = Map.lookup ev env }
    in  e{ emd = emd' }
       
  _ -> e


propCallsAlt :: Env -> Expr InfoTab -> Alt InfoTab -> Alt InfoTab
propCallsAlt env expr a = case a of
  
  ACon{amd, avs, ae} ->
    let env' = foldr Map.delete env avs
    in a{ ae = propCallsExpr env' ae }
  
  ADef{amd, av, ae}  -> 
    let env' = case knownFunExprIT env expr of
          Just it -> Map.insert av it env -- if ee is known function, bind var in env
          Nothing -> Map.delete av env -- else honor shadowing and delete it
    in a { ae = propCallsExpr env' ae}
       

type FunMap = Map.Map Var Bool

class SetHAs a where
  setHAs :: FunMap -> CMap -> a -> a

setHeapAllocs :: [Obj InfoTab] -> CMap -> [Obj InfoTab]
setHeapAllocs defs cmap =
  let
    fmp = addDefsToMap defs Map.empty cmap
  in
   map (setHAs fmp cmap) defs

instance SetHAs (Obj InfoTab) where
  setHAs fmp cmap o = case o of
    FUN{vs, e} ->
      let e' = setHAs (deleteAll vs fmp) cmap e
      in o { e = e' }

    THUNK{e} ->
      let e' = setHAs fmp cmap e
      in o { e = e' }

    _  -> o

instance SetHAs (Expr InfoTab) where
  setHAs fmp cmap e = case e of
    EAtom{emd} ->
      let emd' = emd {noHeapAlloc = not $ growsHeap fmp cmap e}
      in e { emd = emd' }

    ECase{emd, ee, ealts} ->
      let
        ee' = setHAs fmp cmap ee
        emd' = emd {noHeapAlloc = not $ growsHeap fmp cmap e}
        ealts' = setHAs fmp cmap ealts
      in
       e{ emd = emd',
          ee = ee',
          ealts = ealts' }

    ELet{emd, ee, edefs} ->
      let
        fmp' = addDefsToMap edefs fmp cmap
        ee' = setHAs fmp' cmap ee
        emd' = emd { noHeapAlloc = False } -- Let exprs always grow heap
        edefs' = map (setHAs fmp' cmap) edefs
      in
       e{ emd = emd',
          ee = ee',
          edefs = edefs' }

    EFCall{emd, eas} ->
      let
        fHA = not $ growsHeap fmp cmap e
        vsHA = or $ map (growsHeap fmp cmap) eas
        emd' = emd {noHeapAlloc = not $ fHA || vsHA}
      in e { emd = emd' }

    _ ->
      let emd' = (emd e) {noHeapAlloc = not $ growsHeap fmp cmap e}
      in e { emd = emd' }

instance SetHAs (Alts InfoTab) where
  setHAs fmp cmap a@Alts{alts} =
    let alts' = map (setHAs fmp cmap) alts
    in a { alts = alts' }


instance SetHAs (Alt InfoTab) where
  setHAs fmp cmap a = case a of

    ADef{av, ae} ->
      let
        fmp' = Map.delete av fmp
        ae' = setHAs fmp' cmap ae
      in
       a { ae = ae' }

    ACon{avs, ae} ->
      let
        fmp' = deleteAll avs fmp
        ae' = setHAs fmp' cmap ae
      in
       a { ae = ae' }
      

class GrowsHeap a where
  growsHeap :: FunMap -> CMap -> a -> Bool
  
instance GrowsHeap (Expr InfoTab) where
  growsHeap funmap cmap expr = case expr of

    EAtom{emd, ea} ->
      case ea of
       Var v -> case typ emd of
                 MCon c _ -> isBoxedTCon c cmap
                 MPrim _  -> False
                 _ -> True
       _ -> False -- literals are unboxed

    ELet{} -> True

    ECase{ee, ealts = Alts{alts}} ->
      let
        eGH = growsHeap funmap cmap ee
        aGH = or $ map (growsHeap funmap cmap) alts
      in
       eGH || aGH

    -- TODO: Check knownCall hereish (or in setHAs?)
    EFCall{ev} -> case Map.lookup ev funmap of 
      Just b -> b
      Nothing -> True -- assume unknown function grows heap

    _ -> False

instance GrowsHeap (Alt InfoTab) where
  growsHeap fmp cmap a = case a of
    ADef{av, ae}  -> growsHeap (Map.delete av fmp) cmap ae
    ACon{avs, ae} -> growsHeap (deleteAll avs fmp) cmap ae
                 

addDefsToMap defs funmap cmap =
  let
    fmp  = deleteAll (map oname defs) funmap -- remove shadowed bindings 
    toAdd = Map.fromList $ map ((,False) . oname) $ filter isFun defs -- FUNs to add
    fmp' = Map.union toAdd fmp 
    isFun o = case o of
      FUN{} -> True
--      PAP{} -> True -- don't want PAPs for now
      _ -> False
    
    foldfunc def fmp = case def of

      FUN{vs, e, oname} ->
        Map.insert oname (growsHeap (deleteAll vs fmp) cmap e) fmp

      _ -> fmp

    -- iterate until fixed point is found
    fixDefs defs fmp =
      let fmp' = foldr foldfunc fmp defs in
       if fmp == fmp'
       then fmp'
       else fixDefs defs fmp'
  in
   fixDefs defs fmp'
      
       





























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
    fcal = EFCall{emd   = mdErr "EFCall"}
  in
   undefined
