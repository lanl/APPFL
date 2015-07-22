{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE PatternGuards        #-}
{-# LANGUAGE RecordWildCards      #-} -- implies DisambiguateFieldRecords

module Analysis
(exhaustCases,
 propKnownCalls,
 setHeapAllocs)
where

import ADT
import AST
import InfoTab
import CMap
import PPrint
import qualified Data.Map as Map
import Data.List (group)
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

-- KCMap: Knowncall map of Function Name to Fun InfoTab
type KCMap = Map.Map Var InfoTab


-- remove entries from the KCMap matching a list of Var keys
deleteAll :: Ord k => [k] -> Map.Map k v -> Map.Map k v
deleteAll vs env = foldr (Map.delete) env vs




propKnownCalls :: [Obj InfoTab] -> [Obj InfoTab]
propKnownCalls objs =
  let
    env = addDefsToKCMap objs Map.empty
  in
    map (propCallsObj env) objs
  
addDefsToKCMap defs env =
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
     False -> addDefsToKCMap defs env'


propCallsObj :: KCMap -> Obj InfoTab -> Obj InfoTab
propCallsObj env o = case o of

  FUN{e, vs}    -> o { e = propCallsExpr env e }

  PAP{omd, f}   -> let omd' = omd { knownCall = Map.lookup f env }
                   in o { omd = omd'}

  THUNK{omd, e} -> o { e = propCallsExpr env e }

  _ -> o
    

propCallsExpr :: KCMap -> Expr InfoTab -> Expr InfoTab
propCallsExpr env e = case e of

  ELet{edefs, ee} ->
    let
      env' = addDefsToKCMap edefs env
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


propCallsAlt :: KCMap -> Expr InfoTab -> Alt InfoTab -> Alt InfoTab
propCallsAlt env scrut a = case a of
  
  ACon{amd, avs, ae} ->
    let env' = deleteAll avs env
    in a{ ae = propCallsExpr env' ae }
  
  ADef{amd, av, ae}  -> 
    let env' = case knownFunExprIT env scrut of
          Just it -> Map.insert av it env -- if scrut is known function, bind var in env
          Nothing -> Map.delete av env -- else honor shadowing and delete it
    in a { ae = propCallsExpr env' ae}
       

type FunMap = Map.Map Var Bool

class SetHA a where
  setHA :: FunMap -> a -> a
  getHA :: a -> Bool

setHeapAllocs :: [Obj InfoTab] -> [Obj InfoTab]
setHeapAllocs defs =
  let
    fmp = addDefsToMap defs Map.empty
  in
   map (setHA fmp) defs

instance SetHA (Obj InfoTab) where
  setHA fmp o = case o of
    FUN{vs, e} ->
      let e' = setHA (deleteAll vs fmp) e
      in o { e = e' }

    THUNK{e} ->
      let e' = setHA fmp e
      in o { e = e' }

    _  -> o

  getHA o = case o of
    FUN{e}   -> getHA e
    THUNK{e} -> getHA e
    _ -> error $ "DAnalysis.getHA " ++ (show $ rawDocObj o)

instance SetHA (Expr InfoTab) where
  setHA fmp e = case e of
    EAtom{emd} ->
      let nha = case typ emd of
                 MCon b c _ -> b
                 MPrim _    -> True
                 -- MVar => polymorphic => boxed?
                 -- MFun => PAP created dynamically? => boxed
                 _        -> False
          emd' = emd {noHeapAlloc = nha}
      in e { emd = emd' }

    ECase{emd, ee, ealts} ->
      let
        ee' = setHA fmp ee
        ealts' = setHA fmp ealts
        nha = getHA ee' && getHA ealts'
        emd' = emd {noHeapAlloc = nha}
      in
       e{ emd = emd',
          ee = ee',
          ealts = ealts' }

    ELet{emd, ee, edefs} ->
      let
        fmp' = addDefsToMap edefs fmp
        ee' = setHA fmp' ee
        edefs' = map (setHA fmp') edefs
        emd' = emd { noHeapAlloc = False } -- Let exprs always grow heap
      in
       e{ emd = emd',
          ee = ee',
          edefs = edefs' }

    EFCall{emd, ev, eas} ->
      let
        eas' = map (setHA fmp) eas
        fnha = case knownCall emd of
          Just it -> 
            case Map.lookup (name it) fmp of
             Just b -> b 
             Nothing -> False -- unknown function may grow heap
          Nothing ->
            -- this works independently of knownCall analysis (for simple tests)
            -- it shouldn't break anything in its presence either, I think.
            case Map.lookup ev fmp of
             Just b -> b
             Nothing -> False
             
        nha = fnha -- and (fnha:map getHA eas') -- ignoring args for now
        emd' = emd {noHeapAlloc = nha}
      in e { emd = emd' }

    EPrimop{emd, eas} ->
      let eas' = map (setHA fmp) eas -- for consistency
          nha = and $ map getHA eas -- for consistency
          emd' = emd {noHeapAlloc = True} -- hack, typechecker not working?
      in e { emd = emd' }

  getHA = noHeapAlloc.emd
    

instance SetHA (Alts InfoTab) where
  setHA fmp a@Alts{alts} =
    let alts' = map (setHA fmp) alts
    in a { alts = alts' }

  getHA Alts{alts} = and $ map getHA alts


instance SetHA (Alt InfoTab) where
  setHA fmp a = case a of

    ADef{av, ae} ->
      let
        fmp' = Map.delete av fmp
        ae' = setHA fmp' ae
      in
       a { ae = ae' }

    ACon{avs, ae} ->
      let
        fmp' = deleteAll avs fmp
        ae' = setHA fmp' ae
      in
       a { ae = ae' }

  getHA = getHA . ae
      

addDefsToMap defs funmap =
  let
    fmp  = deleteAll (map oname defs) funmap -- remove shadowed bindings 
    toAdd = Map.fromList $ map ((,True) . oname) $ filter isFun defs
    fmp' = Map.union toAdd fmp 
    isFun o = case o of
      FUN{} -> True
--      PAP{} -> -- don't want PAPs for now
      _ -> False
    
    foldfunc def fmp = case def of

      FUN{vs, e, oname} ->
        Map.insert oname (getHA def) fmp

      _ -> fmp

    -- iterate until fixed point is found
    fixDefs defs fmp =
      let
          fmp' = foldr foldfunc fmp $ map (setHA fmp) defs in
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
   FUN{e} -> obj {e = exhaustExpr cmap e}
   THUNK{e} -> obj {e = exhaustExpr cmap e}
   PAP{} -> obj
   CON{} -> obj
   BLACKHOLE{} -> obj
   
exhaustExpr :: CMap -> Expr a -> Expr a
exhaustExpr cmap expr =
  case expr of
   ELet{edefs, ee} -> expr {edefs = exhaustCases cmap edefs,
                            ee = exhaustExpr cmap ee}
                      
   ECase{ealts} -> expr {ealts = exhaustAlts cmap ealts}
   
   EAtom{}   -> expr
   EPrimop{} -> expr
   EFCall{}  -> expr

exhaustAlts :: CMap -> Alts a -> Alts a
exhaustAlts cmap aa@Alts{alts, aname} =
  let acons = filter isACon alts
      adefs = filter (not.isACon) alts
      isACon x = case x of ACon{} -> True; _ -> False
      newAlts = if not (null adefs) || consExhaust (map ac acons) cmap
                then alts
                else alts ++ [defAlt aname]  --get it?
  in aa{alts = newAlts}


defAlt :: String -> Alt a
defAlt name =
  let
    mdErr s = error $ s ++ " metadeta not set!"
    arg   = EAtom{emd = mdErr "EAtom",
                  ea  = Var "x"}
    fcall = EFCall{emd = mdErr "EFCall",
                   ev  = "stg_case_not_exhaustive",
                   eas = [arg]}
    thunk = THUNK{omd   = mdErr "THUNK",
                  e     = fcall,
                  oname = name ++ "_exhaust"}
    letee = EAtom{emd = mdErr "EAtom",
                  ea  = Var $ oname thunk}
    elet  = ELet{emd   = mdErr "ELet",
                 edefs = [thunk],
                 ee    = letee}
  in ADef {amd = mdErr "ADef",
           av  = "x",
           ae  = elet}




     
-- Check an expression to see if it evaluates to a known function and return
-- that function's InfoTab in Maybe form, if so.
-- This isn't terribly useful: expressions will rarely evaluate to known
-- functions.  The only example I can think of that might be useful
-- would be case-dependent partial application of the same function
-- e.g.
-- case e of {
--   val1 -> f val1;
--   val2 -> f val2;
--   x    -> f defaultVal; }
--
-- Note that heap allocation analysis currently does not depend on this,
-- but it could at some point, and this analysis could break it by hiding
-- let expressions "inside" function calls that have been identified as
-- known
-- e.g.
-- f = FUN(a -> e) -- assume f does not grow heap
-- t = THUNK(let {x = THUNK(f)} in x )
-- main = THUNK(t arg)
-- here t could be identified as a known call to f, which
-- might have the result of identifying the expression in
-- in main's THUNK as not causing heap allocation, which it clearly should
knownFunExprIT :: KCMap -> Expr InfoTab -> Maybe InfoTab
knownFunExprIT env e =
  let
    -- check Alts in Alts block with only one alternative, bind ADef var if
    -- necessary in KCMap and examine Alt expression for known function
    checkAlts env e alts = case group $ map (checkAlt env e) alts of
      -- if group produces a list of one list, all in that list are equal
      [its] -> head its
      _ -> Nothing

    -- check single alt  
    checkAlt env e ADef{av, ae} = case knownFunExprIT env e of
      Just it -> knownFunExprIT (Map.insert av it env) ae
      Nothing -> knownFunExprIT env ae
    -- must delete bindings in Con from KCMap before lookup
    checkAlt env _ ACon{avs, ae} = knownFunExprIT (deleteAll avs env) ae
    

    -- remove let bindings from map. Add bindings for any object that
    -- evaluates to a known function f, e.g. PAP(f) or THUNK(e) where e == f
    -- Can't use addDefsToKCMap, because we don't want local FUN definitions
    -- added. They would then be considered known functions and could misrepresent
    -- the letexpr as a known function.
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

--    This will break heap allocation analysis if it depends on known calls
--    ELet  {edefs, ee} ->  knownFunExprIT (addLetDefs env edefs) ee
                                                     
    _ -> Nothing
