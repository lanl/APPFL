{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RecordWildCards      #-} -- implies DisambiguateFieldRecords
{-# LANGUAGE PatternGuards        #-}

module Analysis(
--  isSimple,
  normalize,
) where

import AST
import ADT
import InfoTab
import qualified Data.Map as Map
import Data.List(foldl')

{-
-- isSimple means will not do heap allocation
class IsSimple a where
  isSimple :: a -> Bool

instance IsSimple (Expr a) where
  isSimple EAtom{} = True
  isSimple EFCall{} = False        -- could be True pending deeper analysis
  isSimple EPrimop{} = True
  isSimple ELet{edefs, ee} = False -- assuming edefs is not empty
  isSimple ECase{ee, ealts} = isSimple ee && isSimple ealts

instance IsSimple (Alts a) where
  isSimple Alts{alts} = all isSimple alts

instance IsSimple (Alt a) where
  isSimple alt = isSimple $ ae alt
-}

-- normalize could do many things, for right now it provides a default
-- case alt if needed (in the absence of type information that means if
-- one is not present
-- NOTE:  while metadata is preserved, none is added so this should probably
--   be done early

class Normalize a where
  normalize :: a -> a --should that be a -> b? which won't work with field names?

instance Normalize (Expr a) where
  normalize e@EAtom{} = e
  normalize e@EFCall{} = e
  normalize e@EPrimop{} = e
  normalize e@ELet{edefs, ee} = e{ee = normalize ee, edefs = normalize edefs}
  normalize e@ECase{ee, ealts} = e{ee = normalize ee, ealts = normalize ealts}

instance Normalize (Alts a) where
  normalize a@Alts{alts} = 
    -- if no ADef add one
    -- if ADef followed by ACons we could remove them
    let alts' = if not (areExhaustive alts) then
                    let fcall = EFCall{emd = error "emd not initialized",
                                       ev = "stg_case_not_exhaustive", 
                                       eas = [Var "x"]}
                    in
                      alts ++ 
                      [ADef{amd = error "altsmd not initialized",
                            av = "x", 
--                          ae = fcall
                            ae = ELet{emd = error "emd not initialized",
                                      edefs = [THUNK{omd = error "emd not initialized",
                                                     e = fcall,
                                                     oname = aname a ++ "_exhaust"}],
                                     ee = EAtom{emd = error "emd not initialized",
                                                ea = Var $ aname a ++ "_exhaust"}}}]
                else alts
    in a{alts = map normalize alts'}

instance Normalize (Alt a) where
  normalize a = a{ae = normalize $ ae a}

instance Normalize (Obj a) where
  normalize o@FUN{e} = o{e = normalize e}
  normalize o@THUNK{e} = o{e = normalize e}
  -- PAP, CON, BH
  normalize o = o

instance Normalize a => Normalize [a] where
  normalize = map normalize 
 
areExhaustive = (any isConI) `pfOr` (any isADef)

isADef ADef{} = True
isADef _      = False

isConI ACon{ac} | ac == "I" = True
isConI _                    = False

pfOr p1 p2 x = (p1 x) || (p2 x)


-- set known calls in EFCall, PAP

class SetKnownCalls a where
  setKnownCalls :: Map.Map Var InfoTab -> a -> a

instance SetKnownCalls (Expr InfoTab) where
  setKnownCalls itMap e@EAtom{} = e

  setKnownCalls itMap e@EFCall{emd, ev} = e{emd = emd{knownCall = Map.lookup ev itMap}}

  setKnownCalls itMap e@EPrimop{} = e

  setKnownCalls itMap e@ELet{edefs, ee} = 
      let itMap' = Map.union (Map.fromList [(oname o, omd o) | o@(FUN{}) <- edefs]) itMap
      in e{edefs = map (setKnownCalls itMap') edefs, ee = setKnownCalls itMap' ee}

  setKnownCalls itMap e@ECase{ee, ealts} = 
      e{ee = setKnownCalls itMap ee, ealts = setKnownCalls itMap ealts}

instance SetKnownCalls (Alts InfoTab) where
    setKnownCalls itMap a@Alts{alts} = a{alts = map (setKnownCalls itMap) alts}

instance SetKnownCalls (Alt InfoTab) where
    setKnownCalls itMap a@(ACon{avs, ae}) = 
        let itMap' = foldl' (flip Map.delete) itMap avs
        in a{ae = setKnownCalls itMap' ae}
    setKnownCalls itMap a@(ADef{av, ae}) = 
        a{ae = setKnownCalls (Map.delete av itMap) ae}

instance SetKnownCalls (Obj InfoTab) where
    setKnownCalls itMap (o@FUN{vs, e}) =
        let itMap' = foldl' (flip Map.delete) itMap vs
        in o{e = setKnownCalls itMap' e}

    setKnownCalls itMap (o@PAP{omd, f}) = o{omd = omd{knownCall = Map.lookup f itMap}}

    setKnownCalls itMap (o@CON{}) = o

    setKnownCalls itMap (o@THUNK{e}) = o{e = setKnownCalls itMap e}

    setKnownCalls itMap (o@BLACKHOLE{}) = o


-- determine whether each expression could cause heap allocation
-- order True > False, then least fixed point, i.e. start with positive assumption
-- could construct domain equations but just do traversals

class SetNoHeapAllocTrue a where
  setNoHeapAllocTrue :: a -> a

instance SetNoHeapAllocTrue (Expr InfoTab) where
  setNoHeapAllocTrue e@EAtom{emd} = e{emd = emd{noHeapAlloc = True}}

  setNoHeapAllocTrue e@EFCall{emd} = e{emd = emd{noHeapAlloc = True}}

  setNoHeapAllocTrue e@EPrimop{emd} = e{emd = emd{noHeapAlloc = True}}

  setNoHeapAllocTrue e@ELet{emd, edefs, ee} = 
      e{emd = emd{noHeapAlloc = True},
        edefs = map setNoHeapAllocTrue edefs,
        ee = setNoHeapAllocTrue ee}

  setNoHeapAllocTrue e@ECase{emd, ee, ealts} = 
      e{emd = emd{noHeapAlloc = True},
        ee = setNoHeapAllocTrue ee, 
        ealts = setNoHeapAllocTrue ealts}

instance SetNoHeapAllocTrue (Alts InfoTab) where
    setNoHeapAllocTrue (a@Alts{altsmd, alts}) =
        a{altsmd = altsmd{noHeapAlloc = True},
          alts = map setNoHeapAllocTrue alts}

instance SetNoHeapAllocTrue (Obj InfoTab) where
    setNoHeapAllocTrue (o@FUN{omd, e}) =
        o{omd = omd{noHeapAlloc = True},
          e = setNoHeapAllocTrue e}

    setNoHeapAllocTrue (o@PAP{omd}) = o{omd = omd{noHeapAlloc = True}}

    setNoHeapAllocTrue (o@CON{omd}) = o{omd = omd{noHeapAlloc = True}}

    setNoHeapAllocTrue (o@THUNK{omd, e}) = 
        o{omd = omd{noHeapAlloc = True},
          e = setNoHeapAllocTrue e}

    setNoHeapAllocTrue (o@BLACKHOLE{omd}) = o{omd = omd{noHeapAlloc = True}}

instance SetNoHeapAllocTrue (Alt InfoTab) where
    setNoHeapAllocTrue a@ACon{amd, ae} =
        a{amd = amd{noHeapAlloc = True},
          ae = setNoHeapAllocTrue ae}

-- abstract interpretation!
-- env maps variables of boxed type to
-- Alt-bound
-- Let-bound
-- lambda-bound

-- Bool return value indicated whether any change, i.e. reached fixed point

data Binding = AB              -- Alt bound
             | FB              -- FUN bound
             | LB InfoTab      -- ELet bound

type Env = [(Var, Binding)]

exam v [] = Nothing
exam v ((v',_):bs) | v /= v' = exam v bs
exam v _ | last v == '#' = Just True -- it's unboxed
exam v ((_, x):_) =
    case x of
      AB -> Just False  -- Alt bound => don't know
      FB -> Just False  -- FUN bound => don't know
      LB it -> Just $ noHeapAlloc it

class AI a where
    ai :: Env -> a -> (a, Bool)

instance AI (Expr InfoTab) where
    ai env e@EAtom{emd, ea} | not $ noHeapAlloc emd = (e, False)
    ai env e@EAtom{emd, ea} | LitI{} <- ea = (e, False)
    ai env e@EAtom{emd, ea} | Var v  <- ea =
        case exam v env of
          Nothing -> error "free variable in ai"
          Just b -> let prev = noHeapAlloc emd
                    in (e{emd = emd{noHeapAlloc = b}}, b /= prev)

    ai env e@EFCall{emd, ev, eas} =
        let prev = noHeapAlloc emd
        in case knownCall emd of
             Nothing -> (e{emd = emd{noHeapAlloc = False}}, prev /= False) -- clearer
             Just it -> if arity it /= length eas then -- under- or over-saturated
                            (e{emd = emd{noHeapAlloc = False}}, prev /= False)
                        else let funnoa = noHeapAlloc it in
                            (e{emd = emd{noHeapAlloc = funnoa}}, prev /= funnoa)

-- MODIFIED 7.1 - David ----------------------------------------
{-

-- quickie boxedness propagation

-- boxed = PPoly [] $ (MCon True "Z" [])
boxed = (MCon True "Z" [])

-- unboxed = PMono $ (MCon False "Int#" [])
unboxed = (MCon False "Int#" [])

isBoxedId v = take 2 (reverse v) /= "h_"

typeByName n = if isBoxedId n then boxed else unboxed

-- trying out some of the named field tricks

class SetTypes a where
  setTypes :: a -> a

instance SetTypes (Expr InfoTab) where
  setTypes EAtom{emd = mymd, ..} =
      let emd = case ea of
                  Var v -> mymd{typ = typeByName v}
                  _     -> mymd{typ = unboxed}
      in EAtom{..}

  setTypes EFCall{emd = mymd, ..} = 
      let emd = mymd{typ = typeByName ev}
      in EFCall{..}

  setTypes e@EPrimop{emd = mymd, ..} = 
      let emd = mymd{typ = unboxed}
      in EPrimop{..}

  setTypes e@ELet{emd = mymd, edefs, ee} =
      let edefs' = map setTypes edefs
          ee' = setTypes ee
          typ' = typ $ emd ee'
          emd' = mymd{typ = typ'}
      in e{emd = emd', edefs = edefs', ee = ee'}

  setTypes e@ECase{emd = mymd, ee, ealts} =
      let ealts' = setTypes ealts
          ee' = setTypes ee
          typ' = typ $ altsmd ealts'
          emd' = mymd{typ = typ'}
      in e{emd = emd', ee = ee', ealts = ealts'}

instance SetTypes (Alts InfoTab) where
  setTypes as@Alts{altsmd = mymd, alts} = 
      let alts' = map setTypes alts
          typ' = typ $ amd $ head alts'  -- they better be all the same
          altsmd' = mymd{typ = typ'}
      in as{altsmd = altsmd', alts = alts'}
          
instance SetTypes (Alt InfoTab) where
  setTypes a =
      let ae' = setTypes (ae a)
          typ' = typ $ emd ae'
          amd' = (amd a){typ = typ'}
      in a{amd = amd', ae = ae'}

instance SetTypes (Obj InfoTab) where
    setTypes o@FUN{omd, e, oname} =
        let e' = setTypes e
            typ' = typ $ emd e'
        in if typeByName oname /= typ' then
               error "setTypes mismatch FUN"
           else o{omd = omd{typ = typ'}, e = e'}

    setTypes o@PAP{omd, f, oname} =
        let t1 = typeByName oname
            t2 = typeByName f
        in if t1 /= t2 then
               error "setTypes mismatch PAP"
           else o{omd = omd{typ = t1}}

    setTypes o@CON{omd} = o{omd = omd{typ = boxed}}

    setTypes o@THUNK{omd, e} =
        let e' = setTypes e
            typ' = typ $ emd e'
        in if typ' /= boxed then
           error "setTypes unboxed THUNK"
       else o{omd = omd{typ = typ'}}

    setTypes o@BLACKHOLE{omd} = o{omd = omd{typ = boxed}}

instance SetTypes [Obj InfoTab] where
    setTypes = map setTypes
-}
