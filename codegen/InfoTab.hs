{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InfoTab(
  InfoTab(..),
  setITs,
  showIT,
  showITType,
  showObjType
) where

import Prelude
import Parser
import Data.List(nub,(\\))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set


-- need an infoTab entry for each lexically distinct HO or SHO

-- *****************************************************

{-
  every Expr, Alt, Obj has metadata, e.g. name, freevars
-}

-- IMPORTANT REALIZATION:  need an infoTab entry for each CON(C N P P N) combination
-- for layout information, for garbage collection, when we no longer have a
-- discriminator for PtrOrLiteral -- mostly worry about this later


data InfoTab = 
    Fun { 
      name :: String,
      fvs :: [Var],
      entryCode :: String,
      arity :: Int}      
  | Pap { 
      name :: String,
      fvs :: [Var],
      entryCode :: String,
      args     :: [Atom] }
  | Con { 
      name :: String,
      fvs :: [Var],
      entryCode :: String,
      args :: [Atom],
      arity :: Int,
      con :: String, -- actual constructor name, not object name
      tag :: Int }
  | Thunk { 
      name :: String,
      fvs :: [Var],
      entryCode :: String }
  | Blackhole 
    { name :: String,
      fvs :: [Var],
      entryCode :: String }
  -- this is an umbrella for expressions for now -- need to factor this better
  | JustFVs { 
      fvs :: [Var] }
  | ConMap { 
      fvs :: [Var],
      conMap :: Map.Map Con (Int, Int) } -- just for case, alt      

class ObjsOf a b where 
    objsOf :: a -> b

instance ObjsOf (Expr a) [Obj a] where
    objsOf (ELet _ defs e)  = (objsOf defs) ++ (objsOf e)
    objsOf (ECase _ e alts) = (objsOf e) ++ (objsOf alts)
    objsOf _ = [] -- EAtom, EFCall, EPrimop

instance ObjsOf (Alt a) [Obj a] where
    objsOf (ACon _ c vs e) = objsOf e
    objsOf (ADef _ v e)    = objsOf e

instance ObjsOf [Alt a] [Obj a] where
    objsOf = concatMap objsOf

instance ObjsOf (Obj a) [Obj a] where
    objsOf o@(FUN _ vs e n) = o : objsOf e
    objsOf o@(THUNK _ e n)  = o : objsOf e
    objsOf o              = [o] -- PAP, CON, BLACKHOLE

instance ObjsOf [Obj a] [Obj a] where
    objsOf = concatMap objsOf

itsOf :: [Obj InfoTab] -> [InfoTab]
itsOf = (map omd) . objsOf


-- ****************************************************************

-- here "a" is "Def [Var]",   "Expr [Var]", etc. 
--  and "b" is "Def InfoTab", "Expr InfoTab" etc.
-- and [(String, Int)] is the state, the map String constructors to Int tags

class SetITs a b where 
    setITs :: (Map String (Int,Int)) -> a -> b

instance SetITs [Obj [Var]] [Obj InfoTab] where
    setITs conmap = map (setITs conmap)

instance SetITs (Expr [Var]) (Expr InfoTab) where
    setITs conmap (ELet myfvs defs e) = 
        let
          defs' = setITs conmap defs
          e'    = setITs conmap e
        in ELet (JustFVs {fvs = myfvs}) defs' e'

    setITs conmap (ECase myfvs e alts) = 
        let
          e' = setITs conmap e
          alts' = map (setITs conmap) alts
        in ECase (ConMap{fvs = myfvs, conMap = conmap}) e' alts'

    -- EAtom, EFCall, EPrimop, this doesn't work
    -- setITs conmap e = e{emd = JustFVs {fvs = emd e}}

    setITs conmap (EAtom myfvs a) = 
        EAtom (JustFVs{fvs = myfvs}) a

    setITs conmap (EFCall myfvs f as) =
        EFCall (JustFVs{fvs = myfvs}) f as

    setITs conmap (EPrimop myfvs p as) =
        EPrimop (JustFVs{fvs = myfvs}) p as

instance SetITs (Alt [Var]) (Alt InfoTab) where
    setITs conmap (ACon myfvs c vs e) = 
        ACon (ConMap{fvs = myfvs, conMap = conmap}) c vs (setITs conmap e)
    setITs conmap (ADef myfvs v e) = 
        ADef (ConMap{fvs = myfvs, conMap = conmap}) v (setITs conmap e)

instance SetITs (Obj [Var]) (Obj InfoTab) where
    setITs conmap o@(FUN myfvs vs e n) = 
        FUN (makeIT conmap o) vs (setITs conmap e) n

    setITs conmap o@(PAP myfvs f as n) = 
        PAP (makeIT conmap o) f as n

    setITs conmap o@(CON myfvs c as n) = 
        CON (makeIT conmap o) c as n

    setITs conmap o@(THUNK myfvs e n) = 
        THUNK (makeIT conmap o) (setITs conmap e) n

    setITs conmap o@(BLACKHOLE myfvs n) = 
        BLACKHOLE (makeIT conmap o) n


-- ****************************************************************

-- could factor some of this but generates warnings
makeIT :: (Map String (Int,Int)) -> (Obj [Var]) -> InfoTab
makeIT contab o@(FUN fvs vs e n) = 
    Fun { arity = length vs,
          name = n,
          fvs = fvs,
          entryCode = showITType o ++ "_" ++ n
        }

makeIT contab o@(PAP fvs f as n) =
    Pap { args = as,
          name = n,
          fvs = fvs,
          entryCode = showITType o ++ "_" ++ n
        }

makeIT contab o@(CON fvs c as n) =
    case Map.lookup c contab of
      Nothing -> error "no contab entry"
      Just (a, t) -> 
          if length as /= a then
              error "arity mismatch"
          else
              Con { con = c,
                    tag = t,
                    arity = a,
                    args = as,
                    name = n,
                    fvs = fvs,
                    entryCode = showITType o ++ "_" ++ n
                  }

makeIT contab o@(THUNK fvs e n) =
    Thunk { name = n,
            fvs = fvs,
            entryCode = showITType o ++ "_" ++ n
          }

makeIT contab o@(BLACKHOLE fvs n) =
    Blackhole { name = n,
                fvs = fvs,
                entryCode = showITType o ++ "_" ++ n
              }


showObjType Fun {} = "FUN"
showObjType Pap {} = "PAP"
showObjType Con {} = "CON"
showObjType Thunk {} = "THUNK"
showObjType Blackhole {} = "BLACKHOLE"

showITType FUN {} = "fun"
showITType PAP {} = "pap"
showITType CON {} = "con"
showITType THUNK {} = "tnk"
showITType BLACKHOLE {} = "bhl"



{-
InfoTab it_z =
  { .name               = "z",
    .entryCode          = &z,
    .objType            = THUNK,
    .fvCount            = 1,
  };
-}

showIT it =
    "InfoTab it_" ++ name it ++ " = \n" ++
    "  { .name                = " ++ show (name it) ++ ",\n" ++
    "    .fvCount             = " ++ show (length $ fvs it) ++ ",\n" ++
    "    .entryCode           = &" ++ entryCode it ++ ",\n" ++
    showITspec it ++
    "  };\n"

showITspec it@(Fun {}) =
    "    .objType             = FUN,\n" ++
    "    .funFields.arity     = " ++ show (arity it) ++ ",\n"
        
showITspec it@(Pap {}) =
    "    .objType             = PAP,\n" ++
    "    .argCount            = " ++ show (length $ args it) ++ ",\n"
        
showITspec it@(Con {}) =
    "    .objType             = CON,\n" ++
    "    .conFields.arity     = " ++ show (arity it) ++ ",\n" ++
    "    .conFields.tag       = " ++ show (tag it) ++ ",\n" ++
    "    .conFields.conName   = " ++ show (con it) ++ ",\n"
        
showITspec it@(Thunk {}) =
    "    .objType             = THUNK,\n"
        
showITspec it@(Blackhole {}) = 
    "    .objType             = BLACKHOLE,\n"



      


