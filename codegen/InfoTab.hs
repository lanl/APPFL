{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InfoTab(
  InfoTab(..),
  setITs,
  showITs,
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
      conMap :: Map.Map Con (Int, Int) } -- just for ACon
    deriving(Eq,Show)   

class ObjsOf a b where 
    objsOf :: a -> b

instance ObjsOf [Obj a] [Obj a] where
    objsOf = concatMap objsOf

instance ObjsOf (Obj a) [Obj a] where
    objsOf o@(FUN _ vs e n) = o : objsOf e
    objsOf o@(THUNK _ e n)  = o : objsOf e
    objsOf o              = [o] -- PAP, CON, BLACKHOLE

instance ObjsOf (Expr a) [Obj a] where
    objsOf (ELet _ defs e)  = (objsOf defs) ++ (objsOf e)
    objsOf (ECase _ e alts) = (objsOf e) ++ (objsOf alts)
    objsOf _ = [] -- EAtom, EFCall, EPrimop

instance ObjsOf (Alt a) [Obj a] where
    objsOf (ACon _ c vs e) = objsOf e
    objsOf (ADef _ v e)    = objsOf e
    
instance ObjsOf (Alts a) [Obj a] where
    objsOf (Alts _ alts _) = objsOf alts

instance ObjsOf [Alt a] [Obj a] where
    objsOf = concatMap objsOf

itsOf :: [Obj InfoTab] -> [InfoTab]
itsOf = (map omd) . objsOf


-- ****************************************************************

-- here "a" is "Def [Var]",   "Expr [Var]", etc. 
--  and "b" is "Def InfoTab", "Expr InfoTab" etc.
-- and [(String, Int)] is the state, the map String constructors to Int tags

class SetITs a b where 
    setITs :: a -> b

instance SetITs [Obj [Var]] [Obj InfoTab] where
    setITs = map setITs

instance SetITs (Expr [Var]) (Expr InfoTab) where
    setITs (ELet myfvs defs e) = 
        let
          defs' = setITs defs
          e'    = setITs e
        in ELet (JustFVs {fvs = myfvs}) defs' e'

    setITs (ECase myfvs e alts) = 
        let
          e' = setITs e
          alts' = setITs alts
        in ECase (JustFVs{fvs = myfvs}) e' alts'

    -- EAtom, EFCall, EPrimop, this doesn't work
    -- setITs e = e{emd = JustFVs {fvs = emd e}}

    setITs (EAtom myfvs a) = 
        EAtom (JustFVs{fvs = myfvs}) a

    setITs (EFCall myfvs f as) =
        EFCall (JustFVs{fvs = myfvs}) f as

    setITs (EPrimop myfvs p as) =
        EPrimop (JustFVs{fvs = myfvs}) p as

instance SetITs (Alts [Var]) (Alts InfoTab) where
    setITs (Alts myfvs alts name) = 
       let alts' = map setITs alts
       in Alts (JustFVs {fvs = myfvs}) alts' name

instance SetITs (Alt [Var]) (Alt InfoTab) where
    setITs (ACon myfvs c vs e) = 
        ACon (ConMap{fvs = myfvs, conMap = Map.empty}) c vs (setITs e)
    setITs (ADef myfvs v e) = 
        ADef (JustFVs{fvs = myfvs}) v (setITs e)

instance SetITs (Obj [Var]) (Obj InfoTab) where
    setITs o@(FUN myfvs vs e n) = 
        FUN (makeIT o) vs (setITs e) n

    setITs o@(PAP myfvs f as n) = 
        PAP (makeIT o) f as n

    setITs o@(CON myfvs c as n) = 
        CON (makeIT o) c as n

    setITs o@(THUNK myfvs e n) = 
        THUNK (makeIT o) (setITs e) n

    setITs o@(BLACKHOLE myfvs n) = 
        BLACKHOLE (makeIT o) n


-- ****************************************************************

-- could factor some of this but generates warnings
makeIT :: Obj [Var] -> InfoTab
makeIT o@(FUN fvs vs e n) = 
    Fun { arity = length vs,
          name = n,
          fvs = fvs,
          entryCode = showITType o ++ "_" ++ n
        }

makeIT o@(PAP fvs f as n) =
    Pap { args = as,
          name = n,
          fvs = fvs,
          entryCode = showITType o ++ "_" ++ n
        }

makeIT o@(CON fvs c as n) =
    Con { con = c,
          tag = -1, -- this gets set later
          arity = length as,
          args = as,
          name = n,
          fvs = fvs,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "stg_constructorcall"
        }

makeIT o@(THUNK fvs e n) =
    Thunk { name = n,
            fvs = fvs,
            entryCode = showITType o ++ "_" ++ n
          }

makeIT o@(BLACKHOLE fvs n) =
    Blackhole { name = n,
                fvs = fvs,
                entryCode = showITType o ++ "_" ++ n
              }


showObjType Fun {} = "FUN"
showObjType Pap {} = "PAP"
showObjType Con {} = "CON"
showObjType Thunk {} = "THUNK"
showObjType Blackhole {} = "BLACKHOLE"
showObjType _ = error "bad ObjType"

showITType FUN {} = "fun"
showITType PAP {} = "pap"
showITType CON {} = "con"
showITType THUNK {} = "tnk"
showITType BLACKHOLE {} = "bhl"
showITTType _ = error "bad ITType"

showITs os = concatMap showIT $ itsOf os

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

showITspec _ = error "bad ITspec"

      


