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
import AST
import ADT
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
      typ :: Polytype,
      name :: String,
      fvs :: [Var],
      entryCode :: String,
      arity :: Int}      
  | Pap { 
      typ :: Polytype,
      name :: String,
      fvs :: [Var],
      entryCode :: String,
      args     :: [Atom] }
  | Con { 
      typ :: Polytype,
      name :: String,
      fvs :: [Var],
      entryCode :: String,
      args :: [Atom],
      arity :: Int,
      con :: String, -- actual constructor name, not object name
      tag :: Int }
  | Thunk { 
      typ :: Polytype,
      name :: String,
      fvs :: [Var],
      entryCode :: String }
  | Blackhole {
      typ :: Polytype,
      name :: String,
      fvs :: [Var],
      entryCode :: String }
  -- this is an umbrella for expressions for now -- need to factor this better
  | ITAtom { 
      typ :: Polytype,
      fvs :: [Var] }
  | ITFCall { 
      typ :: Polytype,
      fvs :: [Var] }
  | ITPrimop { 
      typ :: Polytype,
      fvs :: [Var] }
  | ITLet { 
      typ :: Polytype,
      fvs :: [Var] }
  | ITCase { 
      typ :: Polytype,
      fvs :: [Var] }
  | ITAlt { 
      typ :: Polytype,
      fvs :: [Var],
      tconMap :: TyConMap, -- work in progress not used 
      dconMap :: DataConMap } 
  | ITAlts { 
      typ :: Polytype,
      fvs :: [Var] }
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

typUndef = error "typ undefined in infotab"

class SetITs a b where 
    setITs :: a -> b

instance SetITs [Def [Var]] [Def InfoTab] where
   setITs = onObjs setITs

instance SetITs [Obj [Var]] [Obj InfoTab] where
    setITs = map setITs

instance SetITs (Expr [Var]) (Expr InfoTab) where
    setITs (ELet myfvs defs e) = 
        let
          defs' = setITs defs
          e'    = setITs e
        in ELet (ITLet {fvs = myfvs, typ = typUndef}) defs' e'

    setITs (ECase myfvs e alts) = 
        let
          e' = setITs e
          alts' = setITs alts
        in ECase (ITCase{fvs = myfvs, typ = typUndef}) e' alts'

    -- EAtom, EFCall, EPrimop, this doesn't work
    -- setITs e = e{emd = JustFVs {fvs = emd e}}

    setITs (EAtom myfvs a) = 
        EAtom (ITAtom{fvs = myfvs, typ = typUndef}) a

    setITs (EFCall myfvs f as) =
        EFCall (ITFCall{fvs = myfvs, typ = typUndef}) f as

    setITs (EPrimop myfvs p as) =
        EPrimop (ITPrimop{fvs = myfvs, typ = typUndef}) p as

instance SetITs (Alts [Var]) (Alts InfoTab) where
    setITs (Alts myfvs alts name) = 
       let alts' = map setITs alts
       in Alts (ITAlts {fvs = myfvs, typ = typUndef}) alts' name

instance SetITs (Alt [Var]) (Alt InfoTab) where
    setITs (ACon myfvs c vs e) = 
        ACon (ITAlt{fvs = myfvs, 
                    typ = typUndef,
                    dconMap = Map.empty, 
                    tconMap = Map.empty}) c vs (setITs e)
    setITs (ADef myfvs v e) = 
        ADef (ITAlt{fvs = myfvs, 
                    typ = typUndef,
                    dconMap = error "ADef dconMap undefined",
                    tconMap = error "ADef tconMap undefined"}) v (setITs e)

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
          typ = typUndef,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "fun_" ++ n
        }

makeIT o@(PAP fvs f as n) =
    Pap { args = as,
          name = n,
          fvs = fvs,
          typ = typUndef,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "fun_" ++ n
        }

makeIT o@(CON fvs c as n) =
    Con { con = c,
          tag = -1, -- this gets set later
          arity = length as,
          args = as,
          name = n,
          fvs = fvs,
          typ = typUndef,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "stg_constructorcall"
        }

makeIT o@(THUNK fvs e n) =
    Thunk { name = n,
            fvs = fvs,
            typ = typUndef,
--            entryCode = showITType o ++ "_" ++ n
            entryCode = "fun_" ++ n
          }

makeIT o@(BLACKHOLE fvs n) =
    Blackhole { name = n,
                typ = typUndef,
                fvs = fvs,
--                entryCode = showITType o ++ "_" ++ n
                entryCode = "stg_error"
              }


showObjType Fun {} = "FUN"
showObjType Pap {} = "PAP"
showObjType Con {} = "CON"
showObjType Thunk {} = "THUNK"
showObjType Blackhole {} = "BLACKHOLE"
showObjType _ = error "bad ObjType"

showITType _ = "sho"
-- showITType FUN {} = "ofun"
-- showITType PAP {} = "opap"
-- showITType CON {} = "ocon"
-- showITType THUNK {} = "otnk"
-- showITType BLACKHOLE {} = "obhl"
-- showITTType _ = error "bad ITType"

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

      


