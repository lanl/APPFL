{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns    #-}

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
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [Var],
      truefvs :: [Var],
      entryCode :: String,
      arity :: Int}      
  | Pap { 
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [Var],
      truefvs :: [Var],
      entryCode :: String,
      args     :: [Atom],
      knownCall :: Maybe InfoTab} -- of the FUN
  | Con { 
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [Var],
      truefvs :: [Var],
      entryCode :: String,
      args :: [Atom],
      arity :: Int,
      con :: String, -- actual constructor name, not object name
      tag :: Int,
      tconMap :: TyConMap,
      dconMap :: DataConMap }
  | Thunk { 
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [Var],
      truefvs :: [Var],
      entryCode :: String }
  | Blackhole {
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [Var],
      truefvs :: [Var],
      entryCode :: String }
  | ITAtom { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var],
      truefvs :: [Var],
      noHeapAlloc :: Bool }
  | ITFCall { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var],
      truefvs :: [Var],
      noHeapAlloc :: Bool,
      knownCall :: Maybe InfoTab } -- of the FUN
  | ITPrimop { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var], 
      truefvs :: [Var],
      noHeapAlloc :: Bool }
  | ITLet { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var],
      truefvs :: [Var],
      noHeapAlloc :: Bool }
  | ITCase { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var],
      truefvs :: [Var],
      noHeapAlloc :: Bool }
  | ITAlt { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var],
      truefvs :: [Var],
      tconMap :: TyConMap, 
      dconMap :: DataConMap } 
  | ITAlts { 
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [Var],
      truefvs :: [Var] }
  | ITUpdcont, 
  | ITCasecont, 
  | ITCallcont, 
  | ITFuncont,
    deriving(Eq)   

class ObjsOf a b where 
    objsOf :: a -> b

instance ObjsOf [Obj a] [Obj a] where
    objsOf = concatMap objsOf

instance ObjsOf (Obj a) [Obj a] where
    objsOf o@(FUN _ vs e n) = o : objsOf e
    objsOf o@(THUNK _ e n)  = o : objsOf e
    objsOf o              = [o] -- PAP, CON, BLACKHOLE

instance ObjsOf (Expr a) [Obj a] where
    objsOf ELet{edefs, ee}  = (objsOf edefs) ++ (objsOf ee)
    objsOf ECase{ee, ealts} = (objsOf ee) ++ (objsOf ealts)
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

typUndef = error "typ set undefined in InfoTab.hs"
ctypUndef = error "ctyp set undefined in InfoTab.hs"
-- typUndef = PPoly [] (MBoxed (BTyCon "Z" []))

class SetITs a b where 
    setITs :: a -> b

instance SetITs [Obj ([Var],[Var])] [Obj InfoTab] where
    setITs = map setITs

instance SetITs (Expr ([Var],[Var])) (Expr InfoTab) where
    setITs (ELet (myfvs,mytruefvs) defs e) = 
        let
          defs' = setITs defs
          e'    = setITs e
        in ELet (ITLet {fvs = myfvs, 
                        truefvs = mytruefvs, 
                        typ = typUndef, 
                        ctyp = ctypUndef, 
                        noHeapAlloc = False}) defs' e'

    setITs (ECase (myfvs,mytruefvs) e alts) = 
        let
          e' = setITs e
          alts' = setITs alts
        in ECase (ITCase{fvs = myfvs, 
                         truefvs = mytruefvs, 
                         typ = typUndef, 
                         ctyp = ctypUndef, 
                         noHeapAlloc = False}) e' alts'

    -- EAtom, EFCall, EPrimop, this doesn't work
    -- setITs e = e{emd = JustFVs {fvs = emd e}}

    setITs (EAtom (myfvs,mytruefvs) a) = 
        EAtom (ITAtom{fvs = myfvs, 
                      truefvs = mytruefvs, 
                      typ = typUndef, 
                      ctyp = ctypUndef, 
                      noHeapAlloc = False}) a

    setITs (EFCall (myfvs,mytruefvs) f as) =
        EFCall (ITFCall{fvs = myfvs, 
                        truefvs = mytruefvs, 
                        typ = typUndef, 
                        ctyp = ctypUndef, 
                        noHeapAlloc = False, 
                        knownCall = Nothing}) f as

    setITs (EPrimop (myfvs,mytruefvs) p as) =
        EPrimop (ITPrimop{fvs = myfvs, 
                          truefvs = mytruefvs, 
                          typ = typUndef, 
                          ctyp = ctypUndef, 
                          noHeapAlloc = False}) p as

instance SetITs (Alts ([Var],[Var])) (Alts InfoTab) where
    setITs (Alts (myfvs,mytruefvs) alts name) = 
       let alts' = map setITs alts
       in Alts (ITAlts {fvs = myfvs, 
                        truefvs = mytruefvs, 
                        typ = typUndef,
                        ctyp = ctypUndef}) alts' name

instance SetITs (Alt ([Var],[Var])) (Alt InfoTab) where
    setITs (ACon (myfvs,mytruefvs) c vs e) = 
        ACon (ITAlt{fvs = myfvs, truefvs = mytruefvs, 
                    typ = typUndef,
                    ctyp = ctypUndef,
                    dconMap = Map.empty, 
                    tconMap = Map.empty}) c vs (setITs e)
    setITs (ADef (myfvs,mytruefvs) v e) = 
        ADef (ITAlt{fvs = myfvs, truefvs = mytruefvs, 
                    typ = typUndef,
                    ctyp = ctypUndef,
                    dconMap = error "ADef dconMap undefined",
                    tconMap = error "ADef tconMap undefined"}) v (setITs e)

instance SetITs (Obj ([Var],[Var])) (Obj InfoTab) where
    setITs o@(FUN (myfvs,mytruefvs) vs e n) = 
        FUN (makeIT o) vs (setITs e) n

    setITs o@(PAP (myfvs,mytruefvs) f as n) = 
        PAP (makeIT o) f as n

    setITs o@(CON (myfvs,mytruefvs) c as n) = 
        CON (makeIT o) c as n

    setITs o@(THUNK (myfvs,mytruefvs) e n) = 
        THUNK (makeIT o) (setITs e) n

    setITs o@(BLACKHOLE (myfvs,mytruefvs) n) = 
        BLACKHOLE (makeIT o) n


-- ****************************************************************

-- could factor some of this but generates warnings
makeIT :: Obj ([Var],[Var]) -> InfoTab
makeIT o@(FUN (fvs,truefvs) vs e n) = 
    Fun { arity = length vs,
          name = n,
          fvs = fvs,
          truefvs = truefvs,
          typ = typUndef,
          ctyp = ctypUndef,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "fun_" ++ n
        }

makeIT o@(PAP (fvs,truefvs) f as n) =
    Pap { args = as,
          name = n,
          fvs = fvs,
          truefvs = truefvs,
          typ = typUndef,
          ctyp = ctypUndef,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "fun_" ++ n,
          knownCall = Nothing
        }

makeIT o@(CON (fvs,truefvs) c as n) =
    Con { con = c,
          tag = -1, -- this gets set later
          arity = length as,
          args = as,
          name = n,
          fvs = fvs,
          truefvs = truefvs,
          typ = typUndef,
          ctyp = ctypUndef,
--          entryCode = showITType o ++ "_" ++ n
          entryCode = "stg_constructorcall",
          dconMap = error "ADef dconMap undefined",
          tconMap = error "ADef tconMap undefined"
        }

makeIT o@(THUNK (fvs,truefvs) e n) =
    Thunk { name = n,
            fvs = fvs,
            truefvs = truefvs,
            typ = typUndef,
            ctyp = ctypUndef,
--            entryCode = showITType o ++ "_" ++ n
            entryCode = "fun_" ++ n
          }

makeIT o@(BLACKHOLE (fvs,truefvs) n) =
    Blackhole { name = n,
                typ = typUndef,
                ctyp = ctypUndef,
                fvs = fvs,
                truefvs = truefvs,
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

      


