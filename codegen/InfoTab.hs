{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module InfoTab(
  InfoTab(..),
  makeITs,
  showIT,
) where

import Prelude
import Parser
import Data.List(nub,(\\))

-- need an infoTab entry for each distinct HO

-- *****************************************************

{-

  We need the free variables of subtrees for two reasons:
  * for constructing an object (closure), to know what to put it its payload
  * and to know what needs to be stashed in a continuation

  Conversely, when generating code for an expression, we need to know where
  to find each variable.  This is done by maintaining a stack of different
  kinds of `exposures' of variable values:

  * SHOs - just one set of these at the bottom of the stack
  * HOs, following a "let"
  * Formal function parameters, these are named parameters (alternatively,
    could not name and use a CallCont like stgApply--TODO:  think about this)
  * Popped CaseCont vars
  * Alt constructor vars or default var (TODO:  these could be formal function
    params if individual alts are separate functions)

In more detail:

SHO:  referenced with absolute memory address, e.g. "&sho_unit"

HO:  address calculated from TOH pointer, e.g. "&((Obj *)TOH_ptr)[-3]"

Formal parameter:  by name, e.g. "x"

CaseCont:  "myCaseCont.payload[i]"

Alt constructor var:  "stgCurVal.op->payload[i]"

Alt default var:  "stgCurVal"

-}

{-
  gotta go back and fix Parser.hs, Rename.hs
  every Expr, Alt, Obj has metadata, e.g. name, freevars
  or wrap in tuple?
  *** careful, \\ only removes first instance so do all the nubs inside
-}

-- thread around a list of constructors already encountered, with their arity
-- for sanity checking
-- IMPORTANT REALIZATION:  need an infoTab entry for each CON(C N P P N) combination
-- for layout information, for garbage collection, when we no longer have a
-- discriminator for PtrOrLiteral -- mostly worry about this later

-- for InfoTabs we need to know the number of fvs but not where they are so we
-- can flatten the tree first
class DefsOf a b where 
    defsOf :: a -> b

instance DefsOf (Expr a) [Def a] where
    defsOf (ELet _ defs e) = (defsOf defs) ++ (defsOf e)
    defsOf (ECase _ e alts) = (defsOf e) ++ (defsOf alts)
    defsOf _ = [] -- EAtom, EFCall, EPrimop

instance DefsOf (Alt a) [Def a] where
    defsOf (ACon _ c vs e) = (defsOf e)
    defsOf (ADef _ v e) = (defsOf e)

instance DefsOf [Alt a] [Def a] where
    defsOf = concatMap defsOf

instance DefsOf (Obj a) [Def a] where
    defsOf (FUN _ vs e) = defsOf e
    defsOf (THUNK _ e) = defsOf e
    defsOf _ = [] -- PAP, CON, BLACKHOLE

instance DefsOf (Def a) [Def a] where
    defsOf def@(v, o) = def : defsOf o

instance DefsOf [Def a] [Def a] where
    defsOf = concatMap defsOf


type Ctor = (String, Int, String) -- ("Cons", 2, "NP")

data InfoTab = 
    Fun    { name :: String,
             fvs :: [Var],
             entryCode :: String,
             arity :: Int }
  | Pap    { name :: String,
             fvs :: [Var],
             entryCode :: String,
             argCount :: Int }
  | Con    { name :: String,
             fvs :: [Var],
             entryCode :: String,
             arity :: Int,
             con :: String, -- actual constructor name, not object name
             tag :: Int
           }
  | Thunk { name :: String,
            fvs :: [Var],
            entryCode :: String}
  | Blackhole { name :: String,
                fvs :: [Var],
                entryCode :: String}

makeConTags contags [] = []
makeConTags contags (c@(Con {}):cs) =
    case lookup (con c) contags of
      Nothing -> c {tag = length contags} : 
                 makeConTags ((con c, length contags):contags) cs
      Just j  -> c {tag = j} : makeConTags contags cs
makeConTags contags (c:cs) = c : makeConTags contags cs

makeITs :: [Def [Var]] -> [InfoTab] -- monomorphism restriction
makeITs = (makeConTags []) . (map makeIT) . defsOf

makeITcom it (v, o) =
    it { name = v,
         InfoTab.fvs = Parser.fvs o,
         entryCode = "&" ++ showITType it ++ "_" ++ v
       }

makeIT :: (Var, Obj [Var]) -> InfoTab
makeIT d@(v, (FUN fvs vs e)) = 
    let it = Fun { arity = length vs }
    in makeITcom it d

makeIT d@(v, (PAP fvs f as)) =
    let it = Pap { argCount = length as }
    in makeITcom it d

makeIT d@(v, (CON fvs c as)) =
    let it = Con { con = c,
                   -- tag set later, TODO:  fix this
                   arity = length as 
                 }
    in makeITcom it d

makeIT d@(v, o@(THUNK fvs e)) =
    makeITcom Thunk {} d

makeIT d@(v, o@(BLACKHOLE _)) =
    makeITcom Blackhole {} d


showITType Fun {} = "fun"
showITType Pap {} = "pap"
showITType Con {} = "con"
showITType Thunk {} = "tnk"
showITType Blackhole {} = "bhl"

showObjType Fun {} = "FUN"
showObjType Pap {} = "PAP"
showObjType Con {} = "CON"
showObjType Thunk {} = "THUNK"
showObjType Blackhole {} = "BLACKHOLE"

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
    "    .fvCount             = " ++ show (length $ InfoTab.fvs it) ++ ",\n" ++
    "    .entryCode           = " ++ entryCode it ++ ",\n" ++
    showITspec it ++
    "  };\n"

showITspec it@(Fun {}) =
    "    .objType             = FUN,\n" ++
    "    .funFields.arity     = " ++ show (arity it) ++ ",\n"
        
showITspec it@(Pap {}) =
    "    .objType             = PAP,\n" ++
    "    .argCount            = " ++ show (argCount it) ++ ",\n"
        
showITspec it@(Con {}) =
    "    .objType             = CON,\n" ++
    "    .conFields.arity     = " ++ show (arity it) ++ ",\n" ++
    "    .conFields.tag       = " ++ show (tag it) ++ ",\n" ++
    "    .conFields.conName   = " ++ show (con it) ++ ",\n"
        
showITspec it@(Thunk {}) =
    "    .objType             = THUNK,\n"
        
showITspec it@(Blackhole {}) = 
    "    .objType             = BLACKHOLE,\n"


{-
itDef :: (String, (Maybe String, Obj)) -> State [Ctor] [InfoTab])
itDef (_, no) = itNObj no

itNObj (n, FUN a vs e) =
    Fun {name = n,
         fvCount = length vs,
         entryCode = "&fun_"
-}    
      


