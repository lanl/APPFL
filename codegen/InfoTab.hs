{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# LANGUAGE CPP #-}
-- #include "../options.h"

module InfoTab(
  InfoTab(..),
  setCMaps,
  setITs,
  showITs,
  showObjType,
  itsOf,
) where

import Prelude
import STGbits
import PPrint
import AST
import ADT
import CMap
import Options
import Data.Maybe
import Data.List(nub,(\\),intercalate)
import Data.List.Split

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Util

import Language.C.Quote.GCC
import Language.C.Syntax (Definition, Initializer, Type)

-- need an infoTab entry for each lexically distinct HO or SHO

-- *****************************************************

{-
  every Expr, Alt, Obj has metadata, e.g. name, freevars
-}

data InfoTab =
    ITFun {
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      entryCode :: String,
      trueEntryCode :: String,

      arity :: Int}

  | ITPap {
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      entryCode :: String,
      trueEntryCode :: String,

      args     :: [(Atom,Monotype)],
      bargc :: Int,  -- boxed initial arg count
      uargc :: Int,  -- unboxed initial arg count
      argPerm :: [Int], -- map from notional pos to actual pos
      knownCall :: Maybe InfoTab} -- of the FUN

  | ITCon {
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count - not needed for Con
      ufvc :: Int,  -- unboxed FV count - not needed for Con
      truefvs :: [Var],
      entryCode :: String,

      args :: [(Atom,Monotype)],
      bargc :: Int,  -- boxed arg count
      uargc :: Int,  -- unboxed arg count
      argPerm :: [Int], -- map from notional pos to actual pos
      arity :: Int,
      con :: String, -- actual constructor name, not object name
      cmap :: CMap }
  | ITThunk {
      typ :: Monotype,
      ctyp :: Polytype,
      name :: String,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      entryCode :: String }

--BH   | ITBlackhole {
--BH       typ :: Monotype,
--BH       ctyp :: Polytype,
--BH       name :: String,
--BH       fvs :: [(Var,Monotype)],  -- why is this here?
--BH       bfvc :: Int,  -- boxed FV count
--BH       ufvc :: Int,  -- unboxed FV count
--BH       truefvs :: [Var],
--BH       entryCode :: String }

-- Expr
  | ITFCall {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      noHeapAlloc :: Bool,

      knownCall :: Maybe InfoTab } -- of the FUN

  | ITPrimop {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      noHeapAlloc :: Bool }

  | ITLet {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],

      noHeapAlloc :: Bool }

  | ITCase {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      noHeapAlloc :: Bool,
      cmap :: CMap}

  | ITAtom {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],
      noHeapAlloc :: Bool,

      maybeCMap :: Maybe CMap  -- only needed for LitC
    }

-- Alt
  | ITACon {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],

      cmap :: CMap }

  | ITADef {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,  -- boxed FV count
      ufvc :: Int,  -- unboxed FV count
      truefvs :: [Var],

      cmap :: CMap }

-- should continuations have InfoTabs at all???

-- Alts - this is used for CASECONTs
  | ITAlts {
      typ :: Monotype,
      ctyp :: Polytype,
      fvs :: [(Var,Monotype)],
      bfvc :: Int,          -- boxed FV count
      ufvc :: Int,          -- unboxed FV count
      truefvs :: [Var],
      scVar :: Var,         -- binding for scrutinee
      name :: String,       -- for C infotab
      entryCode :: String } -- for C infotab

  -- the following may be useful later
  -- for now case continuation is handled by Alts.ITAlts
  -- similarly function continuation could be handled by EFCall.ITFCall
  -- update continuation by THUNK.ITThunk?

--CInfoTab   | ITUpdcont
--CInfoTab   | ITCasecont
--CInfoTab   | ITCallcont
--CInfoTab   | ITFuncont
    deriving(Eq)

class ITsOf a b where
    itsOf :: a -> b

instance ITsOf a [b] => ITsOf [a] [b] where
   itsOf = concatMap itsOf

instance ITsOf (Obj a) [a] where
    itsOf FUN{omd, e} = omd : itsOf e
    itsOf THUNK{omd, e}  = omd : itsOf e
    itsOf o = [omd o] -- PAP, CON, BLACKHOLE

instance ITsOf (Expr a) [a] where
    itsOf ELet{emd, edefs, ee}  = emd : (itsOf edefs) ++ itsOf ee
    itsOf ECase{emd, ee, ealts} = emd : (itsOf ee) ++ itsOf ealts
    itsOf e = [emd e] -- EAtom, EFCall, EPrimOp

instance ITsOf (Alt a) [a] where
    itsOf ACon{amd, ae} = amd : itsOf ae
    itsOf ADef{amd, ae} = amd : itsOf ae

instance ITsOf (Alts a) [a] where
    itsOf Alts{altsmd, alts} = altsmd : itsOf alts

-- ****************************************************************

-- typUndef = error "typ set undefined in InfoTab.hs"
-- ctypUndef = error "ctyp set undefined in InfoTab.hs"
typUndef = MPhony
ctypUndef = PPoly [] MPhony


class SetITs a b where
    setITs :: a -> b

instance SetITs [Obj ([Var],[Var])] [Obj InfoTab] where
    setITs = map setITs

instance SetITs (Expr ([Var],[Var])) (Expr InfoTab) where
    setITs e@(ELet emd defs ee) =
        ELet (makeIT e) (setITs defs) (setITs ee)

    setITs e@(ECase emd ee alts) =
        ECase (makeIT e) (setITs ee) (setITs alts)

    setITs e@(EAtom emd a) =
        EAtom (makeIT e) a

    setITs e@(EFCall emd f eas) =
        EFCall (makeIT e) f (map setITs eas)

    setITs e@(EPrimOp emd p info eas) =
        EPrimOp (makeIT e) p info (map setITs eas)


instance SetITs (Alts ([Var],[Var])) (Alts InfoTab) where
    setITs as@(Alts altsmd alts name scrt) =
       Alts (makeIT as) (map setITs alts) name (setITs scrt)

instance SetITs (Alt ([Var],[Var])) (Alt InfoTab) where
    setITs a@(ACon amd c vs e) =
        ACon (makeIT a) c vs (setITs e)
    setITs a@(ADef amd v e) =
        ADef (makeIT a) v (setITs e)

instance SetITs (Obj ([Var],[Var])) (Obj InfoTab) where
    setITs o@(FUN omd vs e n) =
        FUN (makeIT o) vs (setITs e) n

    setITs o@(PAP omd f as n) =
        PAP (makeIT o) f (map setITs as) n

    setITs o@(CON omd c as n) =
        CON (makeIT o) c (map setITs as) n

    setITs o@(THUNK omd e n) =
        THUNK (makeIT o) (setITs e) n

--BH    setITs o@(BLACKHOLE omd n) =
--BH        BLACKHOLE (makeIT o) n


-- ****************************************************************

class MakeIT a where
    makeIT :: a -> InfoTab

instance MakeIT (Obj ([Var],[Var])) where
    makeIT o@(FUN (fvs,truefvs) vs e n) =
        ITFun { arity = length vs,
              name = n,
              fvs = zip fvs $ repeat typUndef,
              bfvc = -1,
              ufvc = -1,
              truefvs = truefvs,
              typ = typUndef,
              ctyp = ctypUndef,
              entryCode = "stg_funcall",
              trueEntryCode = "fun_" ++ n
            }

    makeIT o@(PAP (fvs,truefvs) f as n) =
        ITPap { args = zip (projectAtoms as) $ repeat typUndef,
              bargc = -1,
              uargc = -1,
              name = n,
              fvs = zip fvs $ repeat typUndef,
              bfvc = -1,
              ufvc = -1,
              argPerm = [],
              truefvs = truefvs,
              typ = typUndef,
              ctyp = ctypUndef,
              entryCode = "fun_" ++ f,
              trueEntryCode = "fun_" ++ f,
              knownCall = Nothing
            }

    makeIT o@(CON (fvs,truefvs) c as n) =
        ITCon { con = c,
              arity = length as,
              args = zip (projectAtoms as) $ repeat typUndef,
              bargc = -1,
              uargc = -1,
              argPerm = [],
              name = n,
              fvs = zip fvs $ repeat typUndef,
              bfvc = -1,
              ufvc = -1,
              truefvs = truefvs,
              typ = typUndef,
              ctyp = ctypUndef,
              entryCode = "stg_concall",
              cmap = Map.empty
            }

    makeIT o@(THUNK (fvs,truefvs) e n) =
        ITThunk { name = n,
                fvs = zip fvs $ repeat typUndef,
                bfvc = -1,
                ufvc = -1,
                truefvs = truefvs,
                typ = typUndef,
                ctyp = ctypUndef,
                entryCode = "thunk_" ++ n
              }

--BH    makeIT o@(BLACKHOLE (fvs,truefvs) n) =
--BH        ITBlackhole { name = n,
--BH                    typ = typUndef,
--BH                    ctyp = ctypUndef,
--BH                    fvs = zip fvs $ repeat typUndef,
--BH                    bfvc = -1,
--BH                    ufvc = -1,
--BH                    truefvs = truefvs,
--BH                    entryCode = "stgBlackhole"
--BH                  }

instance MakeIT (Expr ([Var],[Var])) where
    makeIT ELet{emd = (fvs,truefvs)} =
        ITLet {fvs = zip fvs $ repeat typUndef,
               bfvc = -1,
               ufvc = -1,
               truefvs = truefvs,
               typ = typUndef,
               ctyp = ctypUndef,
               noHeapAlloc = False}

    makeIT ECase{emd = (fvs,truefvs)} =
        ITCase{fvs = zip fvs $ repeat typUndef,
               bfvc = -1,
               ufvc = -1,
               truefvs = truefvs,
               typ = typUndef,
               ctyp = ctypUndef,
               noHeapAlloc = False,
               cmap = Map.empty}


    makeIT EAtom{emd = (fvs,truefvs)} =
        ITAtom{fvs = zip fvs $ repeat typUndef,
               bfvc = -1,
               ufvc = -1,
               truefvs = truefvs,
               typ = typUndef,
               ctyp = ctypUndef,
               noHeapAlloc = False,
               maybeCMap = Nothing}

    makeIT EFCall{emd = (fvs,truefvs)} =
        ITFCall{fvs = zip fvs $ repeat typUndef,
                bfvc = -1,
                ufvc = -1,
                truefvs = truefvs,
                typ = typUndef,
                ctyp = ctypUndef,
                noHeapAlloc = False,
                knownCall = Nothing}

    makeIT EPrimOp{emd = (fvs,truefvs)} =
        ITPrimop{fvs = zip fvs $ repeat typUndef,
                 bfvc = -1,
                 ufvc = -1,
                 truefvs = truefvs,
                 typ = typUndef,
                 ctyp = ctypUndef,
                 noHeapAlloc = False}


instance MakeIT (Alts ([Var],[Var])) where
    makeIT Alts{altsmd = (fvs,truefvs), aname, scrt} =
        ITAlts {fvs = zip fvs $ repeat typUndef,
                bfvc = -1,
                ufvc = -1,
                truefvs = truefvs,
                typ = typUndef,
                scVar = scvar,
                ctyp = ctypUndef,
                entryCode = aname,
                name = aname}
      where
        scvar = scrtVarName scrt

instance MakeIT (Alt ([Var],[Var])) where
    makeIT ACon{amd = (fvs,truefvs)} =
        ITACon{fvs = zip fvs $ repeat typUndef,
               bfvc = -1,
               ufvc = -1,
               truefvs = truefvs,
               typ = typUndef,
               ctyp = ctypUndef,
               cmap = Map.empty}

    makeIT ADef{amd = (fvs,truefvs)} =
        ITADef{fvs = zip fvs $ repeat typUndef,
               bfvc = -1,
               ufvc = -1,
               truefvs = truefvs,
               typ = typUndef,
               ctyp = ctypUndef,
               cmap = Map.empty}


showObjType :: InfoTab -> String
showObjType ITFun {} = "FUN"
showObjType ITPap {} = "PAP"
showObjType ITCon {} = "CON"
showObjType ITThunk {} = "THUNK"
--BH showObjType ITBlackhole {} = "BLACKHOLE"
showObjType _ = error "bad ObjType"

alignedDecl:: Type -> String -> Initializer -> Definition
alignedDecl typ name ini =
  [cedecl|$ty:typ $id:name __attribute__((aligned(OBJ_ALIGN)))
    = $init:ini;
  |]

-- want to separate CInfoTab and InfoTab--Data.Either seems
-- to be overkill since we wouldn't be using it to enforce type safety
showIT :: InfoTab -> Maybe (Bool, String, Definition)
showIT it@(ITAlts {}) =
  let init = showITinit it
      citname = "it_" ++ name it
      f x = Just $ (False, citname, )
                     (alignedDecl [cty| typename CInfoTab|] citname x)
  in maybe Nothing f init

showIT it =
  let init = showITinit it
      itname = "it_" ++ name it
      f x = Just $ (True, itname, )
                      (alignedDecl [cty| typename InfoTab|] itname x)
  in maybe Nothing f init


showITinit :: InfoTab -> Maybe Initializer
showITinit it@(ITFun {}) =
  Just [cinit|
         {
#if DEBUG_INFOTAB
           .pi = PI(),
#endif
            .name = $string:(name it),
            .entryCode = &$id:(entryCode it),
            .objType = FUN,
            .layoutInfo.payloadSize  = $int:(length $ fvs it),
            .layoutInfo.boxedCount = $int:(bfvc it),
            .layoutInfo.unboxedCount = $int:(ufvc it),
            .funFields.arity = $int:(arity it),
            .funFields.trueEntryCode = $id:(trueEntryCode it)
          }
       |]

showITinit it@(ITPap {}) =
   Just [cinit|
               {
#if DEBUG_INFOTAB
                 .pi = PI(),
#endif
                 .name = $string:(name it),
                 .entryCode = &$id:(entryCode it),
                 .objType = PAP,
                 .layoutInfo.payloadSize = $int:(length (fvs it) +
                                             length (args it) +
                                             1),
                 .layoutInfo.boxedCount = $int:(bfvc it),
                 .layoutInfo.unboxedCount = $int:(ufvc it),
                 .papFields.trueEntryCode = $id:(trueEntryCode it)
               }
             |]


showITinit it@(ITCon {}) =
  Just [cinit|
               {
#if DEBUG_INFOTAB
                 .pi = PI(),
#endif
                 .name = $string:(name it),
                 .entryCode = &$id:(entryCode it),
                 .objType = CON,
                 .layoutInfo.payloadSize = $int:(arity it),
                 .layoutInfo.boxedCount = $int:(bargc it),
                 .layoutInfo.unboxedCount = $int:(uargc it),
                 .layoutInfo.permString = $string:(concatMap show (argPerm it)),
                 .conFields.arity = $int:(arity it),
                 .conFields.tag = $id:(luConTag (con it) (cmap it)),
                 .conFields.conName  = $string:(con it)
               }
             |]

showITinit it@(ITThunk {}) =
  Just [cinit|
               {
#if DEBUG_INFOTAB
                 .pi = PI(),
#endif
                 .name = $string:(name it),
                 .entryCode = &$id:(entryCode it),
                 .objType = THUNK,
                 .layoutInfo.payloadSize = $int:(1 + (length $ fvs it)),
                 .layoutInfo.boxedCount = $int:(bfvc it),
                 .layoutInfo.unboxedCount = $int:(ufvc it)
               }
             |]

--BH showITinit it@(ITBlackhole {}) =
--BH   Just [cinit|
--BH                {
--BH if DEBUG_INFOTAB
--BH                  .pi = PI(),
--BH endif
--BH                  .name = $string:(name it),
--BH                  .entryCode = &$id:(entryCode it),
--BH                  .objType = BLACKHOLE,
--BH                  .layoutInfo.payloadSize = 0,
--BH                  .layoutInfo.boxedCount = 0,
--BH                  .layoutInfo.unboxedCount = 0
--BH                }
--BH              |]

showITinit it@(ITAlts {}) =
  Just [cinit|
               {
                 .name = $string:(name it),
                 .entryCode = &$id:(entryCode it),
                 .contType = CASECONT,
                 .cLayoutInfo.payloadSize = $int:((length $ fvs it) + 1),
                 .cLayoutInfo.bm.bits = $ulint:(npStrToBMInt ( 'N' :
                        replicate (bfvc it) 'P' ++
                        replicate (ufvc it) 'N') )
               }
             |]

showITinit it = Nothing

preDefInfoTabs =
    [ "it_stgCallCont",
      "it_stgStackCont",
      "it_stgLetCont",
      "it_stgUpdateCont",
      "it_stgShowResultCont" ]


showITs :: ITsOf a [InfoTab] => a -> [Definition]
--showITs os = catMaybes (map showIT $ itsOf os)

showITs os =
    let ps = catMaybes (map showIT $ itsOf os)
        -- quick hack
        (itnames, itdefs) =   unzip [ (name, defn) | (True, name, defn) <- ps ]
        (ucitnames, citdefs) = unzip [ (name, defn) | (False, name, defn) <- ps ]
        citnames = preDefInfoTabs ++ ucitnames
        itcount = length itnames
        citcount = length citnames
        -- const int stgInfoTabCount = #InfoTabs ;
        stgInfoTabCount =
            [cedecl| const int stgInfoTabCount = $exp:(itcount) ; |]
        initsIT = [[cinit| & $id:itname |] | itname <- itnames ]
        -- {&it, &it, ...}
        compoundInitIT = [cinit| { $inits:initsIT } |]
        -- InfoTab *const stgInfoTab[#InfoTabs] = {&it, &it, ...} ;
        stgInfoTab =
            [cedecl| typename InfoTab *const stgInfoTab [ $exp:(itcount) ] =
                       $init:compoundInitIT ; |]

        -- const int stgCInfoTabCount = #CInfoTabs ;
        stgCInfoTabCount =
            [cedecl| const int stgCInfoTabCount = $exp:(citcount) ; |]
        initsCIT = [[cinit| & $id:citname |] | citname <- citnames ]
        -- {&it, &it, ...}
        compoundInitCIT = [cinit| { $inits:initsCIT } |]
        -- stgCInfoTab *const stgCInfoTab[#CInfoTabs] = {&it, &it, ...} ;
        stgCInfoTab =
            [cedecl| typename CInfoTab *const stgCInfoTab [ $exp:(citcount) ] =
                       $init:compoundInitCIT ; |]
    in itdefs ++
       citdefs ++
       [ stgInfoTabCount,
         stgInfoTab,
         stgCInfoTabCount,
         stgCInfoTab]

{-
-- quick hack to get names as well
let its = itsOf os
                 maybeDefs = map showIT its
                 zippedMaybe = zip maybeDefs (map name its)
                 zipped = [(itDef, name) | (Just itDef, name) <- zippedMaybe]
                 (itDefs, names) = unzip zipped
                 inits = [[cinit| & $id:name |] | name <- names ]
                 compoundInit = [cinit| { $inits:inits } |]
                 cedecl = [cedecl| typename
-}


setCMaps :: [TyCon] -> [Obj InfoTab] -> ([TyCon], [Obj InfoTab])
setCMaps tycons objs =
  let cmap = toCMap tycons
  in (tycons, addCMapToITs cmap objs)

addCMapToITs :: CMap -> [Obj InfoTab] -> [Obj InfoTab]
addCMapToITs cmap objs =  map (setITs . (cmap,)) objs


instance SetITs (CMap, (Obj InfoTab)) (Obj InfoTab) where
  setITs (cmap,obj) = case obj of

    o@FUN{e} ->
      o{ e = setITs (cmap,e) }

    o@THUNK{e} ->
      o{ e = setITs (cmap,e) }

    o@CON{omd, c, as} ->
      o{omd = omd{ cmap = cmap },
        as = map (setITs . (cmap,)) as}

    o -> o --  BLACKHOLE and PAP don't need modification

instance SetITs (CMap, (Expr InfoTab)) (Expr InfoTab) where
  setITs (cmap,expr) = case expr of

    e@EAtom{emd, ea} ->
        case ea of
          LitC{} -> e{ emd = emd{ maybeCMap = Just cmap } }
          _      -> e{ emd = emd{ maybeCMap = Nothing   } }

    e@EFCall{eas} ->
        e{eas = map (setITs . (cmap,)) eas}

    -- this could be the identity:  primops should not apply to user-defined constants
    e@EPrimOp{eas} ->
        e{eas = map (setITs . (cmap,)) eas}


    e@ECase{ee, ealts, emd} ->
      e{ ee    = setITs (cmap,ee),
         ealts = setITs (cmap,ealts),
         emd   = emd { cmap = cmap } }

    e@ELet{ee, edefs} ->
      e{ ee    = setITs (cmap,ee),
         edefs = map (setITs . (cmap,)) edefs }

instance SetITs (CMap, (Alts InfoTab)) (Alts InfoTab) where
  setITs (cmap, a@Alts{alts}) =
    a{ alts = map (setITs . (cmap,)) alts}

instance SetITs (CMap, (Alt InfoTab)) (Alt InfoTab) where
  setITs (cmap, alt) = case alt of

    a@ACon{amd, ac, ae} ->
      a{ amd = amd{cmap = cmap},
         ae = setITs (cmap,ae) }

    a@ADef{ae} ->
      a { ae = setITs (cmap,ae) }


-- can change this to get infotabs printed in block comments
instance Unparse InfoTab where
  unparse it = empty

instance PPrint InfoTab where
 pprint it = text "Infotab:" <+> itName $+$
            nest 2 (
              text "typ:" <+> pprint (typ it) $+$
              itExtras )
   where
     makeName n = text "name:" <+> text n
     makeKCDoc kc = case kc of
       Just it' -> text "known call to" <+> text (name it')
       Nothing  -> text "unknown call"
     makeHADoc nha = text "noHeapAlloc:" <+> boolean nha
     freevsDoc vs = text "fvs:" <+> listText (map fst vs) -- should show Monotype, too
     trufreevsDoc vs = text "truefvs:" <+> listText vs
     frvarsDoc vs tvs = freevsDoc vs $+$ trufreevsDoc tvs
     (itName, itExtras) =
           case it of
             ITFun{..} ->
               (text "ITFun", makeName name $+$
                            frvarsDoc fvs truefvs)
             ITPap{..} ->
               (text "ITPap", makeName name $+$
                            makeKCDoc knownCall $+$
                            frvarsDoc fvs truefvs)
             ITCon{..} ->
               (text "ITCon", makeName name $+$
                            frvarsDoc fvs truefvs)
             ITThunk{..} ->
               (text "ITThunk", makeName name $+$
                              frvarsDoc fvs truefvs)
--BH              ITBlackhole{..} ->
--BH                (text "ITBlackhole", makeName name $+$
--BH                                   frvarsDoc fvs truefvs)
             ITAtom{..} ->
               (text "ITAtom", makeHADoc noHeapAlloc $+$
                               frvarsDoc fvs truefvs)
             ITFCall{..} ->
               (text "ITFCall", makeHADoc noHeapAlloc $+$
                                makeKCDoc knownCall $+$
                                frvarsDoc fvs truefvs)
             ITPrimop{..} ->
               (text "ITPrimop", makeHADoc noHeapAlloc $+$
                                 frvarsDoc fvs truefvs)
             ITLet{..} ->
               (text "ITLet", makeHADoc noHeapAlloc $+$
                              frvarsDoc fvs truefvs)
             ITCase{..} ->
               (text "ITCase", makeHADoc noHeapAlloc $+$
                               frvarsDoc fvs truefvs)
             ITACon{..} ->
               (text "ITACon", frvarsDoc fvs truefvs)
             ITADef{..} ->
               (text "ITADef", frvarsDoc fvs truefvs)
             ITAlts{..} ->
               (text "ITAlts", makeName name $+$
                               frvarsDoc fvs truefvs)
--             _ -> (text "Other InfoTab",empty)


instance Show InfoTab where
    show it = show (typ it)
