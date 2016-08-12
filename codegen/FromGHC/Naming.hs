
{-# LANGUAGE CPP, NamedFieldPuns, MagicHash, FlexibleInstances,
BangPatterns, ViewPatterns, PatternGuards, UndecidableInstances #-}

#define _HERE ( __FILE__ ++ ":" ++ show (__LINE__ :: Int) )

module FromGHC.Naming where

import FromGHC.BuiltIn
import AST
import ADT
import Util

import qualified Data.Map as Map
import Data.Maybe
import Data.Char 
import Data.List (unfoldr)
import Data.Tuple (swap)
import Numeric (showHex, readHex)
import GHC.Exts (Int(..), indexCharOffAddr# )

import Unique (Unique, getKey)
import Module (moduleNameString, moduleName)
import Name (NamedThing (..), nameModule_maybe, getOccString, nameUnique)
import FastTypes (shiftRLFastInt, bitAndFastInt, iBox, cBox)
import Var (Id)

-- The UniqueNamer is the solution to GHC's non-deterministic naming. The
-- Uniques GHC generates are not the same between otherwise identical runs, but
-- the shape of the AST appears to be, and we can use that with a stateful
-- traversal to introduce deterministic naming.  This is useful for debugging
-- when matching names from a pretty-printed GHC STG program with their
-- counterparts in a pretty-printed APPFL STG program.

-- Uniques are wrappers for unboxed ints, used to disambiguate names in GHC.
-- There is no guarantee regarding shadowing of Uniques (see Note [Shadowing] in
-- CoreSyn.hs) but they preserve program semantics whereas the OccNames alone do
-- not appear to. The Integral a Unique maps to corresponds to when it was
-- encountered in the AST. The second element is the running counter of how many
-- Uniques have been seen.
type UniqueNamer  = (Map.Map Unique Int, Int)

emptyNamer :: UniqueNamer
emptyNamer = (Map.empty, 0)

class Monad m => UniqueNameState m where
  putNamer :: UniqueNamer -> m ()
  getNamer :: m UniqueNamer



getIntFromUnique :: UniqueNameState s => Unique -> s Int
getIntFromUnique u =
  do
    (nameMap, count) <- getNamer
    let (newMap, count', v) = case Map.lookup u nameMap of
                        Just v  -> (nameMap, count, v)
                        Nothing ->
                          (Map.insert u count nameMap, succ count, count)
    putNamer (newMap, count')
    return v




-- | Convenience class for things that we can provide our own name for
class NamedThing a => AppflNameable a where
  makeAppflName :: a -> Maybe AppflName

  -- | We have a good idea for how to map anything with a Name into our domain but
  -- any more specific instance may have a preferred instance (hence the overlap)

instance {-# OVERLAPPABLE #-} NamedThing t => AppflNameable t where
  makeAppflName = namedThingToAppflName

  
-- | Ids have extra information (IdDetails) that we sometimes need to properly
-- rename.
instance {-# OVERLAPPING #-} AppflNameable Id where
  makeAppflName = idToAppflName
  

-- | Produce a String name for a NamedThing.  Everything GHC names should be
-- converted via this function.  This is hard to enforce at the Type level,
-- given that all of our data structures only require Strings as identifiers.
nameGhcThing :: (AppflNameable t, UniqueNameState s) => t -> s String
nameGhcThing thing =
  do
    altName <- qualifyDeterministically thing
    let realName = makeAppflName thing
    return $ fromMaybe altName realName

    

qualifyDeterministically  :: (NamedThing t, UniqueNameState s) => t -> s String 
qualifyDeterministically (getName -> name) -- Using ViewPatterns for fun.
  -- Syntax is (expr '->' pattern) where the expr will be applied to whatever
  -- argument is passed.  In this case, that's NamedThing t => t -> Name
  =  let oname = getOccString name
     in case nameModule_maybe name of
          Just mod -> pure $
                      if isAppflBuiltIn oname
                      then oname
                      else moduleNameString (moduleName mod) ++ "." ++ oname
               
          Nothing  -> do i <- getIntFromUnique (nameUnique name)
                         return (oname ++ showIntTersely i)


-- | Produce a qualified name for a NamedThing as a
--   String. (e.g. Module.Submodule.idname) Does not add package information,
--   but does add characters to express the underlying Unique for Names that
--   don't have a Module. (Internal/System names)
qualifyName :: NamedThing a => a -> String
qualifyName thing = qualify idname
  where name           = getName thing
        qualify n      = case nameModule_maybe name of
                              Just m  -> makePrefix m ++ n
                              Nothing -> n ++ '!':showIntTersely (getKey $ nameUnique name)
        makePrefix mod = moduleNameString (moduleName mod) ++ "."
        idname         = getOccString name


-- This is roughly based on the (uglier, IMO) base62 encoding from
-- basicTypes/Unique.hs (not exported). Because this is a base64
-- encoding (with '-' and '_'), it needs to be sanitized for C
-- codegen.  There's other sanitization to be done on non-uniquified
-- names, so it's deferred for now.
showIntTersely :: Int -> String
showIntTersely i = unfoldr op i
  where chr64 n    = cBox ( indexCharOffAddr# chars64 n)
        !chars64   = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"#
        op (I# 0#) = Nothing
        op (I# i#) = case i# `shiftRLFastInt` 6# {- 2^6 = 64 -} of
                       shifted# ->
                         case i# `bitAndFastInt` 63# of
                           low5# -> Just ( chr64 low5#, iBox shifted# )        



sanitize :: AppflName -> String
sanitize = id -- letting this happen when we generate InfoTab names now.


sanitizeTC (TyCon b c vs dcs) =
  TyCon b (sanitize c) (map sanitize vs) (map sanitizeDC dcs)
sanitizeDC (DataCon con mts)  =
  DataCon (sanitize con) (map sanitizeMono mts)

sanitizeMono (MVar tv)      = MVar (sanitize tv)
sanitizeMono (MFun m1 m2)   = MFun (sanitizeMono m1) (sanitizeMono m2)
sanitizeMono (MCon b c mts) = MCon b (sanitize c) (map sanitizeMono mts)
-- No other Monotypes should be in the program at this point
sanitizeMono _              = unreachable _HERE

sanitizeObj o = case o of
  THUNK {e, oname     } -> THUNK () (sanitizeExpr e) (sanitize oname)
  FUN   {vs, e, oname } -> FUN () (map sanitize vs) (sanitizeExpr e) (sanitize oname)
  CON   {c, as, oname } -> CON () (sanitize c) (map sanitizeExpr as) (sanitize oname)  

  -- PAP and BLACKHOLE should not be in the program at this point.
  -- This is sanity-checking as much as anything else
  _ -> unreachable _HERE

sanitizeExpr e = case e of
  EAtom   {ea           } -> EAtom () (sanitizeAtom ea)
  EFCall  {ev, eas      } -> EFCall () (sanitize ev) (map sanitizeExpr eas)
  EPrimOp {eprimOp, eas, eopInfo } -> EPrimOp () eprimOp eopInfo (map sanitizeExpr eas)
  ELet    {edefs, ee    } -> ELet () (map sanitizeObj edefs) (sanitizeExpr ee)
  ECase   {ee, ealts    } -> ECase () (sanitizeExpr ee) (sanitizeAlts ealts)

sanitizeAlts Alts{alts, aname, scrt} =
  Alts () (map sanitizeAlt alts) (sanitize aname) (sanitizeExpr scrt)

sanitizeAlt a = case a of
  ACon {ac, avs, ae } -> ACon () (sanitize ac) (map sanitize avs) (sanitizeExpr ae)
  ADef {av, ae      } -> ADef () (sanitize av) (sanitizeExpr ae)

sanitizeAtom a = case a of
  Var v  -> Var (sanitize v)
  LitC c -> LitC (sanitize c)
  _      -> a
  
