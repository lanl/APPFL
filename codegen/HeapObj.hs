{-# LANGUAGE QuasiQuotes #-} 

{-# LANGUAGE CPP #-}
#include "../options.h"

module HeapObj (
  showSHOs,
  shoNames
) where

import AST
import InfoTab
import Options
import Prelude
import Util
import Data.List (intercalate)
import Data.Bits
import Foreign.Storable
import Foreign.C.Types
import Language.C.Quote.GCC
import Language.C.Syntax (Definition, Initializer)

-- HOs come from InfoTabs

shoNames :: [Obj InfoTab] -> [String]
shoNames = map (\o -> "sho_" ++ (name . omd) o)

-- return list of forwards (static declarations) and (static) definitions
showSHOs :: [Obj InfoTab] -> ([Definition], [Definition])
showSHOs objs =  unzip $ map cshowSHO objs

cshowSHO :: Obj InfoTab -> (Definition, Definition)
cshowSHO o = 
  ([cedecl| extern typename Obj $id:n; |],
   [cedecl| typename Obj $id:n = $init:(cshowHO (omd o)); |])
  where n = "sho_" ++ (name . omd) o

getIT :: InfoTab -> InfoTab
getIT it@(ITPap {}) = case knownCall it of
                        Just fit -> fit
                        Nothing -> error "unknown call in PAP"
getIT it = it

cshowHO :: InfoTab -> Initializer
cshowHO it = 
   [cinit|
     {
       ._infoPtr = &$id:("it_" ++ name (getIT it)),
#if USE_OBJTYPE
       .objType = $id:(showObjType it),
#endif
       .ident = $string:(name it),
       .payload = $init:(cSHOspec it)
     }     
   |]
   
cSHOspec :: InfoTab -> Initializer
cSHOspec it@(ITFun {}) = [cinit| {0} |]
cSHOspec it@(ITThunk {}) = [cinit| {0} |]
cSHOspec it@(ITBlackhole {}) = [cinit| {0} |]
cSHOspec it@(ITCon {}) = [cinit| { $inits:(cpayloads $ map fst $ args it) } |]
cSHOspec it@(ITPap {}) = cpapPayloads it
cSHOspec it = error $ "cSHOspec " ++ show it

cpapPayloads :: InfoTab -> Initializer
cpapPayloads it = let as = map fst $ args it
                      n = cpayload $ LitI $ papArgsLayout as
                      ap = map cpayload as
                  in [cinit| { $inits:(n:ap) } |]  

papArgsLayout as = let nv = length $ filter isVar as
                       nl = length as - nv
                       bits = 8*sizeOf (CUIntPtr 0)
                   in nv .|. shiftL nl (bits `div` 2)

isVar (Var _) = True
isVar _ = False

cpayloads :: [Atom] -> [Initializer]
cpayloads = map cpayload

cpayload :: Atom -> Initializer
cpayload (LitI i) = 
  if useArgType then
    [cinit| {.argType = INT, .i = $int:i}|]
  else
    [cinit| {.i = $int:i}|]
   
cpayload (LitD d) = 
  if useArgType then
    [cinit| {.argType = DOUBLE, .d = $double:d'}|]
  else
    [cinit| {.d = $double:d'}|]
  where d' = toRational d

cpayload (LitF f) = 
  if useArgType then
    [cinit| {.argType = FLOAT, .f = $float:f'}|]
  else
    [cinit| {.f = $float:f'}|]
  where f' = toRational f

cpayload (LitC c) = 
  if useArgType then
    [cinit| {.argType = INT, .i = $id:con}|]
  else
    [cinit| {.i = $id:con}|]
  where con = "con_" ++ c

cpayload (Var v) = 
  if useArgType then
    [cinit| {.argType = HEAPOBJ, .op = &$id:sho}|]
  else
    [cinit| {.i = &$id:sho}|]
  where sho = "sho_" ++ v

cpayload at = error $ "HeapObj.cpayload: not expecting Atom - " ++ show at
 
