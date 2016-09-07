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
showSHOs objs =  unzip $ map showSHO objs

showSHO :: Obj InfoTab -> (Definition, Definition)
showSHO o =
  ([cedecl| extern typename Obj $id:n; |],
   [cedecl| typename Obj $id:n = $init:(showHO (omd o)); |])
  where n = "sho_" ++ (name . omd) o

getIT :: InfoTab -> InfoTab
getIT it@(ITPap {}) = case knownCall it of
                        Just fit -> fit
                        Nothing -> error "unknown call in PAP"
getIT it = it


showHO :: InfoTab -> Initializer
showHO it =
  if useObjType then
   [cinit|
     {
       ._infoPtr = &$id:("it_" ++ name (getIT it)),
       .objType = $id:(showObjType it),
       .ident = $string:(name it),
       .payload = $init:(showSHOspec it)
     }
   |]
  else
    [cinit|
      {
        ._infoPtr = &$id:("it_" ++ name (getIT it)),
        .ident = $string:(name it),
        .payload = $init:(showSHOspec it)
      }
    |]

showSHOspec :: InfoTab -> Initializer
showSHOspec it@(ITFun {}) = [cinit| {{0}} |]
showSHOspec it@(ITThunk {}) = [cinit| {{0}} |]
--BH showSHOspec it@(ITBlackhole {}) = [cinit| {0} |]
showSHOspec it@(ITCon {}) = [cinit| { $inits:(payloads $ map fst $ args it) } |]
showSHOspec it@(ITPap {}) = papPayloads it
showSHOspec it = error $ "showSHOspec " ++ show it

papPayloads :: InfoTab -> Initializer
papPayloads it =
  let as = map fst $ args it
      n = payload $ LitI $ to64 $ papArgsLayout as
      ap = map payload as
  in [cinit| { $inits:(n:ap) } |]

papArgsLayout :: [Atom] -> Int
papArgsLayout as =
  let nv = length $ filter isVar as
      nl = length as - nv
      bits = 8*sizeOf (CUIntPtr 0)
  in nv .|. shiftL nl (bits `div` 2)

isVar :: Atom -> Bool
isVar (Var _) = True
isVar _ = False

payloads :: [Atom] -> [Initializer]
payloads = map payload

payload :: Atom -> Initializer
payload (LitI i) =
  if useArgType then
    [cinit| {.argType = INT, .i = $int:i}|]
  else
    [cinit| {.i = $int:i}|]

payload (LitD d) =
  if useArgType then
    [cinit| {.argType = DOUBLE, .d = $double:d'}|]
  else
    [cinit| {.d = $double:d'}|]
  where d' = toRational d

payload (LitC c) =
  if useArgType then
    [cinit| {.argType = INT, .i = $id:con}|]
  else
    [cinit| {.i = $id:con}|]
  where con = "con_" ++ c

payload (Var v) =
  if useArgType then
    [cinit| {.argType = HEAPOBJ, .op = &$id:sho}|]
  else
    [cinit| {.op = &$id:sho}|]
  where sho = "sho_" ++ v

payload (LitStr s) =
  if useArgType then
    [cinit| {.argType = STRING, .s = $string:s}|]
  else
    [cinit| {.s = $string:s}|]
