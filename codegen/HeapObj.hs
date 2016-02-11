{-# LANGUAGE CPP #-}

#include "../options.h"

module HeapObj (
  showSHOs,
  cSHOs,
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

#if USE_CAST
import CAST
import Text.PrettyPrint(render)
import Language.C.Pretty
#endif


-- HOs come from InfoTabs

shoNames :: [Obj InfoTab] -> [String]
shoNames = map (\o -> showITType o ++ "_" ++ (name . omd) o)

-- return list of forwards (static declarations) and (static) definitions


-- C AST version
#if USE_CAST

showSHOs = error "showSHOs" -- not used in CAST version

cSHOs :: [Obj InfoTab] -> ([CExtDecl], [CExtDecl])
cSHOs os = unzip $ map cSHO os

cSHO :: Obj InfoTab -> (CExtDecl, CExtDecl)
cSHO o =
   let it = omd o
       n = name it
       infoPtr = cStructMember InfoPtrTy "_infoPtr" n
       objType = cStructMember EnumTy "objType" (showObjType it)
       ident = cStructMember StringTy "ident" n
       payload = cSHOspec it
   in cObjStruct n [infoPtr, objType, ident, payload]

payloads xs = cStructMember StructTy "payload" xs

cSHOspec :: InfoTab -> CInitializerMember
cSHOspec (ITFun {}) = payloads ""

cSHOspec (ITPap {}) = error "top level PAP"

cSHOspec it@(ITCon {}) = payloads (map payload (map fst (args it)))

cSHOspec (ITThunk {}) = payloads "0"

cSHOspec (ITBlackhole {}) = payloads "0"

cSHOspec it = error "bad IT in Obj"

cArgTypeElem :: String ->  [CInitializerMember]
cArgTypeElem ty = [cStructMember EnumTy "argType" ty | useArgType]

payload ::  Atom -> [CInitializerMember]
payload (LitI i) = cArgTypeElem "INT" ++
                   [cStructMember IntTy "i" i]

payload (LitD d) = cArgTypeElem "DOUBLE" ++
                   [cStructMember DoubleTy "d" d]

payload (LitF f) = cArgTypeElem "FLOAT" ++
                   [cStructMember FloatTy "f" f]

-- for SHOs atoms that are variables must be SHOs, so not unboxed
payload (Var v) = cArgTypeElem "HEAPOBJ" ++
                  [cStructMember PtrTy "op" ("sho_" ++ v)]

payload _ = error "bad payload"

-- text version
#else

cSHOs = error "cSHOs" -- not used in text version

-- return list of forwards (static declarations) and (static) definitions
showSHOs :: [Obj InfoTab] -> (String, String)
showSHOs objs =
  let (forwards, defs) = unzip $ map showSHO objs
  in (concat forwards, intercalate "\n" defs)


-- maybe should use "static" instead of "extern"
showSHO o =
    let base = "Obj " ++ showITType o ++ "_" ++ (name . omd) o
    in ("extern " ++ base ++ ";\n",
                     base ++ " =\n" ++ showHO (omd o))

getIT it@(ITPap {}) = case knownCall it of
                        Just fit -> fit
                        Nothing -> error "unknown call in PAP"
getIT it = it


showHO it =
    "{\n" ++
    "  ._infoPtr   = &it_" ++ name (getIT it) ++ ",\n" ++
    (if useObjType
    then "  .objType   = " ++ showObjType it      ++ ",\n"
    else "") ++
    "  .ident     = " ++ show (name it)      ++ ",\n" ++
       showSHOspec it ++
    "};\n"

showSHOspec it@(ITFun {}) = payloads []

showSHOspec it@(ITPap {}) = papPayloads it

showSHOspec it@(ITCon {}) = payloads $ map fst $ args it

-- need at least a payload of length 1 for thunks
showSHOspec it@(ITThunk {}) = indent 2 ".payload = {0}\n"

showSHOspec it@(ITBlackhole {}) = indent 2 ".payload = {0}\n"

showSHOspec it = ""

papPayloads it = let as = map fst $ args it
                     n = indent 4 $ payload $ LitI $ papArgsLayout as
                     ap =   indent 4 $ concatMap payload as
                 in  indent 2 ".payload = {\n" ++ n ++ ap ++ "},\n"

papArgsLayout as = let nv = length $ filter isVar as
                       nl = length as - nv
                       bits = 8*sizeOf (CUIntPtr 0)
                   in nv .|. shiftL nl (bits `div` 2)

isVar (Var _) = True
isVar _ = False

payloads as = let ps = indent 4 $ concatMap payload as
              in  indent 2 ".payload = {\n" ++ ps ++ "},\n"

argTypeElem :: String -> String
argTypeElem ty = if useArgType then ".argType = " ++ ty ++ "," else ""

payload (LitI i) = "{" ++ argTypeElem "INT" ++
                  " .i = " ++ show i ++ "},\n"

payload (LitD d) = "{" ++ argTypeElem "DOUBLE" ++
                   " .d = " ++ show d ++ "},\n"

payload (LitF f) = "{" ++ argTypeElem "FLOAT" ++
                   " .f = " ++ show f ++ "},\n"

payload (LitC c) = "{" ++ argTypeElem "INT" ++
                   " .i = con_" ++ c ++ "},\n"

-- for SHOs atoms that are variables must be SHOs, so not unboxed
payload (Var v) = "{" ++ argTypeElem "HEAPOBJ" ++
                  " .op = &sho_" ++ v ++ "},\n"

payload at = error $ "HeapObj.payload: not expecting Atom - " ++ show at

ptrOrLitSHO a =
    "{ " ++
    case a of
      Var v  -> argTypeElem "HEAPOBJ" ++ " .op = &sho_" ++ v   -- these must be global
      LitI i -> argTypeElem "INT" ++ " .i = " ++ show i
      LitL l -> argTypeElem "LONG" ++ " .d = " ++ show l
      LitF f -> argTypeElem "FLOAT" ++ " .f = " ++ show f
      LitD d -> argTypeElem "DOUBLE" ++ " .d = " ++ show d
      LitC c -> argTypeElem "INT" ++ " .i = " ++ "con_" ++ c
    ++ " }"

-- end of USE_CAST
#endif
