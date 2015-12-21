{-# LANGUAGE CPP #-}
#include "options.h"

module HeapObj (
  showSHOs,
  cSHOs,
  shoNames
) where

import AST
import InfoTab
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
       infoPtr = cStructMember InfoPtrTy "infoPtr" n
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

payload ::  Atom -> [CInitializerMember]
payload (LitI i) = [
#if USE_ARGTYPE
    cStructMember EnumTy "argType" "INT",
#endif
    cStructMember IntTy "i" i
    ]

payload (LitD d) = [
#if USE_ARGTYPE
    cStructMember EnumTy "argType" "DOUBLE",
#endif
    cStructMember DoubleTy "d" d
    ]


payload (LitF f) = [
#if USE_ARGTYPE
    cStructMember EnumTy "argType" "FLOAT",
#endif
    cStructMember FloatTy "f" f
    ]

-- for SHOs atoms that are variables must be SHOs, so not unboxed
payload (Var v) = [
#if USE_ARGTYPE
    cStructMember EnumTy "argType" "HEAPOBJ",
#endif
    cStructMember PtrTy "op" ("sho_" ++ v)
    ]

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
#if USE_OBJTYPE
    "  .objType   = " ++ showObjType it      ++ ",\n" ++
#endif
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

payload (LitI i) =
#if USE_ARGTYPE
    "{.argType = INT, .i = " ++ show i ++ "},\n"
#else
    "{.i = " ++ show i ++ "},\n"
#endif


payload (LitD d) =
#if USE_ARGTYPE
    "{.argType = DOUBLE, .d = " ++ show d ++ "},\n"
#else
    "{.d = " ++ show d ++ "},\n"
#endif

payload (LitF f) =
#if USE_ARGTYPE
   "{.argType = FLOAT, .f = " ++ show f ++ "},\n"
#else
    "{.f = " ++ show f ++ "},\n"
#endif

payload (LitC c) =
#if USE_ARGTYPE
   "{.argType = INT, .i = con_" ++ c ++  "},\n"
#else
   "{.i = con_" ++ c ++ "},\n"
#endif

-- for SHOs atoms that are variables must be SHOs, so not unboxed
payload (Var v) =
#if USE_ARGTYPE
    "{.argType = HEAPOBJ, .op = &sho_" ++ v ++ "},\n"
#else
    "{.op = &sho_" ++ v ++ "},\n"
#endif

payload at = error $ "HeapObj.payload: not expecting Atom - " ++ show at

ptrOrLitSHO a =
    "{ " ++
    case a of
#if USE_ARGTYPE
      Var v  -> ".argType = HEAPOBJ, .op = &sho_" ++ v   -- these must be global
      LitI i -> ".argType = INT, .i = " ++ show i
      LitL l -> ".argType = LONG, .d = " ++ show l
      LitF f -> ".argType = FLOAT, .f = " ++ show f
      LitD d -> ".argType = DOUBLE, .d = " ++ show d
      LitC c -> ".argType = INT, .i = " ++ "con_" ++ c
#else
      Var  v -> ".op = &sho_" ++ v   -- these must be global
      LitI i -> ".i = " ++ show i
      LitL l -> ".l = " ++ show l
      LitF f -> ".f = " ++ show f
      LitD d -> ".d = " ++ show d
      LitC c -> ".i = " ++ "con_" ++ c
#endif
    ++ " }"

-- end of USE_CAST
#endif
