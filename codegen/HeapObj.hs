{-
struct _Obj {
  InfoTab *infoPtr;         // canonical location of ObjType field
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  char ident[64];           // temporary, just for tracing
  PtrOrLiteral payload[32]; // fixed for now
};

(Obj) 
    { .objType = CON,
      .infoPtr = &it_Left,
      .payload[0] = (PtrOrLiteral) {.argType = HEAPOBJ, .op = &sho_one}
    };

Obj sho_main3 =
  { .objType = THUNK,
    .infoPtr = &it_main3,
  };

Obj sho_one = 
  { .objType = CON,
    .infoPtr = &it_I,
    .payload[0].argType = INT,
    .payload[0].i = 1
  };

-}

{-# LANGUAGE CPP #-}
#include "options.h"

module HeapObj (
  showSHOs,
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


-- // two = CON(I 2)
-- Obj sho_two = 
--   { .objType = CON,
--     .infoPtr = &it_I,
--     .payload[0].argType = INT,
--     .payload[0].i = 2
--   };

-- HOs come from InfoTabs

shoNames :: [Obj InfoTab] -> [String]
shoNames = map (\o -> showITType o ++ "_" ++ (name . omd) o)

-- return list of forwards (static declarations) and (static) definitions
showSHOs :: [Obj InfoTab] -> (String,String)
showSHOs objs = 
    let (forwards, defs) = unzip $ map showSHO objs
    in (concat forwards, intercalate "\n" defs)


#if USE_CAST

showSHO o = 
   let n = "sho_" ++ (name . omd) o
    in ("extern Obj " ++ n ++ ";\n",(render (pretty (cSHO o))))
                    
cSHO o =
   let it = omd o
       n = name it
       infoPtr = cStructMember InfoPtrTy "infoPtr" n
       objType = cStructMember EnumTy "objType" (showObjType it)
       ident = cStructMember StringTy "ident" n
       payload = cSHOspec it
   in cObjStruct n [infoPtr, objType, ident, payload]
   
payloads xs = cStructMember StructTy "payload" xs
   
cSHOspec it@(Fun {}) = payloads ""
   
cSHOspec it@(Pap {}) = error "top level PAP"


cSHOspec it@(Con {}) = payloads (map payload (map fst (args it))) 
   
cSHOspec it@(Thunk {}) = payloads "0"
   
cSHOspec it@(Blackhole {}) = payloads "0"
   
cSHOspec it = error "bad IT in Obj"

--payload ::  Atom -> [CAST.CInitializerMember Language.C.Data.Node.NodeInfo]
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

-- end of USE_CAST
#else 

-- maybe should use "static" instead of "extern"
showSHO o =
    let base = "Obj " ++ showITType o ++ "_" ++ (name . omd) o
    in ("extern " ++ base ++ ";\n", 
                     base ++ " =\n" ++ showHO (omd o))

getIT it@(Pap {}) = case knownCall it of 
                        Just fit -> fit
                        Nothing -> error "unknown call in PAP"
getIT it = it


showHO it =
    "{\n" ++
    "  .infoPtr   = (uintptr_t)&it_" ++ name (getIT it) ++ ",\n" ++
#if USE_OBJTYPE    
    "  .objType   = " ++ showObjType it      ++ ",\n" ++
#endif
    "  .ident     = " ++ show (name it)      ++ ",\n" ++
       showSHOspec it ++
    "};\n"

showSHOspec it@(Fun {}) = payloads []

showSHOspec it@(Pap {}) = papPayloads it

showSHOspec it@(Con {}) = payloads $ map fst $ args it

-- need at least a payload of length 1 for thunks
showSHOspec it@(Thunk {}) = indent 2 ".payload = {0}\n"

showSHOspec it@(Blackhole {}) = indent 2 ".payload = {0}\n"

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
      LitL f -> ".l = " ++ show l
      LitF f -> ".f = " ++ show f
      LitD d -> ".d = " ++ show d
      LitC c -> ".i = " ++ "con_" ++ c
#endif
    ++ " }"
    
#endif
