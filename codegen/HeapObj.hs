{-
struct _Obj {
  InfoTab *infoPtr;         // canonical location of ObjType field
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  int argCount;             // for PAP, how many args already applied to?
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
    "  .infoPtr   = &it_" ++ name (getIT it) ++ ",\n" ++
    "  .objType   = " ++ showObjType it      ++ ",\n" ++
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
                 in  indent 2 ".argCount = " ++ show (length as) ++ ",\n" ++ 
                              ".payload = {\n" ++ n ++ ap ++ "},\n"
                                                            
papArgsLayout as = let nv = length $ filter isVar as
                       nl = length as - nv
                   in nv .|. shiftL nl 16
                       
isVar (Var _) = True
isVar _ = False

payloads as = let ps = indent 4 $ concatMap payload as
              in  indent 2 ".payload = {\n" ++ ps ++ "},\n"

payload (LitI i) = 
    "{.argType = INT, .i = " ++ show i ++ "},\n"

payload (LitD d) = 
    "{.argType = DOUBLE, .i = " ++ show d ++ "},\n"

-- payload (LitF d) = 
--    "{.argType = FLOAT, .i = " ++ show d ++ "},\n"

-- for SHOs atoms that are variables must be SHOs, so not unboxed
payload (Var v) = 
    "{.argType = HEAPOBJ, .op = &sho_" ++ v ++ "},\n"

payload at = error $ "HeapObj.payload: not expecting Atom - " ++ show at

ptrOrLitSHO a =
    "{ " ++
    case a of
      LitI i -> ".argType = INT, .i = " ++ show i
      LitB b -> ".argType = BOOL, .b = " ++ (if b then "true" else "false")
      LitD d -> ".argType = DOUBLE, .d = " ++ show d
      LitF f -> ".argType = FLOAT, .f = " ++ show f
      LitC c -> ".argType = CHAR, .c = " ++ show c
      Var v -> ".argType = HEAPOBJ, .op = &sho_" ++ v   -- these must be global
    ++ " }"
