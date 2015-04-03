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
  showSHOs
) where

import Parser
import InfoTab

import Prelude


-- HOs come from InfoTabs
showHOcom it =
    "(Obj) {\n" ++
    "  .infoPtr   = &InfoTab it_" ++ name it ++ ",\n" ++
    "  .objType   = " ++ showObjType it      ++ ",\n" ++
    "  .ident     = " ++ show (name it)      ++ ",\n" ++
    "  };\n"

showSHOs :: [Obj InfoTab] -> String
showSHOs = (concatMap showSHO) . (map omd)

showSHO it = 
    let itName = name it 
    in
      "Obj sho_" ++ name it ++ " =\n" ++
      showHOcom it ++
      showSHOspec itName it

showSHOspec itName it@(Fun {}) = 
    itName ++ ".funFields.arity = " ++ show (arity it) ++ ";\n"

showSHOspec itName it@(Pap {}) = ""

showSHOspec itName it@(Con {}) =
    payload itName $ args it

showSHOspec itName it@(Thunk {}) = ""

showSHOspec itName it@(Blackhole {}) = ""

payload itName as =
  concat [ itName ++ ".payload[ " ++ show i ++ " ] = " ++ ptrOrLitSHO a ++ ";\n"
           | (i,a) <- zip [0..] as ]

ptrOrLitSHO a =
    "(PtrOrLiteral) { " ++
    case a of
      Lit i -> ".argType = INT, .i = " ++ show i
      Var v -> ".argType = HEAPOBJ, .op = &sho_" ++ v   -- these must be global
    ++ " }\n"
