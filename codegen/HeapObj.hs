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

import AST
import InfoTab
import Prelude

-- // two = CON(I 2)
-- Obj sho_two = 
--   { .objType = CON,
--     .infoPtr = &it_I,
--     .payload[0].argType = INT,
--     .payload[0].i = 2
--   };

-- HOs come from InfoTabs

showSHOs :: [Obj InfoTab] -> String
showSHOs = (concatMap showSHO) . (map omd)

showSHO it = 
    "Obj sho_" ++ name it ++ " =\n" ++ showHO it

showHO it =
    "(Obj) {\n" ++
    "  .infoPtr   = &it_" ++ name it ++ ",\n" ++
    "  .objType   = " ++ showObjType it      ++ ",\n" ++
    "  .ident     = " ++ show (name it)      ++ ",\n" ++
       showSHOspec it ++
    "  };\n"

showSHOspec it@(Fun {}) = 
    "  .funFields.arity = " ++ show (arity it) ++ ",\n"

showSHOspec it@(Pap {}) = ""

showSHOspec it@(Con {}) = payload $ args it

showSHOspec it@(Thunk {}) = ""

showSHOspec it@(Blackhole {}) = ""

payload as =
  concat [ "  .payload[ " ++ show i ++ " ] = " ++ ptrOrLitSHO a ++ ",\n"
           | (i,a) <- zip [0..] as ]

ptrOrLitSHO a =
    "(PtrOrLiteral) { " ++
    case a of
      Lit i -> ".argType = INT, .i = " ++ show i
      Var v -> ".argType = HEAPOBJ, .op = &sho_" ++ v   -- these must be global
    ++ " }"
