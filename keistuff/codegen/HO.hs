module HO(
)

where

import Prelude
import Parser

{-
struct _Obj {
  InfoTab *infoPtr;         // canonical location of ObjType field
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  int argCount;             // for PAP, how many args already applied to?
  char ident[64];           // temporary, just for tracing
  PtrOrLiteral payload[32]; // fixed for now
};

typedef struct {
  ArgType argType;        // superfluous, for sanity checking
  union {
    int i;
    double d;
    Obj *op;
  };
} PtrOrLiteral;

typedef enum {          // superfluous, for sanity checking
  INT, 
  DOUBLE, 
  HEAPOBJ 
} ArgType;


-}

data PtrOrLiteral =
    INT Int
  | DOUBLE Double
  | FLOAT Float
  | HEAPOBJ String

data RVal = 
    SHOVal String   -- static heap object
  | HOVal     -- heap object
  | CCVal     -- case continuation
  | AConVal   -- case alt
  | ADefVal   -- case alt
  | FPVal     -- formal parameter

type Env = (Var, RVal)

envLookup env v = HEAPOBJ "not implemented"

{-
shoObj sho_one = 
  { .objType = CON,
    .infoPtr = &it_I,
    .payload[0].argType = INT,
    .payload[0].i = 1
  };
-}
data HO = 
    HO { infoTab :: InfoTab, -- .infoPtr, .objType etc from this
         argCount :: Int,
         ident :: String,
         payload :: [PtrOrLiteral]
       }

makeSHOs :: [Def] -> [HO]
-- SHOs don't need an env because they have no free variables
makeSHOs defs = map $ makeSHO []

--    let vs = map fst defs
--        env = [(v, SHOVal "sho_" ++ v)]
--    in map $ makeSHO env

-- during codegen this will take an env to compute the payload
-- makeHO :: Def -> a -> HO

makeHO it = 
    HO { infoTab = it,
         argCount = 0, -- for PAP only, quash the warning
         ident = v,
         payload = map envLookup fvs
       }

{-
shoObj sho_one = 
  { .objType = CON,
    .infoPtr = &it_I,
    .ident = "one",
    .payload[0].argType = INT,
    .payload[0].i = 1

    .payload[0] = (PtrOrLiteral) {.argType = INT, .i = 3}

  };
-}
showHO it pl =
    "(Obj) { .infoPtr = " ++ "&it_" ++ name it ++ ",\n" ++
    "        .objType = " ++ showObjType it ++ ",\n" ++
    "        .ident   = " ++ show (name it) ++ ",\n" ++
     sh
    "        .


{-
makeHO env (v, o@(FUN fvs vs e)) = 
makeHO env (v, PAP fvs f as) =
makeHO env (v, CON fvs c as) =
makeHO env (v, THUNK fvs e) =
makeHO env (v, BLACKHOLE) =

    HO { infoPtr = ,
         objType = ,
         argCount = ,
         ident = ,
         payload = 
       }
-}    
