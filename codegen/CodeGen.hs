{-

  We need the free variables of subtrees for two reasons:
  * for constructing an object (closure), to know what to put it its payload
  * and to know what needs to be stashed in a continuation

  Conversely, when generating code for an expression, we need to know where
  to find each variable.  This is done by maintaining a stack of different
  kinds of `exposures' of variable values:

  * SHOs - just one set of these at the bottom of the stack
  * HOs, following a "let"
  * Formal function parameters, these are named parameters (alternatively,
    could not name and use a CallCont like stgApply--TODO:  think about this)
  * Popped CaseCont vars
  * Alt constructor vars or default var (TODO:  these could be formal function
    params if individual alts are separate functions)

In more detail:

SHO:  referenced with absolute memory address, e.g. "sho_unit"

HO:  address calculated from TOH pointer, e.g. "((Obj *)TOH_ptr)[-3]"

Formal parameter:  by name, e.g. "x"

CaseCont:  "myCaseCont.payload[i]"

Alt constructor var:  "stgCurVal.op->payload[i], bind these"

Alt default var:  "stgCurVal, bind it"

-}

module CodeGen(
  cgObjs
) where

import Parser
import InfoTab
import HeapObj

import Prelude
import Data.List(intercalate,nub)

import Data.Map (Map)
import qualified Data.Map as Map

newtype State s a = State (s -> (a, s))

state :: (s -> (a, s)) -> State s a
state x = State x

runState :: State s a -> s -> (a, s)
runState (State f) x = f x

instance Monad (State s) where
    return x = state (\st -> (x, st))
    act >>= k = state $ \st -> 
                          let (x, st') = runState act st
                          in runState (k x) st'

get = State $ \s -> (s,s)

put newState = State $ \s -> ((), newState)  

liftM   :: (Monad m) => (a1 -> r) -> m a1 -> m r
liftM f m1              = do { x1 <- m1; return (f x1) }

concatMapM        :: (Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs   =  liftM concat (mapM f xs)

data RVal = SHO           -- static heap obj
          | HO Int        -- heap obj, Int is size, count back
          | FP            -- formal param, use name as is
          | CC String Int -- named case continuation
          | FV Int        -- free var, self->payload[Int]
          | AC Int        -- alt con, use name as is?
          | AD            -- alt def, use name as is?

type Env = [(String, RVal)]

getEnvRef :: String -> Env -> String
getEnvRef v env = lu v env 0

lu v [] _ = error $ "lu " ++ v ++ " failed"

lu v ((v',k):_) n | v == v' =
    case k of
      SHO     -> "HOTOPL(&sho_" ++ v ++ ")"
      HO size -> "HOTOPL(&((Obj*)TOH_ptr)[-" ++ show (n + size) ++ "]"
      FP      -> v
      CC cc i -> cc ++ ".payload[" ++ show i ++ "]"
      FV i    -> "self.op->payload[" ++ show i ++ "]"
      AC i    -> "ctor.op->payload[" ++ show i ++ "]"
      AD      -> "ctor"

lu v ((_, HO size) : xs) n =
    lu v xs (n+size)

lu v (x : xs) n = lu v xs n


indent i xs = (take i $ repeat ' ') ++ indent' i xs
    where
      indent' i ('\n':x:xs) = '\n' : (take i $ repeat ' ') ++ indent' i (x:xs)
      indent' i "\n"        = "\n"
      indent' i (x:xs)      = x : indent' i xs
      indent' i ""          = ""

-- CG Atom, Var ************************************************************

cgv env v = "HOTOPL(" ++ getEnvRef v env ++ ")"

cga :: Env -> Atom -> String
cga env (Lit i) = "HOTOLIT(" ++ show i ++ ")"
cga env (Var v) = cgv env v

-- CG in the state monad ***************************************************
--   need a fresh variable supply, should have made Alts a proper
--   data declaration
-- CG of objects produces no inline code
--   FUN and THUNK produce a DEFUN
--   all objects produce a (S)HO
-- for CG, objects are heap allocated only by let

cgObjs :: [Obj InfoTab] -> ([String],[String])
cgObjs objs =
    let tlnames = map (name . omd) objs
        env = zip tlnames $ repeat SHO
        (funcs, _) = runState (cgos env objs) 0  
        (forwards, fundefs) = unzip funcs
    in (forwards, fundefs)

-- return [(forward,fundef)], will be unzipped at top level

cgos :: Env -> [Obj InfoTab] -> State Int [(String, String)]
cgos env = concatMapM (cgo env)

cgo :: Env -> Obj InfoTab -> State Int [(String, String)]
cgo env (FUN it vs e name) =
    do 
      let env' = zip (fvs it) (map FV [0..]) ++ 
                 zip vs (repeat FP) ++
                 env
      (inline, funcs) <- cge env' e
      let forward = "FnPtr " ++ name ++ "();"
      let func =
            "DEFUN" ++ show (length vs + 1) ++ "(" ++ 
            name ++ ", self, " ++
            intercalate ", " vs ++
            ") {\n" ++
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
               indent 2 inline ++
            "  STGRETURN0();\n" ++  -- in case inline doesn't jump somewhere else
            "  ENDFUN;\n}"
      return $ (forward, func) : funcs

cgo env (PAP it f as name) =
    return []

cgo env (CON it c as name) =
    return []

cgo env (THUNK it e name) =
    do 
      let env' = zip (fvs it) (map FV [0..]) ++ env
      (inline, funcs) <- cge env' e
      let forward = "FnPtr " ++ name ++ "();"
      let func =
            "DEFUN1(" ++ name ++ ", self) {\n" ++
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");" ++
            "  stgThunk(self);\n" ++
            indent 2 inline ++
            "  STGRETURN0();\n" ++  -- in case inline doesn't jump somewhere else
            "  ENDFUN;\n}"
      return $ (forward, func) : funcs

cgo env (BLACKHOLE {}) =
    return []

-- ****************************************************************
-- return (inline code, [(forward, fundef)])
cge :: Env -> Expr InfoTab -> State Int (String, [(String, String)])
cge env (EAtom it l@(Lit _)) =
    return ("stgCurVal = " ++ cga env l ++ ";\n", [])

cge env (EAtom it v@(Var _)) = 
    let inline = "stgCurVal = " ++ cga env v ++ ";\n" ++
                 "STGEVAL(stgCurVal);\n"
    in return (inline, [])

cge env (EFCall it f as) = 
    let inline = "STGAPPLY" ++ show (length as) ++ "(" ++
                 intercalate ", " (cgv env f : map (cga env) as) ++ 
                 ");\n"
    in return (inline, [])

cge env (EPrimop{}) = return ("cge(EPrimop) not implemented\n", [])

cge env (ELet it os e) =
    let names = map oname os
        env'  = zip names (map HO (scanr (flip (-)) 0 sizes)) ++ env
        (sizes, buildcodes) = unzip $ map (buildHeapObj env') os
    in do
      ofunc <- cgos env' os
      (einline, efunc) <- cge env' e
      return (concat buildcodes ++ einline,
              ofunc ++ efunc)
        
cge env (ECase it e alts) = 
    do (ecode, efunc) <- cge env e
       afunc <- cgalts env alts
       let afvs = concatMap (fvs . amd) alts
       let pre = "stgPushCont( (Cont)\n" ++
                 "  { .retAddr = &alts1,\n" ++
                      indent 4 (loadPayloadFVs env afvs) ++
                 "  });\n"
       return (pre ++ ecode, efunc ++ afunc)
              
-- CG Alts ************************************************************
-- DEFUN0(alts1) {
--   Cont cont = stgPopCont();
--   PtrOrLiteral ctor = stgCurVal;
--   if (stgCurVal.argType != HEAPOBJ ||
--       stgCurVal.op->objType != CON ) goto casedefault;
--   switch(ctor.op->infoPtr->conFields.tag) {
--   case TagLeft:
--     // variable saved in casecont
--     stgCurVal = cont.payload[0];
--     STGRETURN0();
--   case TagRight:
--     // from constructor
--     stgCurVal = ctor.op->payload[0];
--     STGRETURN0();
--   default:
--   casedefault:
--     stgCurVal = ctor;
--     STGRETURN0();
--   }
--   ENDFUN;
-- }

-- get into monad
newsuffix = State $ \i -> (show i, i+1)

-- returns just function definitions
cgalts :: [(Var, RVal)] -> [Alt InfoTab] -> State Int [(String,String)]
cgalts env alts = 
    let altenv = zip (nub $ concatMap (fvs . amd) alts)
                     [ CC "cont" i | i <- [0..] ]
        env' = altenv ++ env
    in do
      suf <- newsuffix
      let forward = "FnPtr " ++ "alts" ++ suf ++ "();"
      codefuncs <- mapM (cgalt env') alts
      let (codes, funcss) = unzip codefuncs
      let code = "DEFUN0(alts" ++ suf ++ ") {\n" ++
                 "  Cont cont = stgPopCont();\n" ++
                 "  PtrOrLiteral ctor = stgCurVal;\n" ++
                 "  if (stgCurVal.argType != HEAPOBJ ||\n" ++
                 "      stgCurVal.op->objType != CON ) goto casedefault;\n" ++
                 "  switch(ctor.op->infoPtr->conFields.tag) {\n" ++
                      indent 4 (concat codes) ++
                 "  default:\n" ++
                 "  casedefault:\n" ++
                 "    stgCurVal = ctor;\n" ++
                 "    STGRETURN0();\n" ++
                 "  }\n" ++
                 "  ENDFUN;\n" ++
                 "}\n"
      return $ (forward, code) : concat funcss

cgalt env (ACon it c vs e) =
    let eenv = zip vs (map AC [0..])
        env' = eenv ++ env
    in do
      (inline, func) <- cge env' e
      let (arity, tag) = case Map.lookup c (conMap it) of
                           Nothing -> error "conMap lookup error"
                           Just x -> x
      let code = "case " ++ show tag ++ ":\n" ++
                    indent 2 inline ++
                 "  STGRETURN0();\n"
      return (code, func)
              
cgalt env (ADef it v e) =
    let env' = (v, AD) : env
    in do
      (inline, func) <- cge env' e
      let code = "default:\n" ++
                 "casedefault:\n" ++
                    indent 2 inline ++
                 "  STGRETURN0();\n"
      return (code, func)


-- ****************************************************************
-- build heap object (bho) is only invoked by 'let' so TLDs not built

--   // y = THUNK( x );
--   Obj *y = stgNewHeapObj();
--   *y = (Obj) 
--     { .objType = THUNK,
--       .infoPtr = &it_y,
--       .payload[0] = (PtrOrLiteral) { .argType = HEAPOBJ, 
-- 				     .op = STGHEAPAT(-2) }
--     };
-- return (size, inline code)


--  UPDCONT, 
--  CASECONT, 
--  CALLCONT, 
--  FUNCONT,

buildHeapObj env o =
    let (size, rval) = heapObjRVal env o
        name = oname o
        code =
            "Obj *" ++ name ++ " = stgNewHeapObj();\n" ++
            "*" ++ name ++ " = " ++ rval  ++ ";\n"
    in (size, code)

heapObjRVal env o =
    let (size, guts) = bho env o
        name = oname o
        code =
            "(Obj) { .objType = " ++ showObjType (omd o) ++ ",\n" ++
            "        .infoPtr = &it_" ++ name ++ ",\n" ++
            "        .ident = \"" ++ name ++ "\",\n" ++
                     indent 8 guts ++
            "      }"
    in (size, code)

bho env (FUN it vs e name) =
    (1, loadPayloadFVs env (fvs it))

bho env (PAP it f as name) =
    (1, loadPayloadFVs env (fvs it) ++ loadPayloadAtoms env as (length $ fvs it))

bho env (CON it c as name) = 
    let size = 1
        code = concat ["    .payload[" ++ show i ++ "] = " ++ cga env a ++ ",\n"
                       | (i,a) <- indexFrom 0 as ]
    in (size, code)

bho env (THUNK it e name) =
    (1, loadPayloadFVs env (fvs it))

bho env (BLACKHOLE it name) = (1,"")


loadPayloadFVs env fvs =
    concat [".payload[" ++ show i ++ "] = " ++ cgv env v ++ ",\n"
            | (i,v) <- indexFrom 0 fvs ]

-- load atoms into payload starting at index ind
loadPayloadAtoms env as ind =
    concat [".payload[" ++ show i ++ "] = " ++ cga env a ++ ",\n"
            | (i,a) <- indexFrom ind as]


indexFrom i xs = zip [i..] xs
