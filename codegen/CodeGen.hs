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

import AST
import InfoTab
import HeapObj
import State
import Analysis

import Prelude
import Data.List(intercalate,nub)

import Data.Map (Map)
import qualified Data.Map as Map

data RVal = SHO           -- static heap obj
          | HO Int        -- heap obj, Int is size, count back
          | FP            -- formal param, use name as is
          | CC String Int -- named case continuation
          | FV Int        -- free var, self->payload[Int]
          | AC Var Int    -- alt con
          | AD Var        -- alt def
            deriving(Eq,Show)

type Env = [(String, RVal)]

getEnvRef :: String -> Env -> String
getEnvRef v env = lu v env 0

lu v [] _ = error $ "lu " ++ v ++ " failed"

lu v ((v',k):_) n | v == v' =
    case k of
      SHO     -> "HOTOPL(&sho_" ++ v ++ ")"
      -- HOTOPL(STGHEAPAT(-1))
      HO size -> "HOTOPL(STGHEAPAT(" ++ show (-(size + n)) ++ "))"
      FP      -> v
      CC cc i -> cc ++ ".payload[" ++ show i ++ "]"
      FV i    -> "self.op->payload[" ++ show i ++ "]"
      AC v i  -> v ++ "->payload[" ++ show i ++ "]"
      AD v    -> v

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

--cgv env v = "HOTOPL(" ++ getEnvRef v env ++ ")"
cgv env v = getEnvRef v env -- ++ "/* " ++ v ++ " */"

cga :: Env -> Atom -> String
cga env (Lit i) = "(PtrOrLiteral){.argType = INT, .i = " ++ show i ++ " }"
cga env (Var v) = cgv env v

cgUBa env (Lit i) = show i
cgUBa env (Var v) = "(" ++ cgv env v ++ ").i"

-- CG in the state monad ***************************************************
--   need a fresh variable supply, should have made Alts a proper
--   data declaration
-- CG of objects produces no inline code
--   FUN and THUNK produce a DEFUN
--   all objects produce a (S)HO
-- for CG, objects are heap allocated only by let

cgObjs :: [Obj InfoTab] -> [String] -> ([String],[String])
cgObjs objs runtimeGlobals =
    let tlnames = runtimeGlobals ++ map (name . omd) objs
        env = zip tlnames $ repeat SHO
        (funcs, _) = runState (cgos env objs) 0  
        (forwards, fundefs) = unzip funcs
        (forward, fundef) = statObjFun objs
    in (forward:forwards, fundef:fundefs)

statObjFun objs = 
    ("void registerSHOs();",
     "void registerSHOs() {\n" ++
        concat [ "  stgStatObj[stgStatObjCount++] = &" ++ s ++ ";\n" 
                 | s <- shoNames objs ] ++
     "}\n")

-- return [(forward,fundef)], will be unzipped at top level

cgos :: Env -> [Obj InfoTab] -> State Int [(String, String)]
cgos env = concatMapM (cgo env)

cgo :: Env -> Obj InfoTab -> State Int [(String, String)]
cgo env o@(FUN it vs e name) =
    do 
      let env' = zip (fvs it) (map FV [0..]) ++ 
                 zip vs (repeat FP) ++
                 env
      (inline, funcs) <- cge env' e
--      let name' = showITType o ++ "_" ++ name
--      let forward = "FnPtr " ++ name' ++ "();"
      let forward = "FnPtr fun_" ++ name ++ "();"
      let func =
            "DEFUN" ++ show (length vs + 1) ++ "(fun_" ++ 
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

cgo env o@(THUNK it e name) =
    do 
      let env' = zip (fvs it) (map FV [0..]) ++ env
      (inline, funcs) <- cge env' e
--      let name' = showITType o ++ "_" ++ name
--      let forward = "FnPtr " ++ name' ++ "();"
      let forward = "FnPtr fun_" ++ name ++ "();"
      let func =
            "DEFUN1(fun_" ++ name ++ ", self) {\n" ++
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
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
cge env (EAtom it a@(Lit i)) =
    return ("stgCurVal = " ++ cga env a ++ "; " ++ "// " ++ show i ++ "\n", [])

cge env (EAtom it a@(Var v)) = 
    let inline = "stgCurVal = " ++ cga env a ++ "; " ++ "// " ++ v ++ "\n" 
                 -- this is wrong! ++ "STGEVAL(stgCurVal);\n"
    in return (inline, [])

cge env (EFCall it f as) = 
    let inline = "// " ++ f ++ " " ++ showas as ++ "\n" ++
                 "STGAPPLY" ++ show (length as) ++ "(" ++
                 intercalate ", " (cgv env f : map (cga env) as) ++ 
                 ");\n"
    in return (inline, [])

cge env (EPrimop it op as) = 
    let arg0 = cgUBa env (as !! 0)
        arg1 = cgUBa env (as !! 1)
        inline = case op of
                   Piadd -> cInfix " + "
                   Pisub -> cInfix " - "
                   Pimul -> cInfix " * "
                   Pidiv -> cInfix " / "
                   Pimod -> cInfix " % "
                   Pieq ->  cInfix " == "
                   Pine ->  cInfix " != "
                   Pilt ->  cInfix " < "
                   Pile ->  cInfix " <= "
                   Pigt ->  cInfix " > "
                   Pige ->  cInfix " >= "

                   Pineg -> cPrefix " -"

                   Pimin -> cBinFun "imin"
                   Pimax -> cBinFun "imax"

                   PintToBool ->
                       "stgCurVal = " ++
                       arg0 ++ "?" ++ getEnvRef "true"  env ++
                               ":" ++ getEnvRef "false" env ++ ";\n"

        cPrefix op =  "stgCurVal.argType = INT;\n" ++
                     "stgCurVal.i = " ++ op ++ arg0 ++ ";\n"

        cInfix op =  "stgCurVal.argType = INT;\n" ++
                     "stgCurVal.i = " ++ arg0 ++ op ++ arg1 ++ ";\n"

        cBinFun fun = "stgCurVal.argType = INT;\n" ++
                      "stgCurVal.i = " ++ fun ++ "(" ++ arg0 ++ ", " ++ arg1 ++ ");\n"


    in return (inline, [])


cge env (ELet it os e) =
    let names = map oname os
--        offsets = scanr (flip (-)) 0 sizes
        env'  = (reverse $ zip names (map HO sizes)) ++ env
        (sizes, decls, buildcodes) = unzip3 $ map (buildHeapObj env') os
    in do
      ofunc <- cgos env' os
      (einline, efunc) <- cge env' e
      return (concat decls ++ concat buildcodes ++ einline,
              ofunc ++ efunc)

cge env (ECase _ e a@(Alts italts alts aname)) = 
    do (ecode, efunc) <- cge env e
       (acode, afunc) <- cgalts env a
       let pre = "stgPushCont( (Cont)\n" ++
                 "  { .retAddr = &" ++ aname ++ ",\n" ++
                 "    .objType = CASECONT,\n" ++
                 "    .ident = \"CCont for " ++ aname ++ "\",\n" ++
                 (if fvs italts == [] then
                    "    // no FVs\n"
                  else
                    "    // load payload with FVs " ++ 
                            intercalate " " (fvs italts) ++ "\n") ++
                         indent 4 (loadPayloadFVs env (fvs italts)) ++
                 "  });\n"
       return (pre ++ ecode ++ acode, efunc ++ afunc)

-- CG Alts ************************************************************
-- DEFUN0(alts1) {
--   Cont cont = stgPopCont();
--   PtrOrLiteral scrutinee = stgCurVal;
--   switch(scrutinee.op->infoPtr->conFields.tag) {
--   case TagLeft:
--     // variable saved in casecont
--     stgCurVal = cont.payload[0];
--     STGRETURN0();
--   case TagRight:
--     // from constructor
--     stgCurVal = scrutinee.op->payload[0];
--     STGRETURN0();
--   default:
--     stgCurVal = scrutinee;
--     STGRETURN0();
--   }
--   ENDFUN;
-- }

-- ADef only or unary sum => no C switch
cgalts env (Alts it alts name) = 
    let contName = "ccont_" ++ name
        scrutName = "scrut_" ++ name
        altenv = zip (fvs it) [ CC contName i | i <- [0..] ]
        env' = altenv ++ env
        forward = "FnPtr " ++ name ++ "();"
    in do
      let switch = length alts > 1
      codefuncs <- mapM (cgalt env' switch scrutName) alts
      let (codes, funcss) = unzip codefuncs
      let fun = "DEFUN0("++ name ++ ") {\n" ++
                "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
                -- tricky:  scrutinee might not be evaluated
                "  STGEVAL(stgCurVal);\n" ++
                -- actually need the ccont?
                -- any fvs in the expressions on the rhs's?
                (if (length $ nub $ concatMap (fvs . emd . ae) alts) > 0 then 
                     "  Cont " ++ contName ++ " = "
                 else "  ") ++                      "stgPopCont();\n" ++
                "  PtrOrLiteral " ++ scrutName ++ " = stgCurVal;\n" ++
                (if switch then
                   "  switch(stgCurVal.op->infoPtr->conFields.tag) {\n" ++
                        indent 4 (concat codes) ++
                   "  }\n" 
                 else indent 2 $ concat codes) ++
                "  ENDFUN;\n" ++
                "}\n"
      return ("", (forward, fun) : concat funcss)

cgalt env switch scrutName (ACon it c vs e) =
    let eenv = zip vs (map (AC $ scrutName ++ ".op") [0..])
        env' = eenv ++ env
    in do
      (inline, func) <- cge env' e
      let (arity, tag) = case Map.lookup c (conMap it) of
                           Nothing -> error "conMap lookup error"
                           Just x -> x
      let code = "// " ++ c ++ " " ++ intercalate " " vs ++ " ->\n" ++
                 if switch then
                     "case " ++ show tag ++ ": {\n" ++
                        indent 2 inline ++
                     "  STGRETURN0();\n" ++
                     "}\n"
                 else inline ++ "STGRETURN0();\n"
      return (code, func)
              
cgalt env switch scrutName (ADef it v e) =
    let env' = (v, AD scrutName) : env
    in do
      (inline, func) <- cge env' e
      let code = "// " ++ v ++ " ->\n" ++
                 if switch then
                     "default: {\n" ++
                        indent 2 inline ++
                     "  STGRETURN0();\n" ++
                     "}\n"
                 else inline ++ "STGRETURN0();\n"
      return (code, func)


-- ****************************************************************
-- build heap object (bho) is only invoked by 'let' so TLDs not built

--   // y = THUNK( x );
--   Obj *y = stgNewHeapObj();
--   *y = (Obj) 
--     { .objType = THUNK,
--       .infoPtr = &it_y,
--       .payload[0] = (PtrOrLiteral) { .argType = HEAPOBJ, 
--                                      .op = STGHEAPAT(-2) }
--     };
-- return (size, inline code)


--  UPDCONT, 
--  CASECONT, 
--  CALLCONT, 
--  FUNCONT,

buildHeapObj env o =
    let (size, rval) = heapObjRVal env o
        name = oname o
        decl = "Obj *" ++ name ++ " = stgNewHeapObj();\n"
        code = "*" ++ name ++ " = " ++ rval  ++ ";\n"
    in (size, decl, code)

heapObjRVal env o =
    let (size, guts) = bho env o
        name = oname o
        code =
            "(Obj) \n" ++
            "      { .objType = " ++ showObjType (omd o) ++ ",\n" ++
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
        code = concat [".payload[" ++ show i ++ "] = " ++ 
                       cga env a ++ ", // " ++ showa a ++ "\n"
                       | (i,a) <- indexFrom 0 as ]
    in (size, code)

bho env (THUNK it e name) =
    (1, loadPayloadFVs env (fvs it))

bho env (BLACKHOLE it name) = (1,"")


loadPayloadFVs env fvs =
    concat [".payload[" ++ show i ++ "] = " ++ 
            cgv env v ++ ", // " ++ v ++ "\n"
            | (i,v) <- indexFrom 0 fvs ]

-- load atoms into payload starting at index ind
loadPayloadAtoms env as ind =
    concat [".payload[" ++ show i ++ "] = " ++ 
            cga env a ++ ", //" ++ showa a ++ "\n"
            | (i,a) <- indexFrom ind as]

showas as = intercalate " " $ map showa as

showa (Var v) = v
showa (Lit i) = show i

indexFrom i xs = zip [i..] xs
