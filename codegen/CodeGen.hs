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

Formal parameter or local variable from cast cont:  by name, e.g. "x"

Alt constructor var:  "stgCurVal.op->payload[i], bind these"

Alt default var:  "stgCurVal, bind it"

-}

{-# LANGUAGE NamedFieldPuns    #-}

module CodeGen(
  cgObjs,
  cgStart,
  cgMain
) where

import ADT
import AST
import CMap
import InfoTab
import HeapObj
import State
import Analysis
import Util
import PPrint

import Prelude
import Data.List(intercalate,nub)

import Data.Map (Map)
import qualified Data.Map as Map

data RVal = SHO           -- static heap obj
          | HO Int        -- heap obj,  payload size
          | FP            -- formal param or local var, use name as is
          | FV Int        -- free var, self->payload[Int]
          | AC Var Int    -- alt con
          | AD Var        -- alt def
            deriving(Eq,Show)

type Env = [(String, RVal)]

getEnvRef :: String -> Env -> String
getEnvRef v env = lu v env 0 0

-- first Int is total number of payload elements
-- second Int is total number of Objs 
lu :: String -> Env -> Int -> Int -> String
lu v [] _ _ = error $ "lu " ++ v ++ " failed"

lu v ((v',k):_) size' n | v == v' =
    case k of
      SHO     -> "HOTOPL(&sho_" ++ v ++ ")"
      HO size -> "HOTOPL((Obj *)STGHEAPAT(" ++ show (size+size') ++ "," ++ show (n+1) ++ "))"
      FP      -> v
      FV i    -> "self.op->payload[" ++ show i ++ "]"
      AC v i  -> v ++ "->payload[" ++ show i ++ "]"
      AD v    -> v

lu v ((_, HO size) : xs) size' n =
    lu v xs (size'+size) (n+1)

lu v (x : xs) size n = lu v xs size n

-- boxed expression predicate
{-
isBoxede e = case typ $ emd e of MCon False _ _ -> False
                                 MPrim _        -> False
                                 _              -> True
-}
isBoxede e = isBoxed $ typ $ emd e

-- CG Atom, Var ************************************************************
-- there's a lacking distinction between simply obtaining a reference to an
-- atom or variable and forcing evaluation of it (with STGEVAL or
-- STGAPPLY)

cgUBa env (Var v)  t   =  "(" ++ cgv env v ++ ")." ++ t
cgUBa env (LitI i) "i" = show i
cgUBa env (LitD d) "d" = show d
cgUBa _ at _ = error $ "CodeGen.cgUBa: not expecting Atom - " ++ show at 
-- cgUBa env (LitF f) "f" = show f

cgv env v = getEnvRef v env -- ++ "/* " ++ v ++ " */"

cga :: Env -> Atom -> String
cga env (LitI i) = "((PtrOrLiteral){.argType = INT,    .i = " ++ show i ++ " })"
cga env (LitD d) = "((PtrOrLiteral){.argType = DOUBLE, .d = " ++ show d ++ " })"
--cga env (LitF f) = "((PtrOrLiteral){.argType = FLOAT,  .f = " ++ show f ++ " })"
cga env (Var v) = cgv env v
cga _ at = error $ "CodeGen.cga: not expecting Atom - " ++ show at 

-- CG in the state monad ***************************************************
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
        (forward, fundef) = registerSHOs objs
    in (forward:forwards, fundef:fundefs)
    
cgStart :: String
cgStart = "\n\nDEFUN0(start) {\n" ++
            "  registerSHOs();\n" ++
            "  Obj *showResultCont = stgAllocCallCont2(&it_stgShowResultCont, 0);\n" ++
            "  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));\n" ++
            "  STGRETURN0();\n" ++
            "  ENDFUN;\n" ++
            "}\n\n"  
            
cgMain :: Bool -> String 
cgMain v = let top = "int main (int argc, char **argv) {\n" ++
                     "  initStg();\n" ++
                     "  initCmm();\n" ++
                     "  initGc();\n" ++
                     "  CALL0_0(start);\n"
               bot = "  return 0;\n" ++ "}\n\n"
  in if v then top ++ "  showStgHeap();\n  GC();\n" ++ bot else top ++ bot            

registerSHOs objs = 
    ("void registerSHOs();",
     "void registerSHOs() {\n" ++
        concat [ "  stgStatObj[stgStatObjCount++] = &" ++ s ++ ";\n" 
                 | s <- shoNames objs ] ++
     "}\n")

-- return [(forward,fundef)], will be unzipped at top level

cgos :: Env -> [Obj InfoTab] -> State Int [(String, String)]
cgos env = concatMapM (cgo env)


-- duplicated from HMSTG.hs
-- m is rightmost element, i.e. result type
unfoldr (MFun m1 m2) = let (m,ms) = unfoldr m2 in (m, m1:ms)
unfoldr m = (m,[])

-- given function type and formal parameter list, return the args
-- and types sorted pointer first
permArgs vs ft =
    let (rt, ats) = unfoldr ft
    in part vs ats
        where
          part [] [] = (([],[]),([],[]))
          part (v:vs) (t:ts) =
              let ((bvs,uvs),(bts,uts)) = part vs ts
              in case isBoxed t of
                   True  -> ((v:bvs,uvs),(t:bts,uts))
                   False -> ((bvs,v:uvs),(bts,t:uts))
          part [] (t:ts) = 
                let (([],[]),(bts,uts)) = part [] ts 
                in case isBoxed t of
                   True  -> (([],[]),(t:bts,uts))
                   False -> (([],[]),(bts,t:uts))  
          part _ _ = error ("permArgs fallthrough  vs=" ++ show vs ++ " ft=" ++ show ft)

cgo :: Env -> Obj InfoTab -> State Int [(String, String)]
cgo env o@(FUN it vs e name) =
    do 
      let env' = zip (map fst $ fvs it) (map FV [0..]) ++ 
                 zip vs (repeat FP) ++
                 env
          vts@((bvs,uvs),(bts,uts)) = permArgs vs $ typ it
      (inline, funcs) <- cge env' e
      let forward = "FnPtr fun_" ++ name ++ "();"
          func =
            "// " ++ show (ctyp it) ++ "\n" ++
            "// " ++ show vts ++ "\n" ++
            "DEFUN" ++ show (length vs + 1) ++ "(fun_" ++ 
            name ++ ", self, " ++
--            intercalate ", " vs ++
            intercalate ", " (bvs ++ uvs) ++
            ") {\n" ++
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
               indent 2 inline ++
            "  fprintf(stderr, \"" ++ name ++ " returning\\n\");\n" ++
            "  STGRETURN0();\n" ++  -- in case inline doesn't jump somewhere else
            "  ENDFUN;\n}"
      return $ (forward, func) : funcs

cgo env (PAP it f as name) =
    return []

cgo env (CON it c as name) =
    return []

cgo env o@(THUNK it e name) =
    do 
      let env' = zip (map fst $ fvs it) (map FV [0..]) ++ env
      (inline, funcs) <- cge env' e
      let forward = "FnPtr fun_" ++ name ++ "();"
      let func =
            "// " ++ show (ctyp it) ++ "\n" ++
            "DEFUN1(fun_" ++ name ++ ", self) {\n" ++
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
            "  stgThunk(self);\n" ++
            indent 2 inline ++
            "  fprintf(stderr, \"" ++ name ++ " returning\\n\");\n" ++
            "  STGRETURN0();\n" ++  -- in case inline doesn't jump somewhere else
            "  ENDFUN;\n}"
      return $ (forward, func) : funcs

cgo env (BLACKHOLE {}) =
    return []

-- ****************************************************************

stgApplyGeneric env f as = 
    "// " ++ f ++ " " ++ showas as ++ "\n" ++
    "STGAPPLY" ++ show (length as) ++ "(" ++
    intercalate ", " (cgv env f : map (cga env) as) ++ 
    ");\n"

{-
-- this is one place where strictness is variable
stgApplyGeneric2 env f as = 
    let pnstring = [ if isBoxed $ typ $ emd a then 'P' else 'N' | a <- as ]
    "// " ++ f ++ " " ++ showas as ++ "\n" ++
    
    "{ Obj *callCont = stgAllocCallCont2(&it_stgCallCont, " ++ (show $ length as) ++ ");\n" ++
       concat ["  callCont->payload[" ++ show i ++ "] = " ++ cga a ++ ";\n" 
               | (i,a) <- zip [1..] as] ++
    "  STGEVAL(" ++ cgv env f ++ ")"
    "STGAPPLY" ++ show (length as) ++ "(" ++
    intercalate ", " (cgv env f : map (cga env) as) ++ 
    ");\n"
-}

-- for now
stgApplyDirect env (EFCall it f as) = 
    "// " ++ f ++ " " ++ showas (map ea as) ++ "\n" ++
    "STGAPPLY" ++ show (length as) ++ "(" ++
    intercalate ", " (cgv env f : map ((cga env) . ea) as) ++ 
    ");\n"

stgApplyDirect env expr = 
    error $ "CodeGen.stgApplyDirect: not expecting Expr - " ++ show (pprint expr)


-- return (inline code, [(forward, fundef)])
cge :: Env -> Expr InfoTab -> State Int (String, [(String, String)])

cge env e@(EAtom it a) =
    let inline = "stgCurVal = " ++ cga env a ++ "; " ++ "// " ++ showa a ++ "\n" ++
                 (if isBoxede e then "STGEVAL(stgCurVal);\n" else "")
    in return (inline, [])
{-
cge env e@(EFCall it f eas) = 
    let as = map ea eas
        inline = 
            case (knownCall it) of 
              Nothing -> stgApplyGeneric env f as
              Just kit -> if arity kit == length as
                          -- then stgApplyDirect env e
                          then stgApplyGeneric env f as
                          else stgApplyGeneric env f as
    in return (inline, [])
-}

    

cge env e@(EFCall it f eas) = 
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        inline = 
            "// " ++ f ++ " " ++ showas as ++ "\n" ++
--            "STGAPPLY" ++ show (length as) ++ "(" ++
            "STGAPPLY" ++ pnstring ++ "(" ++
            intercalate ", " (cgv env f : map (cga env) as) ++ 
            ");\n"


    in return (inline, [])

cge env (EPrimop it op eas) =
    let as = map ea eas
        arg0 = cgUBa env (as !! 0) -- these take a type indicator
        arg1 = cgUBa env (as !! 1)
        inline = case op of
                   Piadd -> cInfixIII " + "
                   Pisub -> cInfixIII " - "
                   Pimul -> cInfixIII " * "
                   Pidiv -> cInfixIII " / "
                   Pimod -> cInfixIII " % "

                   Pieq ->  cInfixIII " == "
                   Pine ->  cInfixIII " != "
                   Pilt ->  cInfixIII " < "
                   Pile ->  cInfixIII " <= "
                   Pigt ->  cInfixIII " > "
                   Pige ->  cInfixIII " >= "

                   Pineg -> cPrefixII " -"

                   Pimin -> cFunIII "imin"
                   Pimax -> cFunIII "imax"

                   PintToBool ->
                       "stgCurVal = " ++
                       arg0 "i" ++ "?" ++ getEnvRef "true"  env ++
                                   ":" ++ getEnvRef "false" env ++ ";\n"

        cPrefixII op =  "stgCurVal.argType = INT;\n" ++
                        "stgCurVal.i = " ++ op ++ arg0 "i" ++ ";\n"

        cInfixIII op =  "stgCurVal.argType = INT;\n" ++
                        "stgCurVal.i = " ++ arg0 "i" ++ op ++ arg1 "i" ++ ";\n"

        cInfixIIB op =  "stgCurVal.argType = BOOL;\n" ++
                        "stgCurVal.i = " ++ arg0 "i" ++ op ++ arg1 "i" ++ ";\n"

        cFunIII fun = "stgCurVal.argType = INT;\n" ++
                      "stgCurVal.i = " ++ fun ++ "(" ++ arg0 "i" ++ ", " ++ arg1 "i" ++ ");\n"
    in return (inline, [])

cge env (ELet it os e) =
    let names = map oname os
        env'  = (reverse $ zip names (map HO sizes)) ++ env
        (sizes, decls, buildcodes) = unzip3 $ map (buildHeapObj env') os
    in do
      ofunc <- cgos env' os
      (einline, efunc) <- cge env' e
      return (concat decls ++ concat buildcodes ++ einline,
              ofunc ++ efunc)

cge env (ECase _ e a@(Alts italts alts aname)) = 
    do (ecode, efunc) <- cge env e
       (acode, afunc) <- cgalts env a (isBoxede e)
       let name = "ccont_" ++ aname
           pre = "Obj *" ++ name ++ 
              " = stgAllocCont( &it_" ++ aname ++ ");\n" ++
              (if fvs italts == [] then
                 "    // no FVs\n"
               else
                 "    // load payload with FVs " ++
                   intercalate " " (map fst $ fvs italts) ++ "\n") ++
                 indent 2 (loadPayloadFVs env (map fst $ fvs italts) name)
           scrut = if isBoxede e then
                       "  // boxed scrutinee\n" ++
                       "  STGEVAL(stgCurVal);\n"
                   else "  // unboxed scrutinee\n"
--       return (pre ++ ecode ++ scrut ++ acode, efunc ++ afunc)
       return (pre ++ ecode ++ acode, efunc ++ afunc)
          
-- ADef only or unary sum => no C switch
cgalts env (Alts it alts name) boxed = 
    let contName = "ccont_" ++ name
        scrutName = "scrut_" ++ name
        -- altenv = zip (fvs it) [ CC contName i | i <- [0..] ]
        altenv = zip (map fst $ fvs it) (repeat FP)
        env' = altenv ++ env
        forward = "FnPtr " ++ name ++ "();"
    in do
      let switch = length alts > 1
      codefuncs <- mapM (cgalt env' switch scrutName) alts
      let (codes, funcss) = unzip codefuncs
      let fun = "// " ++ show (ctyp it) ++ "\n" ++
                "DEFUN0("++ name ++ ") {\n" ++
                "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
                -- actually need the ccont?
                -- any fvs in the expressions on the rhs's?
                (if (length $ fvs it) > 0 then 
                     "  Obj *" ++ contName ++ " = stgPopCont();\n" ++
                     concat ["  PtrOrLiteral " ++ v ++ 
                             " = " ++ contName ++ "->payload[" ++ show i ++ "];\n"
                             | (i,v) <- indexFrom 0 $ map fst $ fvs it ] 
                 else 
                     "  stgPopCont();\n") ++
                "  PtrOrLiteral " ++ scrutName ++ " = stgCurVal;\n" ++
                (if switch then
                     (if boxed then
                          "  switch(stgCurVal.op->infoPtr->conFields.tag) {\n"
                      else 
                          "  switch(stgCurVal.i) {\n"
                     ) ++
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
      let tag = luConTag c $ cmap it -- ConTags returned as Strings!
      let code = "// " ++ c ++ " " ++ intercalate " " vs ++ " ->\n" ++
                 if switch then
                   "case " ++ tag ++ ": {\n" ++
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
-- buildHeapObj is only invoked by ELet so TLDs not built

--  UPDCONT, 
--  CASECONT, 
--  CALLCONT, 
--  FUNCONT,

buildHeapObj env o =
    let (size, rval) = bho env o
        name = oname o
        decl = "Obj *" ++ name ++ " = stgNewHeapObj( &it_" ++ name ++ " );\n" 
    in (size, decl, rval)

bho :: [(String, RVal)] -> Obj InfoTab -> (Int, [Char])
bho env (FUN it vs e name) = 
    (length $ fvs it, loadPayloadFVs env (map fst $ fvs it) name)

-- TODO: the size here should be based on the FUN rather than being maxPayload
bho env (PAP it f as name) =
    (maxPayload, loadPayloadFVs env (map fst $ fvs it) name ++ 
                 loadPayloadAtoms env (projectAtoms as) (length $ fvs it) name)

-- CON is special in that the payload contains not just FVs but literals
-- as well, and we need their types.  Three ways this could be done:
-- 0.  The right way would be to have Atom be typed (major TODO)
-- 1.  Use HMStg.luTCon using cmap it, typ it, and c, subsequent gyrations
-- 2.  Use fvs type information
bho env (CON it c as name) = 
    let ps = [name ++ "->payload[" ++ show i ++ "] = " ++ 
                       cga env a ++ "; // " ++ showa a ++ "\n"
                       | (i,a) <- indexFrom 0 (projectAtoms as) ]
    in (length ps, concat ps)

bho env (THUNK it e name) =
    (max 1 (length $ fvs it), loadPayloadFVs env (map fst $ fvs it) name)
    
bho env (BLACKHOLE it name) = (1,"")

loadPayloadFVs env fvs name =
    concat [name ++ "->payload[" ++ show i ++ "] = " ++ 
            cgv env v ++ "; // " ++ v ++ "\n"
            | (i,v) <- indexFrom 0 $ fvs ]

-- load atoms into payload starting at index ind
loadPayloadAtoms env as ind name =
    concat [name ++ "->payload[" ++ show i ++ "] = " ++ 
            cga env a ++ "; //" ++ showa a ++ "\n"
            | (i,a) <- indexFrom ind as]

showas as = intercalate " " $ map showa as

showa (Var v) = v
showa (LitI i) = show i
-- showa (LitF f) = show f
showa (LitD d) = show d
showa at = error $ "CodeGen.showa: not expecting Atom - " ++ show at 

indexFrom :: Int -> [a] -> [(Int, a)]
indexFrom i xs = zip [i..] xs
