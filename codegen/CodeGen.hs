{-

  We need the free variables of subtrees for two reasons:
  * for constructing an object (closure), to know what to put it its payload
  * and to know what needs to be stashed in a continuation

  Conversely, when generating code for an expression, we need to know where
  to find each variable.  This is done by maintaining a stack of different
  kinds of `exposures' of variable values:

  * SOs - just one set of these at the bottom of the stack
  * HOs, following a "let"
  * Formal function parameters, these are named parameters (alternatively,
    could not name and use a CallCont like stgApply--TODO:  think about this)
  * Popped CaseCont vars
  * Alt constructor vars or default var (TODO:  these could be formal function
    params if individual alts are separate functions)

In more detail:

SO:  referenced with absolute memory address, e.g. "sho_unit"

HO:  address calculated from TOH pointer, e.g. "((Obj *)TOH_ptr)[-3]"

Formal parameter or local variable from case cont:  by name, e.g. "x"

Alt constructor var:  "stgCurVal.op->payload[i], bind these"

Alt default var:  "stgCurVal, bind it"

-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE CPP               #-}

#include "../options.h"

module CodeGen(
  cgObjs,
  cgStart,
  cgMain,
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
import STGbits
import Options

import Prelude
import Data.List(intercalate,nub)

import Data.Map (Map)
import qualified Data.Map as Map

import Language.C.Quote.GCC
import Language.C.Syntax (Definition, Initializer, Func)
import qualified Text.PrettyPrint.Mainland as PP

pp f = PP.pretty 80 $ PP.ppr f

data RVal = SO              -- static object
          | HO String       -- Heap Obj, payload size, TO GO?
          | FP String Int   -- stack Formal Param, pointer to stack payload
          | FV String Int   -- Free Variable, payload in heap via pointer to
                            --   pointer in stack, e.g. fvpp->op->payload[i]
-- because we don't have fresh names for the Let Blocks we use existing
-- names for let-bound variables for now.  We could have done the same
-- dereference scheme for FP above
            deriving(Eq,Show)

type Env = [(String, RVal)]

optStr b s = if b then s else ""
iff b x y = if b then x else y

cStart :: Func
cStart = [cfun|
           typename FnPtr start()
           {
             typename Cont *showResultCont = stgAllocCallOrStackCont(&it_stgShowResultCont, 0);
#if USE_ARGTPYE
             stgCurVal.argType = HEAPOBJ;
#endif
             stgCurVal.op = &sho_main;
             STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);
           }
         |]

cMain :: Bool -> Func
cMain v =    
  let top = [citems|
              startCheck();
              parseArgs(argc, argv);
              initStg();
              initGc();
              registerSOs();
              CALL0_0(start);
            |]
      extra = [citems|
                 showStgHeap(LOG_DEBUG); 
                 GC();
              |]
   in if v then [cfun|
                  int main (int argc, char **argv)
                  {
                    $items:top
                    $items:extra
                    return 0;
                  }
                |]
      else [cfun|
              int main (int argc, char **argv)
              {
                $items:top
                return 0;
               }
            |]

cregisterSOs :: [Obj InfoTab] -> (Definition, Func)
cregisterSOs objs = 
  let proto = [cedecl| void registerSOs(); |]
      items = [ [citem|stgStatObj[stgStatObjCount++] = &$id:s; |] | s <- shoNames objs]
      fun = [cfun| void registerSOs() { $items:items } |]
  in (proto, fun)

cgStart :: String
cgStart = "\n\nFnPtr start() {\n" ++
            "  Cont *showResultCont = " ++
            "  stgAllocCallOrStackCont(&it_stgShowResultCont, 0);\n" ++
            (if useArgType then "  stgCurVal.argType = HEAPOBJ;\n" else "") ++
            "  stgCurVal.op = &sho_main;\n" ++
            "  STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);\n" ++
            "}\n\n"

cgMain :: Bool -> String
cgMain v = let top = "int main (int argc, char **argv) {\n" ++
                     "  startCheck();\n" ++
                     "  parseArgs(argc, argv);\n" ++
                     "  initStg();\n" ++
                     "  initGc();\n" ++
                     "  registerSOs();\n" ++
                     "  CALL0_0(start);\n"
               bot = "  return 0;\n" ++ "}\n\n"
  in if v then top ++ "  showStgHeap(LOG_DEBUG);\n  GC();\n" ++ bot else top ++ bot


registerSOs :: [Obj InfoTab] -> (String, String)
registerSOs objs =
    ("void registerSOs();",
     "void registerSOs() {\n" ++
        concat [ "  stgStatObj[stgStatObjCount++] = &" ++ s ++ ";\n"
                 | s <- shoNames objs ] ++
     "}\n")

listLookup k [] = Nothing
listLookup k ((k',v):xs) | k == k' = Just v
                         | otherwise = listLookup k xs

getEnvRef :: String -> Env -> String
getEnvRef v kvs =
    case listLookup v kvs of
      Nothing -> error $ "getEnvRef " ++ v ++ " failed"
      Just k ->
          case k of
            SO      -> "HOTOPL(&sho_" ++ v ++ ")"
            HO name -> "(*" ++ name ++ ")" -- pointer to STACKCONT payload
            FP fp i -> fp ++ "[" ++ show i ++ "]"
            FV fpp i -> fpp ++ "->op->payload[" ++ show i ++ "]"

argTypeElem :: String -> String
argTypeElem ty = if useArgType then ".argType = " ++ ty ++ "," else ""

cga :: Env -> Atom -> String
cga env (Var v) = cgv env v
cga env (LitI i) = "((PtrOrLiteral){" ++ argTypeElem "INT" ++
                   " .i = " ++ show i ++ " })"
cga env (LitL l) = "((PtrOrLiteral){" ++ argTypeElem "LONG" ++
                   " .l = " ++ show l ++ " })"
cga env (LitF f) = "((PtrOrLiteral){" ++ argTypeElem "FLOAT" ++
                   " .f = " ++ show f ++ " })"
cga env (LitD d) = "((PtrOrLiteral){" ++ argTypeElem "DOUBLE" ++
                   " .d = " ++ show d ++ " })"
cga env (LitC c) = "((PtrOrLiteral){" ++ argTypeElem "INT" ++
                   " .i = con_" ++ c ++ " })"

-- boxed expression predicate
isBoxede e = isBoxed $ typ $ emd e


cgUBa env (Var v)  t   =  "(" ++ cgv env v ++ ")." ++ t
cgUBa env (LitI i) "i" = show i
cgUBa env (LitD d) "d" = show d
cgUBa _ at _ = error $ "CodeGen.cgUBa: not expecting Atom - " ++ show at
-- cgUBa env (LitF f) "f" = show f

cgv env v = getEnvRef v env ++ "/* " ++ v ++ " */"


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
          part x y = error "CodeGen.part length mismatch"


-- CG in the state monad ***************************************************
-- CG of objects produces no inline code
-- THUNK produces a function, other OBJ types have single global function
--    (ignore explicit PAP for now)
-- FUN produces a function (not object)
--   all objects produce a (S)HO
-- for CG, objects are heap allocated only by let

data YPN = Yes | Possible | No -- could use Maybe Bool but seems obscure
           deriving(Eq, Show)

--entry point, not recursive
cgObjs :: [Obj InfoTab] -> [String] -> ([String],[String])
cgObjs objs runtimeGlobals =
    let tlnames = runtimeGlobals ++ map (name . omd) objs
        env = zip tlnames $ repeat SO
        (funcs, _) = runState (cgos env objs) 0
        (forwards, fundefs) = unzip funcs
        (forward, fundef) = registerSOs objs
    in (forward:forwards, fundef:fundefs)

cgos :: Env -> [Obj InfoTab] -> State Int [(String,  String)]
--                                        [(forward, func)]
cgos env = concatMapM (cgo env)

cgo :: Env -> Obj InfoTab -> State Int [(String, String)]
cgo env o@(FUN it vs e name) =
    let forward = "FnPtr fun_" ++ name ++ "();"
        argp = "argp" -- also free variable pointer pointer
        fps = "self":vs
        env' = zip (map fst $ fvs it) (map (FV argp) [0..]) ++
               zip fps (map (FP argp) [0..]) ++
               env
    in do
      ((inline, ypn), funcs) <- cge env' e
      let func =
            "// " ++ show (ctyp it) ++ "\n" ++
            "// " ++ name ++ "(self, " ++ intercalate ", " vs ++ ")\n" ++
            "FnPtr fun_" ++ name ++ "() {\n" ++
            "  LOG(LOG_INFO, \"" ++ name ++ " here\\n\");\n" ++
            "  PtrOrLiteral *" ++ argp ++
                 " = &(stgGetStackArgp()->payload[0]);\n" ++
               indent 2 inline ++
               optStr (ypn /= Yes) "  STGRETURN0();\n" ++
            "}\n"
      return $ (forward, func) : funcs

cgo env (PAP it f as name) =
    return []

cgo env (CON it c as name) =
    return []

cgo env o@(THUNK it e name) =
    let
      fvpp = "fvpp"
      env' = zip (map fst $ fvs it) (map (FV fvpp) [1..]) ++ env
      forward = "FnPtr thunk_" ++ name ++ "();"
    in do
      ((inline,ypn), funcs) <- cge env' e
      let func =
            "// " ++ show (ctyp it) ++ "\n" ++
            "FnPtr thunk_" ++ name ++ "() {\n" ++
            "  LOG(LOG_INFO, \"" ++ name ++ " here\\n\");\n" ++
            "  // access free vars through frame pointer for GC safety\n" ++
            "  // is this really necessary???\n" ++
            "  Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);\n" ++
            "  stg_fp->layout = " ++ npStrToBMStr "P" ++ ";\n" ++
            "  stg_fp->payload[0] = stgCurVal;\n" ++
            "  PtrOrLiteral *" ++ fvpp ++ " = &(stg_fp->payload[0]);\n" ++
            "  stgThunk(stgCurVal);\n" ++
            -- expression code doesn't take stgCurVal as arg
            "  stgCurVal.op = NULL;\n" ++
            indent 2 inline ++
              optStr (ypn /= Yes) "  STGRETURN0();\n" ++
            "}\n"
      return $ (forward, func) : funcs

cgo env (BLACKHOLE {}) =
    return []

-- ****************************************************************

stgApplyGeneric env f eas direct =
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        -- HACK
        f' = if f == "stg_case_not_exhaustive" then
                 f ++ pnstring
             else f
        inline =
            -- new STACKFRAME
            "{ Cont *cp = stgAllocCallOrStackCont( &it_stgStackCont, " ++
                 show (length pnstring + 2) ++ ");\n" ++
            "  cp->layout = " ++ npStrToBMStr ('N' : 'P' : pnstring ) ++ ";\n" ++
            "  cp->payload[ 0 ] = " ++ cga [] (LitI 0) ++ ";\n" ++
            "  cp->payload[ 1 ] = " ++ cgv env f' ++ ";\n" ++
            concat ["  cp->payload[ " ++ show i ++ " ] = " ++ cga env a ++ ";\n"
                    | (i,a) <- zip [2..] as ] ++

            (if direct then
                "  // DIRECT TAIL CALL " ++ f ++ " " ++ showas as ++ "\n" ++
--              "  STGJUMP0(getInfoPtr(cp->payload[0].op)->funFields.trueEntryCode);\n"
                -- really direct
                "  if (evalStrategy == STRICT1) stgEvalStackFrameArgs(cp);\n" ++
                "  STGJUMP0(fun_" ++ f ++ ");\n"
            else
                "  // INDIRECT TAIL CALL " ++ f' ++ " " ++ showas as ++ "\n" ++
                "  STGJUMP0(stgApplyNew);\n") ++
            "}\n"
    in return ((inline, Yes), [])

stgCurValUArgType :: String -> String
stgCurValUArgType ty = if useArgType
                       then "stgCurValU.argType = " ++ ty ++ ";\n"
                       else ""

-- return (inline code, [(forward, fundef)])
cge :: Env
    -> Expr InfoTab
    -> State Int ((String,YPN), [(String, String)])
-- State Int ((inline code, inline code ends in JUMP/RETURN),
--            [(forward, function)])

cge env e@(EAtom it a) =
    let inline =
            if isBoxede e then
                "stgCurVal = " ++ cga env a ++ "; // " ++ showa a ++ "\n" ++
                "// boxed EAtom, stgCurVal updates itself \n" ++
                "STGJUMP();\n"
            else
                "stgCurValU = " ++ cga env a ++ "; // " ++ showa a ++ "\n" ++
                "// unboxed EAtom\n"
    in return ((inline, if isBoxede e then Yes else No), [])

cge env e@(EFCall it f eas) =
    case (knownCall it) of
      Nothing -> stgApplyGeneric env f eas False
      Just kit -> if arity kit == length eas
                  then stgApplyGeneric env f eas False
                  else stgApplyGeneric env f eas False

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
                   _ -> error "Eprimop"

        cPrefixII op = stgCurValUArgType "INT" ++
                       "stgCurValU.i = " ++ op ++ arg0 "i" ++ ";\n"

        cInfixIII op = stgCurValUArgType "INT" ++
                       "stgCurValU.i = " ++ arg0 "i" ++ op ++ arg1 "i" ++ ";\n"

        cInfixIIB op = stgCurValUArgType "BOOL" ++
                       "stgCurValU.i = " ++ arg0 "i" ++ op ++ arg1 "i" ++ ";\n"

        cFunIII fun = stgCurValUArgType "INT" ++
                      "stgCurValU.i = " ++ fun ++ "(" ++ arg0 "i" ++ ", " ++ arg1 "i" ++ ");\n"
    in return ((inline, No), [])

-- Allocating all objs in ELet before making their payloads valid is somewhat problematic
-- if GC can be initiated by a single object allocation.  This is exacerbated by strict
-- constructors.  This is not readily fixed by changing the order of things here because
-- of mutually recursive definitions.  
-- TODO:
--   Make runtime more robust
--   then for strict constructors evaluate the args
cge env (ELet it os e) =
    let names = map oname os
        decl =
            concat [ "PtrOrLiteral *" ++ name ++ ";\n" | name <- names ] ++
            "{Cont *contp = stgAllocCallOrStackCont(&it_stgLetCont, " ++
                               show (length os) ++ ");\n" ++
            concat [ name ++ " = &(contp->payload[" ++ show i ++ "]);\n" |
                     (name, i) <- zip names [0..] ] ++
            "contp->layout = " ++ npStrToBMStr (replicate (length os) 'P') ++ ";}\n"
        env'  = zip names (map HO names) ++ env
        (decls, buildcodes) = unzip $ map (buildHeapObj env') os
--        declbuildcodes = map (buildHeapObj env') os

    in do
      ofunc <- cgos env' os
      ((einline, ypn), efunc) <- cge env' e
      return ((decl ++ concat decls ++ concat buildcodes ++ einline, ypn),
--      return ((decl ++ concat declbuildcodes ++ einline, ypn),
              ofunc ++ efunc)

cge env ecase@(ECase _ e a) =
    do ((ecode, eypn), efunc) <- cge env e
       -- weird:  compiler requires parens around if, ghci does not
       (if eypn == No then
            cgeInline env (isBoxede e) (ecode, efunc) a
        else
            cgeNoInline env (isBoxede e) (ecode, efunc) a)

-- TODO:  inline
-- TODO:  YPN from Alts, Alt

cgeInline
  :: [(Var, RVal)]
     -> Bool
     -> ([Char], [([Char], [Char])])
     -> Alts InfoTab
     -> State Int (([Char], YPN), [([Char], [Char])])

cgeInline env boxed (ecode, efunc) a@(Alts italts alts aname) =
-- TODO:  efunc should be empty
    let contName = "ccont_" ++ aname
        pre = "// inline:  scrutinee does not STGJUMP or STGRETURN\n"
    in do
      ((acode, ypn), afunc) <- cgaltsInline env a boxed
      return ((pre ++ ecode ++ acode, ypn),
              efunc ++ afunc)

cgaltsInline
  :: [(Var, RVal)]
     -> Alts InfoTab
     -> Bool
     -> State Int (([Char], YPN), [([Char], [Char])])

cgaltsInline env a@(Alts it alts name) boxed =
    let contName = "ccont_" ++ name
        scrutPtr = "scrutPtr_" ++ name
        phonyforward = "FnPtr " ++ name ++ "();"
        phonyfun = "FnPtr "++ name ++ "() {}\n"
        switch = length alts > 1
     in do
       codefuncs <- mapM (cgalt env switch scrutPtr) alts
       let (codeypns, funcss) = unzip codefuncs
       let (codes, ypns) = unzip codeypns
       let myypn = if all (==Yes) ypns then Yes
                   else if all (==No) ypns then No
                        else Possible
       let inl = "Cont *" ++ contName ++
                 " = stgAllocCallOrStackCont(&it_stgStackCont, 1);\n" ++
                 "// " ++ show (ctyp it) ++ "\n" ++
                 contName ++ "->layout = " ++
                    npStrToBMStr (iff boxed "P" "N") ++ ";\n" ++
                 contName ++ "->payload[0] = " ++
                    (iff boxed "stgCurVal" "stgCurValU") ++ ";\n" ++
                 optStr boxed "stgCurVal.op = NULL;\n" ++
                 "PtrOrLiteral *" ++ scrutPtr ++
                     " = &(" ++ contName ++ "->payload[0]);\n" ++
                 (if switch then
                    (if boxed then
                       "switch(getInfoPtr(" ++ scrutPtr ++ "[0].op)->conFields.tag) {\n"
                     else
                       "switch(stgCurValU.i) {\n"
                    ) ++
                      indent 2 (concat codes) ++
                    "}\n"
                  else concat codes)
       return ((inl, myypn), (phonyforward, phonyfun) : concat funcss)

payloadArgType :: String -> String -> String
payloadArgType name ty = if useArgType
                         then name ++ "->payload[0].argType = " ++ ty ++ ";\n"
                         else ""

cgeNoInline
  :: [(Var, RVal)]
     -> Bool
     -> ([Char], [([Char], [Char])])
     -> Alts InfoTab
     -> State Int (([Char], YPN), [([Char], [Char])])

cgeNoInline env boxed (ecode, efunc) a@(Alts italts alts aname) =
    let contName = "ccont_" ++ aname
        pre = "// scrutinee may STGJUMP or STGRETURN\n" ++
              "Cont *" ++ contName ++ " = stgAllocCont( &it_" ++ aname ++ ");\n" ++
              "// dummy value for scrutinee, InfoTab initializes to unboxed\n" ++
              contName ++ "->payload[0].i = 0;\n" ++
              payloadArgType contName "INT" ++
              (if fvs italts == [] then
                 "// no FVs\n"
               else
                 "// load payload with FVs " ++
                         intercalate " " (map fst $ fvs italts) ++ "\n") ++
                 (loadPayloadFVs env (map fst $ fvs italts) 1 contName)
    in do (acode, afunc) <- cgalts env a boxed
--        need YPN results from Alts
          return ((pre ++ ecode ++ acode, Possible),
                  efunc ++ afunc)
cgalts
  :: [(Var, RVal)]
     -> Alts InfoTab
     -> Bool
     -> State Int ([Char], [([Char], [Char])])

-- ADef only or unary sum => no C switch
cgalts env (Alts it alts name) boxed =
    let contName = "ccont_" ++ name
        fvp = "fvp"
        -- case scrutinee is not current explicitly bound to variable
        altenv = zip (map fst $ fvs it) (map (FP fvp) [1..])
        env' = altenv ++ env
        forward = "FnPtr " ++ name ++ "();"
        switch = length alts > 1
    in do
      codefuncs <- mapM (cgalt env' switch fvp) alts
      let (codeypns, funcss) = unzip codefuncs
      let (codes, ypns) = unzip codeypns
      let body =
              "LOG(LOG_INFO, \"" ++ name ++ " here\\n\");\n" ++
              "Cont *" ++ contName ++ " = stgGetStackArgp();\n" ++
              "// make self-popping\n" ++
              "stgCaseToPopMe(" ++ contName ++ ");\n" ++
              "PtrOrLiteral *" ++ fvp ++
                  " = &(" ++ contName ++ "->payload[0]);\n" ++
--              concat ["  PtrOrLiteral " ++ v ++
--                      " = " ++ contName ++ "->payload[" ++ show i ++ "];\n"
--                      | (i,v) <- indexFrom 0 $ map fst $ fvs it ] ++
              (if boxed then
                   fvp ++ "[0] = stgCurVal;\n"
               else
                   fvp ++ "[0] = stgCurValU;\n") ++

              optStr boxed (contName ++ "->layout.bitmap.mask |= 0x1;\n") ++
              (if switch then
                 (if boxed then
--                    "switch(getInfoPtr(stgCurVal.op)->conFields.tag) {\n"
                    "stgCurVal.op = NULL;\n" ++
                    "switch(getInfoPtr(" ++ fvp ++ "[0].op)->conFields.tag) {\n"
                  else
                    "switch(stgCurValU.i) {\n"
                 ) ++
                   indent 2 (concat codes) ++
                 "}\n"
               else concat codes)
      let fun =
              "// " ++ show (ctyp it) ++ "\n" ++
              "FnPtr "++ name ++ "() {\n" ++ indent 2 body ++ "}\n"

      return ("", (forward, fun) : concat funcss)

cgalt
  :: [(Var, RVal)]
     -> Bool
     -> String
     -> Alt InfoTab
     -> State Int (([Char], YPN), [(String, String)])

cgalt env switch fvp (ACon it c vs e) =
    let DataCon c' ms = luDCon c (cmap it)
        (_,_,perm) = partPerm isBoxed ms
        eenv = zzip vs (map (FV fvp) perm)
        env' = eenv ++ env
    in do
      ((inline, ypn), func) <- cge env' e
      let tag = luConTag c $ cmap it -- ConTags returned as Strings!
      let code = "// " ++ c ++ " " ++ intercalate " " vs ++ " ->\n" ++
                 if switch then
                   "case " ++ tag ++ ": {\n" ++
                     indent 2 inline ++
                     (optStr (ypn /= Yes) "  STGRETURN0();\n") ++
                   "}\n"
                 else inline ++ optStr (ypn /= Yes) "STGRETURN0();\n"
      return ((code, Yes), func)

cgalt env switch fvp (ADef it v e) =
    let env' = (v, FP fvp 0) : env
    in do
      ((inline, ypn), func) <- cge env' e
      let code = "// " ++ v ++ " ->\n" ++
                 if switch then
                     "default: {\n" ++
                        indent 2 inline ++
                        optStr (ypn /= Yes) "  STGRETURN0();\n" ++
                     "}\n"
                 else inline ++ optStr (ypn /= Yes) "STGRETURN0();\n"
      return ((code, Yes), func)


-- ****************************************************************
-- buildHeapObj is only invoked by ELet so TLDs not built

buildHeapObj :: Env -> Obj InfoTab -> (String, String)
-- buildHeapObj :: Env -> Obj InfoTab -> String
buildHeapObj env o =
    let rval = bho env o
        name = oname o
        decl = name ++ "->op = stgNewHeapObj( &it_" ++ name ++ " );\n" ++
               optStr useArgType (name ++ "->argType = HEAPOBJ;\n")
    in (decl, rval)
--    in decl ++ rval

bho :: Env -> Obj InfoTab -> String
bho env (FUN it vs e name) =
    loadPayloadFVs env (map fst $ fvs it) 0 (name ++ "->op")


bho env (PAP it f as name) = error "unsupported explicit PAP"


-- CON is special in that the payload contains not just FVs but literals
-- as well, and we need their types.  This could be done:
-- 0.  The right way would be to have Atom be typed (major TODO)
-- 1.  Use HMStg.luTCon using cmap it, typ it, and c, subsequent gyrations
-- 2.  Use fvs type information
-- 3.  Embedding the Atoms into typed expressions
bho env (CON it c as name) =
    let ps = [name ++ "->op->payload[" ++ show i ++ "] = " ++
                       cga env a ++ "; // " ++ showa a ++ "\n"
                       | (i,a) <- indexFrom 0 (projectAtoms as) ]
    in concat ps

bho env (THUNK it e name) =
    let fv = fvs it
    in name ++ "->op->payload[0].op = NULL;\n" ++
       optStr useArgType (name ++ "->op->payload[0].argType = HEAPOBJ;\n") ++
       loadPayloadFVs env (map fst fv) 1 (name ++ "->op")

bho env (BLACKHOLE it name) = ""

loadPayloadFVs env fvs ind name =
    concat [name ++ "->payload[" ++ show i ++ "] = " ++
            cgv env v ++ "; // " ++ v ++ "\n"
            | (i,v) <- indexFrom ind $ fvs ]

-- load atoms into payload starting at index ind
loadPayloadAtoms env as ind name =
    concat [name ++ "->payload[" ++ show i ++ "] = " ++
            cga env a ++ "; //" ++ showa a ++ "\n"
            | (i,a) <- indexFrom ind as]


showas as = intercalate " " $ map showa as

showa (Var  v) = v
showa (LitI i) = show i
showa (LitL l) = show l
showa (LitF f) = show f
showa (LitD d) = show d
showa (LitC c) = "con_" ++ c
-- showa at = error $ "CodeGen.showa: not expecting Atom - " ++ show at

indexFrom :: Int -> [a] -> [(Int, a)]
indexFrom i xs = zip [i..] xs
