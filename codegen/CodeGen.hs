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
{-# LANGUAGE CPP #-}
#include "../options.h"

module CodeGen(
  cgObjs,
  cgStart,
  cgMain,
  cStart,
  cMain,
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

import Prelude
import Data.List(intercalate,nub)

import Data.Map (Map)
import qualified Data.Map as Map

#if USE_CAST
import CAST
import Text.PrettyPrint(render)
import Language.C.Pretty
import Language.C.Syntax
import Language.C.Data.Node
import Language.C.Data.Ident
#endif

#if USE_ARGTYPE
useArgType = True
#else
useArgType = False
#endif

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

--  C AST version
#if USE_CAST

-- PointerOrLiteral members
_polMembers(LitI i) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "INT",
#endif
                       cStructMember IntTy "i" i]

_polMembers(LitL l) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "LONG",
#endif
                       cStructMember IntTy "l" l]

_polMembers (LitF f) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "FLOAT",
#endif
                       cStructMember FloatTy "f" f]

_polMembers (LitD d) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "DOUBLE",
#endif
                       cStructMember DoubleTy "d" d]

_polMembers(LitC c) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "INT",
#endif
                       cStructMember EnumTy "i" ("con_" ++ c)]

_polMembers(Var v) = [
#if USE_ARGTYPE
                       cStructMember EnumTy "argType" "HEAPOBJ",
#endif
                       cStructMember EnumTy "op" v]


cPoLE :: Atom -> CExpr
cPoLE a =  CCompoundLit (CDecl [cTypeSpec "PtrOrLiteral"] [] undefNode)
              (_polMembers a) undefNode

cFnPtrFun :: String -> CExtDecl
cFnPtrFun name = cFunProto (cTypeSpec "FnPtr") ("fun_" ++ name)

getEnvRef :: String -> Env -> String
getEnvRef v env = render $ pretty $ lu v env 0 0

lu :: String -> Env -> Int -> Int -> CExpr
lu v [] _ _ = error $ "lu " ++ v ++ " failed"


lu v ((v',k):_) size' n | v == v' =
    case k of
      SO -> cCallExpr "HOTOPL" [CUnary CAdrOp (cVarE ("sho_" ++ v)) undefNode]

      HO size -> cCallExpr "HOTOPL"
                [CCast (CDecl [cTypeSpec "Obj"]
                [(Just (CDeclr Nothing [cPtrD] Nothing [] undefNode), Nothing, Nothing)] undefNode)
                (cCallExpr "STGHEAPAT" [cIntE $ toInteger (size+size'), cIntE $ toInteger (n+1)]) undefNode]

      FP{} -> error "fixme"

      FV{} -> error "fixme"

--      FV i -> CIndex (CMember (CMember (cVarE "self") (builtinIdent "op") False undefNode)
              (builtinIdent "payload") True undefNode) (cIntE $ toInteger i) undefNode

lu v ((_, HO size) : xs) size' n =
    lu v xs (size'+size) (n+1)

lu v (x : xs) size n = lu v xs size n

cgaE :: Env -> Atom -> CExpr
cgaE env (Var v) = lu v env 0 0
cgaE _ a = cPoLE a

cga :: Env -> Atom -> String
cga env (Var v) = cgv env v
cga _ a =  "(" ++ (render $ pretty $ cPoLE a) ++ ")"

emptyFunDeclr = [CFunDeclr (Right ([],False)) [] undefNode]

_cRegSOExpr :: String -> CBlockItem
_cRegSOExpr name =  CBlockStmt (CExpr
  (Just (CAssign CAssignOp (CIndex (CVar (builtinIdent "stgStatObj") undefNode)
  (CUnary CPostIncOp (CVar (builtinIdent "stgStatObjCount") undefNode) undefNode)
  undefNode) (CUnary CAdrOp (CVar (builtinIdent ("sho_" ++ name)) undefNode) undefNode)
  undefNode)) undefNode)

cRegisterSOs :: [String] -> (CExtDecl, CExtDecl)
cRegisterSOs names = let name = "registerSOs"
                     in (cFunProto (CTypeSpec (CVoidType undefNode)) name,
                         cFun VoidTy "" name emptyFunDeclr
                         (map _cRegSOExpr names))

registerSOs :: [Obj InfoTab] -> (String, String)
registerSOs objs = let (p,f) = cRegisterSOs (map (name . omd) objs)
                   in (render $ pretty p, render $ pretty f)

cMain :: Bool -> CExtDecl
cMain v =
  let top = [cCallVars "parseArgs" ["argc","argv"]
            ,cCall "initStg" []
            ,cCall "initGc" []
            ,cCallVars "CALL0_0" ["start"]
             ]
      body = top ++ [cCallVars "showStgHeap" [] | v] ++ [cIntReturn 0]

  in cFun IntTy "" "main"
  [CFunDeclr (Right ([CDecl [CTypeSpec (CIntType undefNode)]
  [(Just (CDeclr (Just (builtinIdent "argc")) [] Nothing [] undefNode),Nothing,Nothing)] undefNode,
  CDecl [CTypeSpec (CCharType undefNode)] [(Just (CDeclr (Just (builtinIdent "argv"))
  [cPtrD, cPtrD] Nothing [] undefNode),Nothing,Nothing)]
  undefNode],False)) [] undefNode] body

cStart :: CExtDecl
cStart = let body = [cCall "_POPVALS0" []
                  ,cCall "registerSOs" []
                  ,cUserPtrDecl "Cont" "showResultCont"
                  (cInitCall "stgAllocCallOrStackCont"
                  [cAddrvarE "it_stgShowResultCont", cIntE 0])
#if USE_ARGTYPE
                  ,cAssign (cMemberE "stgCurVal" "argType" False) (cVarE "HEAPOBJ")
#endif
                  ,cAssign (cMemberE "stgCurVal" "op" False) (cAddrvarE "sho_main")
                  ,cCall "STGJUMP1"
                     [CMember (cCallExpr "getInfoPtr" [cMemberE "stgCurVal" "op" False])
                     (builtinIdent "entryCode") True undefNode, cVarE "stgCurVal"]
                  ]
       in cFun UserTy "FnPtr" "start" emptyFunDeclr body

-- unused in C AST version
cgStart = error "cgStart"
cgMain = error "cgMain"

-- text version
#else

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

cga :: Env -> Atom -> String
cga env (Var v) = cgv env v
#if USE_ARGTYPE
cga env (LitI i) = "((PtrOrLiteral){.argType = INT,    .i = " ++ show i ++ " })"
cga env (LitL l) = "((PtrOrLiteral){.argType = LONG,   .l = " ++ show l ++ " })"
cga env (LitF f) = "((PtrOrLiteral){.argType = FLOAT,  .f = " ++ show f ++ " })"
cga env (LitD d) = "((PtrOrLiteral){.argType = DOUBLE, .d = " ++ show d ++ " })"
cga env (LitC c) = "((PtrOrLiteral){.argType = INT,    .i = con_" ++ c ++ " })"
#else
cga env (LitI i) = "((PtrOrLiteral){.i = " ++ show i ++ " })"
cga env (LitL l) = "((PtrOrLiteral){.l = " ++ show l ++ " })"
cga env (LitF f) = "((PtrOrLiteral){.f = " ++ show f ++ " })"
cga env (LitD d) = "((PtrOrLiteral){.d = " ++ show d ++ " })"
cga env (LitC c) = "((PtrOrLiteral){.i = con_" ++ c ++ " })"
#endif

cgStart :: String
cgStart = "\n\nFnPtr start() {\n" ++
            "  Cont *showResultCont = " ++ 
               "stgAllocCallOrStackCont(&it_stgShowResultCont, 0);\n" ++
#if USE_ARGTYPE
            "  stgCurVal.argType = HEAPOBJ;\n" ++
#endif
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
  in if v then top ++ "  showStgHeap();\n  GC();\n" ++ bot else top ++ bot

registerSOs :: [Obj InfoTab] -> (String, String)
registerSOs objs =
    ("void registerSOs();",
     "void registerSOs() {\n" ++
        concat [ "  stgStatObj[stgStatObjCount++] = &" ++ s ++ ";\n"
                 | s <- shoNames objs ] ++
     "}\n")

-- unused in text version
cStart = error "cStart"
cMain = error "cMain"

-- end of USE_CAST
#endif


-- boxed expression predicate
isBoxede e = isBoxed $ typ $ emd e


cgUBa env (Var v)  t   =  "(" ++ cgv env v ++ ")." ++ t
cgUBa env (LitI i) "i" = show i
cgUBa env (LitD d) "d" = show d
cgUBa _ at _ = error $ "CodeGen.cgUBa: not expecting Atom - " ++ show at
-- cgUBa env (LitF f) "f" = show f

cgv env v = getEnvRef v env -- ++ "/* " ++ v ++ " */"


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

cgos :: Env -> [Obj InfoTab] -> State Int [(String, String)]
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
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
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
            "  fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
            "  Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);\n" ++
            "  stg_fp->layout = " ++ npStrToBMStr "P" ++ ";\n" ++
            "  stg_fp->payload[0] = stgCurVal;\n" ++
            "  PtrOrLiteral *" ++ fvpp ++ " = &(stg_fp->payload[0]);\n" ++
            "  stgThunk(stgCurVal);\n" ++
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
                 show (length pnstring + 1) ++ ");\n" ++
            "  cp->layout = " ++ npStrToBMStr ('P' : pnstring ) ++ ";\n" ++
            "  cp->payload[ 0 ] = " ++ cgv env f' ++ ";\n" ++
            concat ["  cp->payload[ " ++ show i ++ " ] = " ++ cga env a ++ ";\n"
                    | (i,a) <- zip [1..] as ] ++

            (if direct then
                "// DIRECT TAIL CALL " ++ f ++ " " ++ showas as ++ "\n" ++
--                "STGJUMP0(getInfoPtr(cp->payload[0].op)->funFields.trueEntryCode);\n"
                -- really direct
                "if (evalStrategy == STRICT1) stgEvalStackFrameArgs(cp);\n" ++
                "STGJUMP0(fun_" ++ f ++ ");\n"
            else
                "  // INDIRECT TAIL CALL " ++ f' ++ " " ++ showas as ++ "\n" ++
                "  STGJUMP0(stgApply);\n") ++
            "}\n"
    in return ((inline, Yes), [])


-- return (inline code, [(forward, fundef)])
cge :: Env -> Expr InfoTab -> State Int ((String,YPN), [(String, String)])

cge env e@(EAtom it a) =
    let inline = "stgCurVal = " ++ cga env a ++ "; // " ++ showa a ++ "\n" ++
                 (if isBoxede e then
                      "// boxed EAtom, stgCurVal updates itself \n" ++
                      "STGJUMP();\n"
                  else
                      "// unboxed EAtom\n")
    in return ((inline, if isBoxede e then Yes else No), [])

cge env e@(EFCall it f eas) =
    case (knownCall it) of
      Nothing -> stgApplyGeneric env f eas False
      Just kit -> if arity kit == length eas
                  then stgApplyGeneric env f eas True
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

                   PintToBool ->
                       "stgCurVal = " ++
                       arg0 "i" ++ "?" ++ getEnvRef "true"  env ++
                                   ":" ++ getEnvRef "false" env ++ ";\n"

        cPrefixII op =
#if USE_ARGTYPE
                        "stgCurVal.argType = INT;\n" ++
#endif
                        "stgCurVal.i = " ++ op ++ arg0 "i" ++ ";\n"

        cInfixIII op =
#if USE_ARGTYPE
                        "stgCurVal.argType = INT;\n" ++
#endif
                        "stgCurVal.i = " ++ arg0 "i" ++ op ++ arg1 "i" ++ ";\n"

        cInfixIIB op =
#if USE_ARGTYPE
                        "stgCurVal.argType = BOOL;\n" ++
#endif
                        "stgCurVal.i = " ++ arg0 "i" ++ op ++ arg1 "i" ++ ";\n"

        cFunIII fun =
#if USE_ARGTYPE
                      "stgCurVal.argType = INT;\n" ++
#endif
                      "stgCurVal.i = " ++ fun ++ "(" ++ arg0 "i" ++ ", " ++ arg1 "i" ++ ");\n"
    in return ((inline, No), [])

cge env (ELet it os e) =
    let names = map oname os
        decl = concat [ "PtrOrLiteral *" ++ name ++ ";\n" | name <- names ] ++
               "{Cont *contp = stgAllocCallOrStackCont(&it_stgLetCont, " ++ 
                               show (length os) ++ ");\n" ++
               "memset(contp->payload, 0, " ++ 
                  show (length os) ++ " * sizeof(PtrOrLiteral));\n" ++
               concat [ name ++ " = &(contp->payload[" ++ show i ++ "]);\n" |
                        (name, i) <- zip names [0..] ] ++
               "contp->layout = " ++ npStrToBMStr (replicate (length os) 'P') ++
               ";}\n" -- only size actually matters
        env'  = zip names (map HO names) ++ env
#if USE_CAST
        (sizes, cDecls, cBuildcodes) = unzip3 $ map (buildHeapObj env') os
        decls = render $ pretty cDecls 
        buildcodes =  intercalate "\n" (map (render . pretty) cBuildcodes))
#else
        (decls, buildcodes) = unzip $ map (buildHeapObj env') os
#endif
    in do
      ofunc <- cgos env' os
      ((einline, ypn), efunc) <- cge env' e
      return ((decl ++ concat decls ++ concat buildcodes ++ einline, ypn),
              ofunc ++ efunc)

-- scrutinee does no heap allocation
{-
cge env (ECase _ e a@(Alts italts alts aname)) =
    do (ecode, efunc) <- cge env e
       (acode, afunc) <- cgalts_noheapalloc env a (isBoxede e)
       let name = "ccont_" ++ aname
           pre = "// scrutinee no heap allocation\n"
       return (pre ++ ecode ++ acode, efunc ++ afunc)

-- ADef only or unary sum => no C switch
cgalts_noheapalloc env (Alts it alts name) boxed =
    let scrutName = "scrut_" ++ name
    in do
      let switch = length alts > 1
      codefuncs <- mapM (cgalt env switch scrutName) alts
      let (codes, funcss) = unzip codefuncs
      let phonyforward = "FnPtr " ++ name ++ "();"
          phonyfun = "FnPtr "++ name ++ "() {\n" ++
                     "}\n"
      let inl = "// " ++ show (ctyp it) ++ "\n" ++
                "PtrOrLiteral " ++ scrutName ++ " = stgCurVal;\n" ++
                (if switch then
                     (if boxed then
                          "switch(getInfoPtr(stgCurVal.op)->conFields.tag) {\n"
                      else
                          "switch(stgCurVal.i) {\n"
                     ) ++
                     indent 2 (concat codes) ++
                   "}\n"
                 else concat codes)
      return (inl, (phonyforward, phonyfun) : concat funcss)
-}

-- TOFIX:  even if scrutinee doesn't heap alloc it may return through the
-- continuation stack, so we need better analysis
-- cge env (ECase _ e a@(Alts italts alts aname)) | (not $ noHeapAlloc $ emd e) =
cge env (ECase _ e a@(Alts italts alts aname)) =
    let scrutName = "scrut_" ++ aname
        cname = "ccont_" ++ aname
        pre = "// scrutinee may heap alloc\n" ++
              "Cont *" ++ cname ++ " = stgAllocCont( &it_" ++ aname ++ ");\n" ++
              "// dummy value for scrutinee\n" ++
              cname ++ "->payload[0].i = 0;\n" ++
#if USE_ARGTYPE
              cname ++ "->payload[0].argType = INT;\n" ++
#endif
              (if fvs italts == [] then
                 "// no FVs\n"
               else
                 "// load payload with FVs " ++
                         intercalate " " (map fst $ fvs italts) ++ "\n") ++
                 (loadPayloadFVs env (map fst $ fvs italts) 1 cname)
    in do ((ecode, eypn), efunc) <- cge env e
          (acode, afunc) <- cgalts env a (isBoxede e) scrutName
          return ((pre ++ ecode ++ acode, eypn), efunc ++ afunc)

-- ADef only or unary sum => no C switch
cgalts env (Alts it alts name) boxed scrutName =
    let contName = "ccont_" ++ name
        fvp = "fvp"
        -- scrutName in case scrutinee is explicitly bound to variable
        altenv = zip (scrutName : (map fst $ fvs it)) (map (FP fvp) [0..])
        env' = altenv ++ env
        forward = "FnPtr " ++ name ++ "();"
        switch = length alts > 1
    in do
      codefuncs <- mapM (cgalt env' switch fvp) alts
      let (codes, funcss) = unzip codefuncs
      let body =
              "fprintf(stderr, \"" ++ name ++ " here\\n\");\n" ++
              "Cont *" ++ contName ++ " = stgGetStackArgp();\n" ++
              "// make self-popping\n" ++
              "stgCaseToPopMe(" ++ contName ++ ");\n" ++
              "PtrOrLiteral *" ++ fvp ++ 
                  " = &(" ++ contName ++ "->payload[0]);\n" ++
--              concat ["  PtrOrLiteral " ++ v ++
--                      " = " ++ contName ++ "->payload[" ++ show i ++ "];\n"
--                      | (i,v) <- indexFrom 0 $ map fst $ fvs it ] ++
              fvp ++ "[0] = stgCurVal;\n" ++
              optStr boxed (contName ++ "->layout.bitmap.mask |= 0x1;\n") ++
              (if switch then
                 (if boxed then
--                    "switch(getInfoPtr(stgCurVal.op)->conFields.tag) {\n"
                    "stgCurVal.op = NULL;\n" ++
                    "switch(getInfoPtr(" ++ fvp ++ "[0].op)->conFields.tag) {\n"
                  else
                    "switch(stgCurVal.i) {\n"
                 ) ++
                   indent 2 (concat codes) ++
                 "}\n"
               else concat codes)
      let fun = 
              "// " ++ show (ctyp it) ++ "\n" ++ 
              "FnPtr "++ name ++ "() {\n" ++ indent 2 body ++ "}\n"

      return ("", (forward, fun) : concat funcss)

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
      return (code, func)

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
      return (code, func)


-- ****************************************************************
-- buildHeapObj is only invoked by ELet so TLDs not built

#if USE_CAST

buildHeapObj :: Env -> Obj InfoTab -> (Int, CBlockItem, [CBlockItem])
buildHeapObj env o =
    let (size, rvals) = bho env o
        name = oname o
        decl = cNewHeapObj name name 
    in (size, decl, rvals)
--render $ pretty decl, 
--        intercalate "\n" (map (render . pretty) rvals))

bho :: Env -> Obj InfoTab -> (Int, [CBlockItem])
bho env (FUN it vs e name) =
    (length $ fvs it, cLoadPayloadFVs env (map fst $ fvs it) 0 name)

bho env (PAP it f as name) = error "unsupported explicit PAP"

bho env (CON it c as name) =
    let ps = [cAssign (cPayloadE name i) (cgaE env a) 
                | (i,a) <- indexFrom 0 (projectAtoms as) ]
    in (length ps, ps)

bho env (THUNK it e name) =
    let fv = fvs it
    in (1 + (length fv), cLoadPayloadFVs env (map fst fv) 1 name)

bho env (BLACKHOLE it name) = (1,[])

cLoadPayloadFVs env fvs ind name =
    [cAssign (cPayloadE name i) (lu v env 0 0)
      | (i,v) <- indexFrom ind $ fvs ]

-- load atoms into payload starting at index ind
cLoadPayloadAtoms env as ind name =
    [cAssign (cPayloadE name i) (cgaE env a) 
      | (i,a) <- indexFrom ind as]


#else

buildHeapObj :: Env -> Obj InfoTab -> (String, String)
buildHeapObj env o =
    let rval = bho env o
        name = oname o
        decl = name ++ "->op = stgNewHeapObj( &it_" ++ name ++ " );\n" ++
               optStr useArgType (name ++ "->argType = HEAPOBJ;\n")
    in (decl, rval)


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

#endif

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

