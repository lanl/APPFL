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

Formal parameter or local variable from case cont:  by name, e.g. "x"

Alt constructor var:  "stgCurVal.op->payload[i], bind these"

Alt default var:  "stgCurVal, bind it"

-}

{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE CPP #-}
#include "options.h"

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


data RVal = SHO           -- static heap obj
          | HO Int        -- heap obj,  payload size
          | FP            -- formal param or local var, use name as is
          | FV Int        -- free var, self->payload[Int]
          | AC Var Int    -- alt con
          | AD Var        -- alt def
            deriving(Eq,Show)

type Env = [(String, RVal)]

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
      SHO -> cCallExpr "HOTOPL" [CUnary CAdrOp (cVarE ("sho_" ++ v)) undefNode]

      HO size -> cCallExpr "HOTOPL"
                [CCast (CDecl [cTypeSpec "Obj"]
                [(Just (CDeclr Nothing [cPtrD] Nothing [] undefNode), Nothing, Nothing)] undefNode)
                (cCallExpr "STGHEAPAT" [cIntE $ toInteger (size+size'), cIntE $ toInteger (n+1)]) undefNode]

      FP -> cVarE v

      FV i -> CIndex (CMember (CMember (cVarE "self") (builtinIdent "op") False undefNode)
              (builtinIdent "payload") True undefNode) (cIntE $ toInteger i) undefNode

      AC v i -> CIndex (cMemberE v "payload" True) (cIntE $ toInteger i) undefNode

      AD v -> cVarE v

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

_cRegSHOExpr :: String -> CBlockItem
_cRegSHOExpr name =  CBlockStmt (CExpr
  (Just (CAssign CAssignOp (CIndex (CVar (builtinIdent "stgStatObj") undefNode)
  (CUnary CPostIncOp (CVar (builtinIdent "stgStatObjCount") undefNode) undefNode)
  undefNode) (CUnary CAdrOp (CVar (builtinIdent ("sho_" ++ name)) undefNode) undefNode)
  undefNode)) undefNode)

cRegisterSHOs :: [String] -> (CExtDecl, CExtDecl)
cRegisterSHOs names = let name = "registerSHOs"
                      in (cFunProto (CTypeSpec (CVoidType undefNode)) name,
                          cFun VoidTy "" name emptyFunDeclr
                          (map _cRegSHOExpr names))

registerSHOs :: [Obj InfoTab] -> (String, String)
registerSHOs objs = let (p,f) = cRegisterSHOs (map (name . omd) objs)
                    in (render $ pretty p, render $ pretty f)

cMain :: Bool -> CExtDecl
cMain v =
  let top = [cCallVars "parseArgs" ["argc","argv"]
            ,cCall "initStg" []
            ,cCall "initCmm" []
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
                  ,cCall "registerSHOs" []
                  ,cUserPtrDecl "Cont" "showResultCont"
                  (cInitCall "stgAllocCallCont"
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

getEnvRef :: String -> Env -> String
getEnvRef v env = lu v env 0 0

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
cgStart = "\n\nDEFUN0(start)" ++
            "  registerSHOs();\n" ++
            "  Cont *showResultCont = stgAllocCallCont(&it_stgShowResultCont, 0);\n" ++
            "  showResultCont->layout.bits = 0x0UL; // empty\n" ++
#if USE_ARGTYPE
            "  stgCurVal.argType = HEAPOBJ;\n" ++
#endif
            "  stgCurVal.op = &sho_main;\n" ++
            "  STGJUMP1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal);\n" ++
            "}\n\n"

cgMain :: Bool -> String
cgMain v = let top = "int main (int argc, char **argv) {\n" ++
                     "  parseArgs(argc, argv);\n" ++
                     "  initStg();\n" ++
                     "  initCmm();\n" ++
                     "  initGc();\n" ++
                     "  CALL0_0(start);\n"
               bot = "  return 0;\n" ++ "}\n\n"
  in if v then top ++ "  showStgHeap();\n  GC();\n" ++ bot else top ++ bot

registerSHOs :: [Obj InfoTab] -> (String, String)
registerSHOs objs =
    ("void registerSHOs();",
     "void registerSHOs() {\n" ++
        concat [ "  stgStatObj[stgStatObjCount++] = &" ++ s ++ ";\n"
                 | s <- shoNames objs ] ++
     "}\n")

-- unused in text version
cStart = error "cStart"
cMain = error "cMain"

-- end of USE_CAST
#endif


-- boxed expression predicate
{-
isBoxede e = case typ $ emd e of MCon False _ _ -> False
                                 MPrim _        -> False
                                 _              -> True
-}
isBoxede e = isBoxed $ typ $ emd e


cgUBa env (Var v)  t   =  "(" ++ cgv env v ++ ")." ++ t
cgUBa env (LitI i) "i" = show i
cgUBa env (LitD d) "d" = show d
cgUBa _ at _ = error $ "CodeGen.cgUBa: not expecting Atom - " ++ show at
-- cgUBa env (LitF f) "f" = show f

cgv env v = getEnvRef v env -- ++ "/* " ++ v ++ " */"



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



cgos :: Env -> [Obj InfoTab] -> State Int [(String, String)]
cgos env = concatMapM (cgo env)


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

cgo :: Env -> Obj InfoTab -> State Int [(String, String)]
cgo env o@(FUN it vs e name) =
    do
      let env' = zip (map fst $ fvs it) (map FV [0..]) ++
                 zip vs (repeat FP) ++
                 env
--          vts@((bvs,uvs),(bts,uts)) = permArgs vs $ typ it
      (inline, funcs) <- cge env' e
      let forward = "FnPtr fun_" ++ name ++ "();"
          func =
            "// " ++ show (ctyp it) ++ "\n" ++
--            "// " ++ show vts ++ "\n" ++
            "DEFUN" ++ show (length vs + 1) ++ "(fun_" ++
            name ++ ", self, " ++
            intercalate ", " vs ++
--            intercalate ", " (bvs ++ uvs) ++
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
      let env' = zip (map fst $ fvs it) (map FV [1..]) ++ env
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

{-
stgApplyGeneric env f eas =
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        inline =
            "// INDIRECT TAIL CALL " ++ f ++ " " ++ showas as ++ "\n" ++
            "STGAPPLY" ++ pnstring ++ "(" ++
            intercalate ", " (cgv env f : map (cga env) as) ++
            ");\n"
    in return (inline, [])
-}

stgApplyGeneric env f eas =
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        inline =
            -- new STACKFRAME
            "{ Cont *cp = stgAllocStackCont( &it_stgStackCont, " ++ 
                                             show (length pnstring + 1) ++ ");\n" ++
            "  cp->layout = " ++ npStrToBMStr ('P' : pnstring ) ++ ";\n" ++
            "  cp->payload[ 0 ] = " ++ cgv env f ++ ";\n" ++
            concat ["  cp->payload[ " ++ show i ++ " ] = " ++ cga env a ++ ";\n"
                    | (i,a) <- zip [1..] as ] ++
            -- now pop it
--            "  cp = stgPopCont();\n" ++
            "}\n" ++

            "// INDIRECT TAIL CALL " ++ f ++ " " ++ showas as ++ "\n" ++
--            "STGAPPLY" ++ pnstring ++ "(" ++
--            intercalate ", " (cgv env f : map (cga env) as) ++
            "STGJUMP0(stgApply" ++ pnstring ++
            ");\n"
    in return (inline, [])



stgApplyDirect env (EFCall it f eas) =
    let as = map ea eas
        inline =
            "// DIRECT TAIL CALL " ++ f ++ " " ++ showas as ++ "\n" ++
            "STGAPPLY" ++ show (length as) ++ "(" ++
              intercalate ", " (cgv env f : map (cga env) as) ++
            ");\n"
    in return (inline, [])


stgApplyDirect env expr =
    error $ "CodeGen.stgApplyDirect: not expecting Expr - " ++ show (pprint expr)


-- return (inline code, [(forward, fundef)])
cge :: Env -> Expr InfoTab -> State Int (String, [(String, String)])

cge env e@(EAtom it a) =
    let inline = "stgCurVal = " ++ cga env a ++ "; " ++ "// " ++ showa a ++ "\n" ++
                 (if isBoxede e then
                      "// boxed EAtom, stgCurVal updates itself \n" ++
                      "STGJUMP();\n"
                  else
                      "// unboxed EAtom\n")
    in return (inline, [])

cge env e@(EFCall it f eas) =
    case (knownCall it) of
      Nothing -> stgApplyGeneric env f eas
      Just kit -> if arity kit == length eas
                  then stgApplyGeneric env f eas -- stgApplyDirect env e
                  else stgApplyGeneric env f eas


{-
cge env e@(EFCall it f eas) =
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        inline =
            "// " ++ f ++ " " ++ showas as ++ "\n" ++
            "STGAPPLY" ++ pnstring ++ "(" ++
            intercalate ", " (cgv env f : map (cga env) as) ++
            ");\n"
    in return (inline, [])
-}

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
    in return (inline, [])


cge env (ELet it os e) =
    let names = map oname os
        env'  = (reverse $ zip names (map HO sizes)) ++ env
#if USE_CAST
        (sizes, cDecls, cBuildcodes) = unzip3 $ map (buildHeapObj env') os
        decls = render $ pretty cDecls 
        buildcodes =  intercalate "\n" (map (render . pretty) cBuildcodes))
#else
        (sizes, decls, buildcodes) = unzip3 $ map (buildHeapObj env') os
#endif
    in do
      ofunc <- cgos env' os
      (einline, efunc) <- cge env' e
      return (concat decls ++ concat buildcodes ++ einline,
              ofunc ++ efunc)


-- TOFIX:  even if scrutinee doesn't heap alloc it may return through the
-- continuation stack, so we need better analysis
-- cge env (ECase _ e a@(Alts italts alts aname)) | (not $ noHeapAlloc $ emd e) =
cge env (ECase _ e a@(Alts italts alts aname)) =
    do (ecode, efunc) <- cge env e
       (acode, afunc) <- cgalts env a (isBoxede e)
       let name = "ccont_" ++ aname
           pre = "// scrutinee may heap alloc\n" ++
              "Cont *" ++ name ++
              " = stgAllocCont( &it_" ++ aname ++ ");\n" ++
              (if fvs italts == [] then
                 "// no FVs\n"
               else
                 "// load payload with FVs " ++
                 intercalate " " (map fst $ fvs italts) ++ "\n") ++
                 (loadPayloadFVs env (map fst $ fvs italts) 0 name)
--           scrut = if isBoxede e then
--                       "  // boxed scrutinee\n" ++
--                       "  STGEVAL(stgCurVal);\n"
--                   else "  // unboxed scrutinee\n"
--       return (pre ++ ecode ++ scrut ++ acode, efunc ++ afunc)
       return (pre ++ ecode ++ acode, efunc ++ afunc)

-- scrutinee does no heap allocation
{-
cge env (ECase _ e a@(Alts italts alts aname)) =
    do (ecode, efunc) <- cge env e
       (acode, afunc) <- cgalts_noheapalloc env a (isBoxede e)
       let name = "ccont_" ++ aname
           pre = "// scrutinee no heap allocation\n"
       return (pre ++ ecode ++ acode, efunc ++ afunc)
-}

-- ADef only or unary sum => no C switch
cgalts_noheapalloc env (Alts it alts name) boxed =
    let scrutName = "scrut_" ++ name
    in do
      let switch = length alts > 1
      codefuncs <- mapM (cgalt env switch scrutName) alts
      let (codes, funcss) = unzip codefuncs
      let phonyforward = "FnPtr " ++ name ++ "();"
          phonyfun = "DEFUN0("++ name ++ ") {\n" ++
                     "  ENDFUN;\n" ++
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
                     "  Cont *" ++ contName ++ " = stgPopCont();\n" ++
                     concat ["  PtrOrLiteral " ++ v ++
                             " = " ++ contName ++ "->payload[" ++ show i ++ "];\n"
                             | (i,v) <- indexFrom 0 $ map fst $ fvs it ]
                 else
                     "  stgPopCont();\n") ++
                "  PtrOrLiteral " ++ scrutName ++ " = stgCurVal;\n" ++
                (if switch then
                     (if boxed then
                          "  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {\n"
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
    let DataCon c' ms = luDCon c (cmap it)
        (_,_,perm) = partPerm isBoxed ms
        -- eenv = zip vs (map (AC $ scrutName ++ ".op") [0..])
        eenv = zzip vs (map (AC $ scrutName ++ ".op") perm)
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

buildHeapObj :: Env -> Obj InfoTab -> (Int, String, String)
buildHeapObj env o =
    let (size, rval) = bho env o
        name = oname o
        decl = "Obj *" ++ name ++ " = stgNewHeapObj( &it_" ++ name ++ " );\n"
    in (size, decl, rval)


bho :: Env -> Obj InfoTab -> (Int, String)
bho env (FUN it vs e name) =
    (length $ fvs it, loadPayloadFVs env (map fst $ fvs it) 0 name)


bho env (PAP it f as name) = error "unsupported explicit PAP"


-- TODO: the size here should be based on the FUN rather than being maxPayload
{-

    (maxPayload, loadPayloadFVs env (map fst $ fvs it) 0 name ++
                 loadPayloadAtoms env (projectAtoms as) (length $ fvs it) name)
-}

-- CON is special in that the payload contains not just FVs but literals
-- as well, and we need their types.  This could be done:
-- 0.  The right way would be to have Atom be typed (major TODO)
-- 1.  Use HMStg.luTCon using cmap it, typ it, and c, subsequent gyrations
-- 2.  Use fvs type information
-- 3.  Embedding the Atoms into typed expressions
bho env (CON it c as name) =
    let ps = [name ++ "->payload[" ++ show i ++ "] = " ++
                       cga env a ++ "; // " ++ showa a ++ "\n"
                       | (i,a) <- indexFrom 0 (projectAtoms as) ]
    in (length ps, concat ps)

bho env (THUNK it e name) =
    let fv = fvs it
    in (1 + (length fv), loadPayloadFVs env (map fst fv) 1 name)

bho env (BLACKHOLE it name) = (1,"")

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

