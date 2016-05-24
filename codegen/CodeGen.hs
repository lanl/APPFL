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
import Language.C.Syntax (Definition, Initializer, Func, Exp, BlockItem)
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
#if USE_ARGTYPE
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
      bot = [citems|return 0;|]
      items = if v then top ++ extra ++ bot else top ++ bot 
   in [cfun|
        int main (int argc, char **argv)
        {
          $items:items
              
        }
      |]
    
cregisterSOs :: [Obj InfoTab] -> (Definition, Func)
cregisterSOs objs = 
  let proto = [cedecl| void registerSOs(); |]
      items = [ [citem|stgStatObj[stgStatObjCount++] = &$id:s; |] | s <- shoNames objs]
      fun = [cfun| void registerSOs() { $items:items } |]
  in (proto, fun)

cgStart :: String
cgStart = pp cStart

cgMain :: Bool -> String
cgMain v = pp $ cMain v

registerSOs :: [Obj InfoTab] -> (String, String)
registerSOs objs = let (p,f) = cregisterSOs objs
                   in (pp p, pp f)

listLookup k [] = Nothing
listLookup k ((k',v):xs) | k == k' = Just v
                         | otherwise = listLookup k xs

cgetEnvRef :: String -> Env -> Exp
cgetEnvRef v kvs = 
  case listLookup v kvs of
    Nothing -> error $ "cgetEnvRef " ++ v ++ " failed"
    Just k ->
        case k of
          SO       -> [cexp| HOTOPL(&$id:("sho_" ++ v)) |]
          HO name  -> [cexp| (*$id:name) |]
          FP fp i  -> [cexp| $id:fp[$int:i] |]
          FV fpp i -> [cexp| $id:fpp->op->payload[$int:i] |]


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



-- 2nd arg is comment to attach to statement
ccga :: Env -> Atom -> (Exp, String)
ccga env (Var v) =  ccgv env v

ccga env (LitI i) = (
  if useArgType then 
    [cexp| ((typename PtrOrLiteral){.argType = INT, .i = $int:i}) |]
  else
    [cexp| ((typename PtrOrLiteral){.i = $int:i}) |], "")


ccga env (LitL l) = (              
  if useArgType then 
    [cexp| ((typename PtrOrLiteral){.argType = LONG, .l = $lint:l}) |]
  else
    [cexp| ((typename PtrOrLiteral){.l = $lint:l}) |], "")    
    
ccga env (LitF f) =
  let f' = toRational f
      e = if useArgType then 
            [cexp| ((typename PtrOrLiteral){.argType = FLOAT, .f = $float:f'}) |]
          else
            [cexp| ((typename PtrOrLiteral){.f = $float:f'}) |] 
  in (e, "")
 
  
ccga env (LitD d) = 
  let d' = toRational d          
      e = if useArgType then 
            [cexp| ((typename PtrOrLiteral){.argType = DOUBLE, .d = $double:d'}) |]
          else
            [cexp| ((typename PtrOrLiteral){.d = $double:d'}) |] 
  in (e, "")   

ccga env (LitC c) = 
  let c' = "con_" ++ c        
      e = if useArgType then 
            [cexp| ((typename PtrOrLiteral){.argType = INT, .i = $id:c'}) |]
          else
            [cexp| ((typename PtrOrLiteral){.c = $id:c'}) |] 
  in (e, "")
  
ccgv :: Env -> String -> (Exp, String)
ccgv env v = (cgetEnvRef v env, "/* " ++ v ++ " */") 

cga env x = pp $ fst $ ccga env x


-- boxed expression predicate
isBoxede e = isBoxed $ typ $ emd e

ccgUBa :: Env -> Atom -> String -> Exp
ccgUBa env (Var v)  t   = [cexp| ($exp:(fst $ ccgv env v)).$id:t |] 
ccgUBa env (LitI i) "i" = [cexp| $int:i |]
ccgUBa env (LitD d) "d" = [cexp| $double:(toRational d) |]
ccgUBa _ at _ = error $ "CodeGen.cgUBa: not expecting Atom - " ++ show at


--cgv env v = getEnvRef v env ++ "/* " ++ v ++ " */"
cgv env v = pp $ fst $ ccgv env v


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

cgObjs' :: [Obj InfoTab] -> [String] -> ([String],[String])
cgObjs' objs runtimeGlobals = 
  let (defs, funs) = ccgObjs objs runtimeGlobals
  in (map pp defs, map pp funs)

ccgObjs :: [Obj InfoTab] -> [String] -> ([Definition],[Func])
ccgObjs objs runtimeGlobals =
   let tlnames = runtimeGlobals ++ map (name . omd) objs
       env = zip tlnames $ repeat SO
       (funcs, _) = runState (ccgos env objs) 0
       (forwards, fundefs) = unzip funcs
       (forward, fundef) = cregisterSOs objs
   in (forward:forwards, fundef:fundefs)


ccgos :: Env -> [Obj InfoTab] -> State Int [(Definition,  Func)]
ccgos env = concatMapM (ccgo env)

ccgo :: Env -> Obj InfoTab -> State Int [(Definition, Func)]
ccgo env o@(FUN it vs e name) =
    let cforward = [cedecl| typename FnPtr $id:("fun_" ++ name)();|]
        argp = "argp" -- also free variable pointer pointer
        fps = "self":vs
        env' = zip (map fst $ fvs it) (map (FV argp) [0..]) ++
               zip fps (map (FP argp) [0..]) ++
               env
    in do
      ((inline, ypn), funcs) <- ccge env' e
      let top = [citems|
                  $comment:("// " ++ show (ctyp it))
                  $comment:("// " ++ name ++ "(self, " ++ intercalate ", " vs ++ ")")
                  LOG(LOG_INFO, $string:(name ++ " here\n"));
                  typename PtrOrLiteral *$id:argp = &(stgGetStackArgp()->payload[0]);
                |]
          bot = [citems| STGRETURN0();|] 
          items = if (ypn /= Yes ) then top ++ inline ++ bot else top ++ inline      
          cfunc = [cfun|
                      typename FnPtr $id:("fun_" ++ name)()
                      {
                        $items:items
                      }
                    |]                     
      return $ (cforward, cfunc) : funcs
      
ccgo env (PAP it f as name) = return []

ccgo env (CON it c as name) = return []  
  
ccgo env o@(THUNK it e name) =
  let fvpp = "fvpp"
      env' = zip (map fst $ fvs it) (map (FV fvpp) [1..]) ++ env
      cforward = [cedecl| typename FnPtr $id:("thunk_" ++ name)();|]
  in do
    ((inline,ypn), funcs) <- ccge env' e
    let top = [citems|
                $comment:("// " ++ show (ctyp it))
                LOG(LOG_INFO, $string:(name ++ " here\n"));
                $comment:("// access free vars through frame pointer for GC safety")
                $comment:("// is this really necessary???");
                typename Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);
                stg_fp->layout = $string:(npStrToBMStr "P");
                stg_fp->payload[0] = stgCurVal;
                typename PtrOrLiteral *$id:fvpp = &(stg_fp->payload[0]);
                stgThunk(stgCurVal);
                stgCurVal.op = NULL;  
              |]
        bot = [citems| STGRETURN0;|] 
        items = if (ypn /= Yes ) then top ++ inline ++ bot else top ++ inline             
        cfunc = [cfun|
                    typename FnPtr $id:("thunk_" ++ name)()
                    {
                      $items:items
                    }
                  |]        
    return $ (cforward, cfunc) : funcs

ccgo env (BLACKHOLE {}) = return []    

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
        cforward = [cedecl| typename FnPtr $id:("fun_" ++ name)();|]
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
          top = [citems|
                  $comment:("// " ++ show (ctyp it))
                  $comment:("// " ++ name ++ "(self, " ++ intercalate ", " vs ++ ")")
                  LOG(LOG_INFO, $string:(name ++ " here\n"));
                  typename PtrOrLiteral *$id:argp = &(stgGetStackArgp()->payload[0]);
                |]
          inlines = []  -- TODO: inline needs to go in functions
          bot = [citems| STGRETURN0;|] 
          items = if (ypn /= Yes ) then top ++ inlines ++ bot else top ++ inlines      
          cfunc = [cfun|
                      typename FnPtr $id:("fun_" ++ name)()
                      {
                        $items:items
                      }
                    |]
                         
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
      cforward = [cedecl| typename FnPtr $id:("thunk_" ++ name)();|]
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
          
          top = [citems|
                  $comment:("// " ++ show (ctyp it))
                  LOG(LOG_INFO, $string:(name ++ " here\n"));
                  $comment:("// access free vars through frame pointer for GC safety")
                  $comment:("// is this really necessary???");
                  typename Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);
                  stg_fp->layout = $string:(npStrToBMStr "P");
                  stg_fp->payload[0] = stgCurVal;
                  typename PtrOrLiteral *$id:fvpp = &(stg_fp->payload[0]);
                  stgThunk(stgCurVal);
                  stgCurVal.op = NULL;  
                |]
          inlines = []  -- TODO: inline needs to go in functions
          bot = [citems| STGRETURN0;|] 
          items = if (ypn /= Yes ) then top ++ inlines ++ bot else top ++ inlines             
          cfunc = [cfun|
                      typename FnPtr $id:("thunk_" ++ name)()
                      {
                        $items:items
                      }
                    |]
          
      return $ (forward, func) : funcs

cgo env (BLACKHOLE {}) =
    return []

-- ****************************************************************

cstgApplyGeneric env f eas direct =
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        -- HACK
        f' = if f == "stg_case_not_exhaustive" then
                 f ++ pnstring
             else f
        (expr0, comment0) = ccga [] (LitI 0) 
        (expr1, comment1) = ccgv env f'  
        in1 = [citems|
                typename Cont *cp = stgAllocCallOrStackCont( &it_stgStackCont,
                $int:(length pnstring + 2));
                cp->layout =  $lint:(npStrToBMInt ('N' : 'P' : pnstring ));
                cp->payload[ 0 ] = $exp:(expr0); $comment:(comment0)
                cp->payload[ 1 ] = $exp:(expr1); $comment:(comment1)
              |]
        in2 = [ [citem| cp->payload[$int:i] = $exp:(expr); $comment:(comm) |] 
                  | (i,a) <- zip [2..] as, let (expr, comm) = ccga env a ]
        in3 = if direct then 
                [citems|
                  $comment:("// DIRECT TAIL CALL " ++ f ++ " " ++ showas as)
                  if (evalStrategy == STRICT1) stgEvalStackFrameArgs(cp);
                  STGJUMP0($id:("fun_" ++ f)); 
                |]
              else 
                [citems|
                  $comment:("// INDIRECT TAIL CALL " ++ f' ++ " " ++ showas as)
                  STGJUMP0(stgApplyNew); 
                |]
    in return ((in1 ++ in2 ++ in3, Yes), [])

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

cstgCurValUArgType :: String -> [BlockItem]
cstgCurValUArgType ty = if useArgType
                           then [citems| stgCurValU.argType = $id:ty; |]
                           else []

stgCurValUArgType :: String -> String
stgCurValUArgType ty = if useArgType
                       then "stgCurValU.argType = " ++ ty ++ ";\n"
                       else ""


ccge :: Env
  -> Expr InfoTab
  -> State Int (([BlockItem], YPN), [(Definition, Func)])
ccge env e@(EAtom it a) =
  let (expr,comm) = ccga env a
      inline = 
        if isBoxede e then
          [citems|
            stgCurVal = $exp:expr; $comment:(comm)
            $comment:("// boxed EAtom, stgCurVal updates itself")
            STGJUMP();
          |]
        else
          [citems|
            stgCurValU = $exp:expr; $comment:("// " ++ showa a);
            $comment:("// unboxed EAtom")
          |]
  in return ((inline, if isBoxede e then Yes else No), [])    
    
ccge env e@(EFCall it f eas) =
  case (knownCall it) of
    Nothing -> cstgApplyGeneric env f eas False
    Just kit -> if arity kit == length eas
                then cstgApplyGeneric env f eas False
                else cstgApplyGeneric env f eas False  

ccge env (EPrimop it op eas) =
    let as = map ea eas
        arg0 = ccgUBa env (as !! 0) "i" 
        arg1 = ccgUBa env (as !! 1) "i"
        inline = case op of
                   Piadd -> cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 + $exp:arg1; |]
                   Pisub -> cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 - $exp:arg1; |]
                   Pimul -> cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 * $exp:arg1; |]
                   Pidiv -> cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 / $exp:arg1; |]
                   Pimod -> cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 % $exp:arg1; |]
                   Pieq ->  cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 == $exp:arg1; |]
                   Pine ->  cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 != $exp:arg1; |]
                   Pilt ->  cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 < $exp:arg1; |]
                   Pile ->  cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 <= $exp:arg1; |]
                   Pigt ->  cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 > $exp:arg1; |]
                   Pige ->  cstgCurValUArgType "INT" ++ 
                            [citems| stgCurValU.i = $exp:arg0 >= $exp:arg1; |]
                   Pineg -> cstgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = -$exp:arg0; |]
                   Pimin -> cstgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = imin($exp:arg0,$exp:arg1); |]
                   Pimax -> cstgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = imax($exp:arg0,$exp:arg1); |]
                   _ -> error "Eprimop"
    in return ((inline, No), [])    


ccge env (ELet it os e) =
  let names = map oname os
      decl1 = [ [citem|typename PtrOrLiteral *$id:name; |] | name <- names ] 
      decl2 = [citems|
                typename Cont *contp = stgAllocCallOrStackCont(&it_stgLetCont, $int:(length os));
              |]
      decl3 =  [ [citem| $id:name = &(contp->payload[$int:i]); |] 
                  | (name, i) <- zip names [0..] ]        
      decl4 = [citems| contp->layout = $lint:(npStrToBMInt (replicate (length os) 'P'));|]    
      decl = decl1 ++ decl2 ++ decl3 ++ decl4  
        
      env'  = zip names (map HO names) ++ env
      (decls, buildcodes) = unzip $ map (cbuildHeapObj env') os
  in do
    ofunc <- ccgos env' os
    ((einline, ypn), efunc) <- ccge env' e
    return ((decl ++ concat decls ++ concat buildcodes ++ einline, ypn),
            ofunc ++ efunc)                  

ccge env ecase@(ECase _ e a) =
  do ((ecode, eypn), efunc) <- ccge env e
     -- weird:  compiler requires parens around if, ghci does not
     (if eypn == No then
          ccgeInline env (isBoxede e) (ecode, efunc) a
      else
          ccgeNoInline env (isBoxede e) (ecode, efunc) a)

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
        arg0 = pp . ccgUBa env (as !! 0) -- these take a type indicator
        arg1 = pp . ccgUBa env (as !! 1)
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

ccgeInline = undefined -- TODO

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

ccgaltsInline :: Env -> Alts InfoTab -> Bool
     -> State Int (([BlockItem], YPN), [(Definition, Func)])
ccgaltsInline env a@(Alts it alts name) boxed =
    let contName = "ccont_" ++ name
        scrutPtr = "scrutPtr_" ++ name
        phonyforward = [cedecl| typename FnPtr $id:name();|]
        phonyfun = [cfun| typename FnPtr $id:name() {}|]
        switch = length alts > 1
     in do
       codefuncs <- mapM (ccgalt env switch scrutPtr) alts
       let (codeypns, funcss) = unzip codefuncs
       let (codes, ypns) = unzip codeypns
       let myypn = if all (==Yes) ypns then Yes
                   else if all (==No) ypns then No
                        else Possible
       let in1 = [citems|
                   typename Cont *$id:contName = stgAllocCallOrStackCont(&it_stgStackCont, 1);
                   $comment:("// " ++ show (ctyp it))
                   $id:contName->layout = $lint:(npStrToBMInt (iff boxed "P" "N"));
                |] 
       let in2 = if boxed then 
                  [citems|
                    $id:contName->payload[0] = stgCurVal;
                    stgCurVal.op = NULL;
                  |] 
                else 
                  [citems|$id:contName->payload[0] = stgCurValU;|] 
       let in3 = [citems|
                   typename PtrOrLiteral *$id:scrutPtr = &($id:contName->payload[0]);
                 |] 
       let in4 = if switch then 
                  if boxed then 
                     [citems| 
                       switch(getInfoPtr($id:scrutPtr[0].op)->conFields.tag) {
                         $items:(concat codes)
                       }
                     |] 
                   else
                     [citems|
                       switch(stgCurValU.i) {
                          $items:(concat codes)
                       }
                     |]
                 else
                   [citems| $items:(concat codes)|]
       return ((in1 ++ in2 ++ in3 ++ in4, myypn), (phonyforward, phonyfun) : concat funcss)
       
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


ccgeNoInline :: Env -> Bool
    -> ([BlockItem], [(Definition, Func)])
    -> Alts InfoTab
    -> State Int (([BlockItem], YPN), [(Definition, Func)])
ccgeNoInline env boxed (ecode, efunc) a@(Alts italts alts aname) =
    let contName = "ccont_" ++ aname
        its1 = [citems| 
                $comment:("// scrutinee may STGJUMP or STGRETURN")
                typename Cont *$id:contName = stgAllocCont(&$id:("it_" ++ aname));
                $comment:("// dummy value for scrutinee, InfoTab initializes to unboxed") 
                $id:contName->payload[0].i = 0;
              |]
        its2 = if useArgType then 
                 [citems|$id:contName-> payload[0].argType = "INT"; |]
               else []
        its3 = if fvs italts == [] then
                 [citems| $comment:("// no FVs");|]
               else 
                 [citems| 
                   $comment:("// load payload with FVs" 
                     ++ intercalate " " (map fst $ fvs italts))
                   $items:(cloadPayloadFVs env (map fst $ fvs italts) 1 contName)
                 |]
    in do (acode, afunc) <- ccgalts env a boxed
--        need YPN results from Alts
          return ((its1 ++ its2 ++ its3 ++ ecode ++ acode, Possible),
                  efunc ++ afunc)
      

cgeNoInline :: Env -> Bool
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



-- ADef only or unary sum => no C switch
ccgalts :: Env -> Alts InfoTab -> Bool 
  -> State Int ([BlockItem], [(Definition, Func)])

ccgalts env (Alts it alts name) boxed =
    let contName = "ccont_" ++ name
        fvp = "fvp"
        -- case scrutinee is not current explicitly bound to variable
        altenv = zip (map fst $ fvs it) (map (FP fvp) [1..])
        env' = altenv ++ env
        cforward = [cedecl| typename FnPtr $id:name();|]
        switch = length alts > 1
    in do
      codefuncs <- mapM (ccgalt env' switch fvp) alts
      let (codeypns, funcss) = unzip codefuncs
      let (codes, ypns) = unzip codeypns
      let it1 = [citems|
                  LOG(LOG_INFO, $string:(name ++ " here"));
                  typename Cont *$id:contName = stgGetStackArgp();
                  $comment:("// make self-popping")
                  stgCaseToPopMe($id:contName);
                  typename PtrOrLiteral *$id:fvp = &($id:contName->payload[0]);
                |]
      let it2 = if boxed then 
                  [citems| 
                    $id:fvp[0] = stgCurVal; 
                    $id:contName->layout.bitmap.mask |= 0x1;
                  |]
                else 
                  [citems| $id:fvp[0] = stgCurValU;|]
      let it3 = if switch then 
                  if boxed then 
                    [citems|
                      stgCurVal.op = NULL;
                      switch(getInfoPtr($id:fvp[0].op)->conFields.tag) {
                        $items:(concat codes)
                      }
                   |]
                  else 
                    [citems|
                      switch(stgCurValU.i) {
                       $items:(concat codes)
                      }
                    |]
                else [citems| $items:(concat codes) |] 
      let body = it1 ++ it2 ++ it3         
      let fun = [cfun|
                   typename FnPtr $id:name() {
                     $comment:("//" ++ show (ctyp it) )
                     $items:body
                   }
                |]

      return ([], (cforward, fun) : concat funcss)                

cgalts :: Env -> Alts InfoTab -> Bool
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


ccgalt:: Env -> Bool -> String -> Alt InfoTab
  -> State Int (([BlockItem], YPN), [(Definition, Func)])

ccgalt env switch fvp (ACon it c vs e) =
  let DataCon c' ms = luDCon c (cmap it)
      (_,_,perm) = partPerm isBoxed ms
      eenv = zzip vs (map (FV fvp) perm)
      env' = eenv ++ env
  in do
    ((inline, ypn), func) <- ccge env' e
    let tag = luConTag c $ cmap it -- ConTags returned as Strings!
    let top = [citems|
                 $comment:("// " ++ c ++ " " ++ intercalate " " vs ++ " ->");
               |]
    let its = if ypn /= Yes then 
                inline ++ [citems| STGRETURN0();|]
              else inline
    let extra = if switch then 
                  [citems|
                    case $id:tag: {
                    $items:its
                    }
                  |] 
                else 
                  [citems| $items:its|] 
    return ((top ++ extra, Yes), func)

ccgalt env switch fvp (ADef it v e) =
    let env' = (v, FP fvp 0) : env
    in do
      ((inline, ypn), func) <- ccge env' e
      let top = [citems| $comment:("// " ++ v ++ " ->");|]
      let its = if ypn /= Yes then 
                  inline ++ [citems| STGRETURN0();|]
                else inline
      let extra = if switch then 
                    [citems|
                      default: {
                        $items:its
                      }
                    |] 
                  else 
                    [citems| $items:its|] 
      return ((top ++ extra, Yes), func)

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

cbuildHeapObj :: Env -> Obj InfoTab -> ([BlockItem], [BlockItem])
cbuildHeapObj env o =
    let rval = cbho env o 
        name = oname o
        decl = [citems| $id:name->op = stgNewHeapObj($id:("&it_" ++ name)); |]
        decl2 = if useArgType then 
                  [citems| $id:name->argType = HEAPOBJ; |] 
                else []
                        
    in (decl ++ decl2, rval)


buildHeapObj :: Env -> Obj InfoTab -> (String, String)
-- buildHeapObj :: Env -> Obj InfoTab -> String
buildHeapObj env o =
    let rval = bho env o
        name = oname o
        decl = name ++ "->op = stgNewHeapObj( &it_" ++ name ++ " );\n" ++
               optStr useArgType (name ++ "->argType = HEAPOBJ;\n")
    in (decl, rval)
--    in decl ++ rval

cbho :: Env -> Obj InfoTab -> [BlockItem]
cbho env (FUN it vs e name) = 
  cloadPayloadFVs env (map fst $ fvs it) 0 (name ++ "->op")

cbho env (PAP it f as name) = error "unsupported explicit PAP"
  
cbho env (CON it c as name) =
  [ [citem| $id:name->op->payload[$int:i] = $exp:(fst $ ccga env a); $comment:("// " ++ showa a) |]
    | (i,a) <- indexFrom 0 (projectAtoms as) ]

cbho env (THUNK it e name) =
    let top = [citems| $id:name->op->payload[0].op = NULL; |] ++
              if useArgType then 
                [citems| $id:name->op->payload[0].argType = HEAPOBJ; |] 
              else []
    in top ++ cloadPayloadFVs env (map fst (fvs it)) 1 (name ++ "->op")

cbho env (BLACKHOLE it name) = []


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

cloadPayloadFVs :: Env -> [String] -> Int -> String -> [BlockItem]
cloadPayloadFVs env fvs ind name =
  [ [citem| $id:name->payload[$int:i] = $exp:(fst $ ccgv env v); $comment:("// " ++ v) |] 
    | (i,v) <- indexFrom ind $ fvs]
  
cloadPayloadAtoms :: Env -> [Atom] -> Int -> String -> [BlockItem]
cloadPayloadAtoms env as ind name =
  [ [citem| $id:name->payload[$int:i] = $exp:(fst $ ccga env a); $comment:("// " ++ showa a) |]
    | (i,a) <- indexFrom ind as]

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
