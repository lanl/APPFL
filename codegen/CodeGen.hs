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
  shos
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

--import Data.Loc(noLoc)
import Language.C.Quote.GCC
import Language.C.Syntax(Initializer(..),
                         Definition,
                         Initializer,
                         Exp,
                         BlockItem,
                         InitGroup)

-- use a list of Definitions rather than Func
-- this allows us to put comments before functions
-- using "esc"
type CFun = [Definition]

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

iff b x y = if b then x else y

cgStart :: Definition
cgStart =
  let its = [citems|
              typename Cont *showResultCont = stgAllocCallOrStackCont(&it_stgShowResultCont, 0);
              (void)showResultCont; // suppress warning
            |]
          ++ (if useArgType then [citems| stgCurVal.argType = HEAPOBJ; |] else [])
          ++ [citems|
                stgCurVal.op = &sho_main;
                STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);
             |]
      f = [cfun|
            typename FnPtr start()
            {
               $items:its
            }
          |]
  in [cedecl| $func:f |]

cgMain :: Bool -> Definition
cgMain v =
  let its = [citems|
              typename clock_t start_t = clock();
              startCheck();
              parseArgs(argc, argv);
              initStg();
              initGc();
              CALL0_0(start);
              perfCounter.totalTime = (double)(clock() - start_t) / CLOCKS_PER_SEC;
            |]
          ++ (if v then
                [citems|
                  showStgHeap(LOG_DEBUG);
                |]
              else [])
          ++ [citems|showPerfCounters(LOG_NONE);|]
          ++ [citems|return 0;|]
      fun = [cfun|
               int main (int argc, char **argv)
               {
                 $items:its
               }
            |]
  in [cedecl| $func:fun |]

registerSOs :: [Obj InfoTab] -> (Definition, CFun)
registerSOs objs =
  let proto = [cedecl| void registerSOs(); |]
      its = [ [citem|stgStatObj[stgStatObjCount++] = &$id:s; |] | s <- shoNames objs]
      f = [cfun| void registerSOs() { $items:its } |]
  in (proto, [[cedecl|$func:f|]])

shos :: [Obj InfoTab] -> (Definition, Definition)
shos objs =
    let names = shoNames objs
        inits = [[cinit| &$id:name |] | name <- names ]
        compoundInit = [cinit| { $inits:inits } |]
    -- const int stgStatObjCount = #static objects;
    -- Obj *const stgStatObj[#static objects] = {&obj, &obj, ... } ;
    in ([cedecl| const int stgStatObjCount = $exp:(length names) ; |] ,
        [cedecl| typename Obj *const stgStatObj [ $exp:(length names) ] =
                   $init:compoundInit ; |])

listLookup k [] = Nothing
listLookup k ((k',v):xs) | k == k' = Just v
                         | otherwise = listLookup k xs

getEnvRef :: String -> Env -> Exp
getEnvRef v kvs =
  case listLookup v kvs of
    Nothing -> error $ "getEnvRef " ++ v ++ " failed"
    Just k ->
        case k of
          SO       -> [cexp| HOTOPL(&$id:("sho_" ++ v)) |]
          HO name  -> [cexp| (*$id:name) |]
          FP fp i  -> [cexp| $id:fp[$int:i] |]
          FV fpp i -> [cexp| $id:fpp->op->payload[$int:i] |]


-- 2nd arg is comment to attach to statement
cga :: Env -> Atom -> (Exp, String)
cga env (Var v) =  cgv env v

cga env (LitI i) = (
  if useArgType then
    [cexp| ((typename PtrOrLiteral){.argType = INT, .i = $int:i}) |]
  else
    [cexp| ((typename PtrOrLiteral){.i = $int:i}) |], "")


cga env (LitL l) = (
  if useArgType then
    [cexp| ((typename PtrOrLiteral){.argType = LONG, .l = $lint:l}) |]
  else
    [cexp| ((typename PtrOrLiteral){.l = $lint:l}) |], "")

cga env (LitF f) =
  let f' = toRational f
      e = if useArgType then
            [cexp| ((typename PtrOrLiteral){.argType = FLOAT, .f = $float:f'}) |]
          else
            [cexp| ((typename PtrOrLiteral){.f = $float:f'}) |]
  in (e, "")


cga env (LitD d) =
  let d' = toRational d
      e = if useArgType then
            [cexp| ((typename PtrOrLiteral){.argType = DOUBLE, .d = $double:d'}) |]
          else
            [cexp| ((typename PtrOrLiteral){.d = $double:d'}) |]
  in (e, "")

cga env (LitC c) =
  let c' = "con_" ++ c
      e = if useArgType then
            [cexp| ((typename PtrOrLiteral){.argType = INT, .i = $id:c'}) |]
          else
            [cexp| ((typename PtrOrLiteral){.i = $id:c'}) |]
  in (e, "")

cga env (LitStr s) = error "TODO"

cgv :: Env -> String -> (Exp, String)
cgv env v = (getEnvRef v env, "// " ++ v)

-- boxed expression predicate
isBoxede e = isBoxed $ typ $ emd e

cgUBa :: Env -> Atom -> String -> Exp
cgUBa env (Var v)  t   = [cexp| ($exp:(fst $ cgv env v)).$id:t |]
cgUBa env (LitI i) "i" = [cexp| $int:i |]
cgUBa env (LitD d) "d" = [cexp| $double:(toRational d) |]
cgUBa _ at _ = error $ "CodeGen.cgUBa: not expecting Atom - " ++ show at


-- CG in the state monad ***************************************************
-- CG of objects produces no inline code
-- THUNK produces a function, other OBJ types have single global function
--    (ignore explicit PAP for now)
-- FUN produces a function (not object)
--   all objects produce a (S)HO
-- for CG, objects are heap allocated only by let

data YPN = Yes | Possible | No -- could use Maybe Bool but seems obscure
           deriving(Eq, Show)


cgObjs :: [Obj InfoTab] -> [String] -> ([Definition], [CFun])
cgObjs objs runtimeGlobals =
   let tlnames = runtimeGlobals ++ map (name . omd) objs
       env = zip tlnames $ repeat SO
       (funcs, _) = runState (cgos env objs) 0
       (forwards, funs) = unzip funcs
--       (forward, fun) = registerSOs objs
--   in (forward:forwards, fun:funs)
   in (forwards, funs)

cgos :: Env -> [Obj InfoTab] -> State Int [(Definition,  CFun)]
cgos env = concatMapM (cgo env)

cgo :: Env -> Obj InfoTab -> State Int [(Definition, CFun)]
cgo env o@(FUN it vs e name) =
    let cforward = [cedecl| typename FnPtr $id:("fun_" ++ name)();|]
        argp = "argp" -- also free variable pointer pointer
        fps = "self":vs
        env' = zip (map fst $ fvs it) (map (FV argp) [0..]) ++
               zip fps (map (FP argp) [0..]) ++
               env
    in do
      ((inline, ypn), funcs) <- cge env' e
      let comm = [cedecl|$esc:("\n// " ++ show (ctyp it) ++ "\n" ++
                           "// " ++ name ++ "(self, " ++
                           intercalate ", " vs ++ ")"  ) |]
          top = [citems|
                  LOG(LOG_INFO, $string:(name ++ " here\n"));
                  typename PtrOrLiteral *$id:argp = &(stgGetStackArgp()->payload[0]);
                |]
          bot = [citems| STGRETURN0();|]
          items = if ypn /= Yes then top ++ inline ++ bot else top ++ inline
          f = [cfun|
                typename FnPtr $id:("fun_" ++ name)()
                {
                  $items:items
                }
              |]
          cfunc = [comm, [cedecl|$func:f|]]
      return $ (cforward, cfunc) : funcs

cgo env (PAP it f as name) = return []

cgo env (CON it c as name) = return []

cgo env o@(THUNK it e name) =
  let fvpp = "fvpp"
      env' = zip (map fst $ fvs it) (map (FV fvpp) [1..]) ++ env
      cforward = [cedecl| typename FnPtr $id:("thunk_" ++ name)();|]
  in do
    ((inline,ypn), funcs) <- cge env' e
    let comm = [cedecl|$esc:("\n// " ++ show (ctyp it))|]
        top = [citems|
                LOG(LOG_INFO, $string:(name ++ " here7\n"));
                $comment:("// access free vars through frame pointer for GC safety")
                $comment:("// is this really necessary???");
                typename Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);
                stg_fp->layout = (typename Bitmap64)$ulint:(npStrToBMInt "P");
                stg_fp->payload[0] = stgCurVal;
                typename PtrOrLiteral *$id:fvpp = &(stg_fp->payload[0]);
                (void)$id:fvpp; // suppress warning
                stgThunk(stgCurVal);
                stgCurVal.op = NULL;
              |]
        bot = [citems| STGRETURN0();|]
        items = if ypn /= Yes then top ++ inline ++ bot else top ++ inline
        f = [cfun|
              typename FnPtr $id:("thunk_" ++ name)()
              {
                $items:items
              }
            |]
        cfunc = [comm, [cedecl|$func:f|]]
    return $ (cforward, cfunc) : funcs

--BH cgo env (BLACKHOLE {}) = return []


stgApplyGeneric env f eas direct =
    let as = map ea eas
        pnstring = [ if b then 'P' else 'N' | b <- map (isBoxed . typ . emd) eas ]
        -- HACK
        f' = if f == "stg_case_not_exhaustive" then
                 f ++ pnstring
             else f
        (expr0, comm0) = cga [] (LitI 0)
        (expr1, comm1) = cgv env f'
        its = [citems|
                typename Cont *cp = stgAllocCallOrStackCont( &it_stgStackCont,
                $int:(length pnstring + 2));
                cp->layout = (typename Bitmap64)$ulint:(npStrToBMInt ('N' : 'P' : pnstring ));
                $comment:comm0
                cp->payload[ 0 ] = $exp:(expr0);
                $comment:comm1
                cp->payload[ 1 ] = $exp:(expr1);
              |]
            ++ [ [citem| cp->payload[$int:i] = $exp:(expr); $comment:(comm) |]
                  | (i,a) <- zip [2..] as, let (expr, comm) = cga env a ]
            ++ (if direct then
                 [citems|
                   $comment:("// DIRECT TAIL CALL " ++ f ++ " " ++ showas as)
                   if (evalStrategy == STRICT1) stgEvalStackFrameArgs(cp);
                   STGJUMP0($id:("fun_" ++ f));
                 |]
               else
                 [citems|
                   $comment:("// INDIRECT TAIL CALL " ++ f' ++ " " ++ showas as)
                   STGJUMP0(stgApplyNew);
                 |])
    in return ((its, Yes), [])

stgCurValUArgType :: String -> [BlockItem]
stgCurValUArgType ty = if useArgType
                           then [citems| stgCurValU.argType = $id:ty; |]
                           else []

cge :: Env
  -> Expr InfoTab
  -> State Int (([BlockItem], YPN), [(Definition, CFun)])
cge env e@(EAtom it a) =
  let (expr, comm) = cga env a
      inline =
        if isBoxede e then
          [citems|
            $comment:comm
            stgCurVal = $exp:expr;
            $comment:("// boxed EAtom, stgCurVal updates itself")
            STGJUMP();
          |]
        else
          [citems|
            $comment:("// " ++ showa a)
            stgCurValU = $exp:expr;
            $comment:("// unboxed EAtom")
          |]
  in return ((inline, if isBoxede e then Yes else No), [])

cge env e@(EFCall it f eas) =
  case (knownCall it) of
    Nothing -> stgApplyGeneric env f eas False
    Just kit -> if arity kit == length eas
                then stgApplyGeneric env f eas False  -- disabled was True
                else stgApplyGeneric env f eas False


cge env (EPrimop it op eas) =
    let as = map ea eas
        arg0 = cgUBa env (as !! 0) "i"
        arg1 = cgUBa env (as !! 1) "i"
        inline = case op of
                   Piadd -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 + $exp:arg1; |]
                   Pisub -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 - $exp:arg1; |]
                   Pimul -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 * $exp:arg1; |]
                   Pidiv -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 / $exp:arg1; |]
                   Pimod -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 % $exp:arg1; |]
                   Pieq ->  stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 == $exp:arg1; |]
                   Pine ->  stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 != $exp:arg1; |]
                   Pilt ->  stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 < $exp:arg1; |]
                   Pile ->  stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 <= $exp:arg1; |]
                   Pigt ->  stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 > $exp:arg1; |]
                   Pige ->  stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 >= $exp:arg1; |]
                   Pineg -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = -$exp:arg0; |]
                   Pimin -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = imin($exp:arg0,$exp:arg1); |]
                   Pimax -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = imax($exp:arg0,$exp:arg1); |]
                   _ -> error "Eprimop"
    in return ((inline, No), [])

cge env (EPrimOp it op ty eas) =
    let as = map ea eas
        f = case ty of
              Pint -> "i"
              Pdouble -> "d"
        arg0 = cgUBa env (as !! 0) f
        arg1 = cgUBa env (as !! 1) f
        inline = case ty of
                   Pint -> case op of
                     Padd -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 + $exp:arg1; |]
                     Psub -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 - $exp:arg1; |]
                     Pmul -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 * $exp:arg1; |]
                     Pdiv -> stgCurValUArgType "INT" ++
                            [citems| stgCurValU.i = $exp:arg0 / $exp:arg1; |]
                   Pdouble -> case op of
                     Padd -> stgCurValUArgType "DOUBLE" ++
                           [citems| stgCurValU.d = $exp:arg0 + $exp:arg1; |]
                     Psub -> stgCurValUArgType "DOUBLE" ++
                           [citems| stgCurValU.d = $exp:arg0 - $exp:arg1; |]
                     Pmul -> stgCurValUArgType "DOUBLE" ++
                           [citems| stgCurValU.d = $exp:arg0 * $exp:arg1; |]
                     Pdiv -> stgCurValUArgType "DOUBLE" ++
                           [citems| stgCurValU.d = $exp:arg0 / $exp:arg1; |]
    in return ((inline, No), [])

cge env (ELet it os e) =
  let names = map oname os
      decl1 = [ [citem|typename PtrOrLiteral *$id:name; |] | name <- names ]
      decl2 = [citems|
                typename Cont *contp = stgAllocCallOrStackCont(&it_stgLetCont, $int:(length os));
              |]
            ++ [ [citem| $id:name = &(contp->payload[$int:i]); |]
                  | (name, i) <- zip names [0..] ]
            ++ [citems|
                 contp->layout = (typename Bitmap64)$ulint:(npStrToBMInt (replicate (length os) 'P'));
               |]
      decl = decl1 ++ [citems| { $items:decl2 } |]

      env'  = zip names (map HO names) ++ env
      (decls, buildcodes) = unzip $ map (buildHeapObj env') os
  in do
    ofunc <- cgos env' os
    ((einline, ypn), efunc) <- cge env' e
    return ((decl ++ concat decls ++ concat buildcodes ++ einline, ypn),
            ofunc ++ efunc)

cge env ecase@(ECase _ e a) =
  do ((ecode, eypn), efunc) <- cge env e
     -- weird:  compiler requires parens around if, ghci does not
     (if eypn == No then
          cgeInline env (isBoxede e) (ecode, efunc) a
      else
          cgeNoInline env (isBoxede e) (ecode, efunc) a)


cgeInline :: Env -> Bool
     -> ([BlockItem], [(Definition, CFun)])
     -> Alts InfoTab
     -> State Int (([BlockItem], YPN), [(Definition, CFun)])
cgeInline env boxed (ecode, efunc) a@(Alts{}) =
  let pre = [citems| $comment:("// inline:  scrutinee does not STGJUMP or STGRETURN"); |]
  in do
    ((acode, ypn), afunc) <- cgaltsInline env a boxed
    return ((pre ++ ecode ++ acode, ypn),
            efunc ++ afunc)


cgaltsInline :: Env -> Alts InfoTab -> Bool
     -> State Int (([BlockItem], YPN), [(Definition, CFun)])
cgaltsInline env a@(Alts it alts name scrt) boxed =
    let contName = "ccont_" ++ name
        scrutPtr = "scrutPtr_" ++ name
        phonyforward = [cedecl| typename FnPtr $id:name();|]
        f = [cfun| typename FnPtr $id:name() {}|]
        phonyfun = [[cedecl|$func:f|]]
        switch = length alts > 1
     in do
       codefuncs <- mapM (cgalt env switch scrutPtr) alts
       let (codeypns, funcss) = unzip codefuncs
       let (codes, ypns) = unzip codeypns
       let myypn | all (== Yes) ypns = Yes
                 | all (== No) ypns = No
                 | otherwise = Possible
       let its = [citems|
                   typename Cont *$id:contName =
                     stgAllocCallOrStackCont(&it_stgStackCont, 1);
                   $comment:("// " ++ show (ctyp it))
                   $id:contName->layout =
                     (typename Bitmap64)$ulint:(npStrToBMInt (iff boxed "P" "N"));
                |]
               ++ (if boxed then
                    [citems|
                      $id:contName->payload[0] = stgCurVal;
                      stgCurVal.op = NULL;
                    |]
                  else
                    [citems|$id:contName->payload[0] = stgCurValU;|])
               ++ [citems|
                    typename PtrOrLiteral *$id:scrutPtr =
                      &($id:contName->payload[0]);
                    (void)$id:scrutPtr; // suppress warning
                  |]
               ++ (if switch then
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
                     [citems| $items:(concat codes)|])
       return ((its, myypn), (phonyforward, phonyfun) : concat funcss)

cgeNoInline :: Env -> Bool
    -> ([BlockItem], [(Definition, CFun)])
    -> Alts InfoTab
    -> State Int (([BlockItem], YPN), [(Definition, CFun)])
cgeNoInline env boxed (ecode, efunc) a@(Alts italts alts aname scrt) =
    let contName = "ccont_" ++ aname
        its = [citems|
                $comment:("// scrutinee may STGJUMP or STGRETURN")
                typename Cont *$id:contName = stgAllocCont(&$id:("it_" ++ aname));
                $comment:("// dummy value for scrutinee, InfoTab initializes to unboxed")
                $id:contName->payload[0].i = 0;
              |]
            ++ (if useArgType then
                  [citems|$id:contName-> payload[0].argType = INT; |]
                else [])
            ++ (if fvs italts == [] then
                  [citems| $comment:("// no FVs");|]
                else
                  [citems|
                    $comment:("// load payload with FVs"
                      ++ intercalate " " (map fst $ fvs italts))
                    $items:(loadPayloadFVs env (map fst $ fvs italts) 1 contName)
                  |])
    in do (acode, afunc) <- cgalts env a boxed
--        need YPN results from Alts
          return ((its ++ ecode ++ acode, Possible),
                  efunc ++ afunc)


-- ADef only or unary sum => no C switch
cgalts :: Env -> Alts InfoTab -> Bool
  -> State Int ([BlockItem], [(Definition, CFun)])

cgalts env (Alts it alts name scrt) boxed =
    let contName = "ccont_" ++ name
        fvp = "fvp"
        -- scrutinee now has a name in the environment
        -- can I do more with it?
        -- e.g. Should fvp[0] be bound to a named PtrOrLiteral? (and used in its place)
        altenv = zip (scrtVarName scrt : (map fst $ fvs it)) (map (FP fvp) [0..])
        env' = altenv ++ env
        cforward = [cedecl| typename FnPtr $id:name();|]
        switch = length alts > 1
    in do
      codefuncs <- mapM (cgalt env' switch fvp) alts
      let (codeypns, funcss) = unzip codefuncs
      let (codes, ypns) = unzip codeypns
      let its = [citems|
                  LOG(LOG_INFO, $string:(name ++ " here\n"));
                  typename Cont *$id:contName = stgGetStackArgp();
                  $comment:("// make self-popping")
                  stgCaseToPopMe($id:contName);
                  typename PtrOrLiteral *$id:fvp = &($id:contName->payload[0]);
                |]
              ++ (if boxed then
                   [citems|
                      $id:fvp[0] = stgCurVal;
                      $id:contName->layout.bitmap.mask |= 0x1;
                    |]
                  else
                    [citems| $id:fvp[0] = stgCurValU;|])
              ++ (if switch then
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
                  else [citems| $items:(concat codes) |])
      let comm = [cedecl| $esc:("\n//" ++ show (ctyp it))|]
      let fun = [cfun|
                   typename FnPtr $id:name()
                   {
                     $items:its
                   }
                |]
      let cfunc = [comm, [cedecl| $func:fun |]]
      return ([], (cforward, cfunc) : concat funcss)

cgalt:: Env -> Bool -> String -> Alt InfoTab
  -> State Int (([BlockItem], YPN), [(Definition, CFun)])
cgalt env switch fvp (ACon it c vs e) =
  let DataCon c' ms = luDCon c (cmap it)
      (_,_,perm) = partPerm isBoxed ms
      eenv = zzip vs (map (FV fvp) perm)
      env' = eenv ++ env
  in do
    ((inline, ypn), func) <- cge env' e
    let tag = luConTag c $ cmap it -- ConTags returned as Strings!
    let its = if ypn /= Yes then
                inline ++ [citems| STGRETURN0();|]
              else inline
    let casei = if switch then
                  [citems|
                    case $id:tag: $comment:("// " ++ c ++ " " ++ intercalate " " vs ++ " ->")
                    {
                      $items:its
                    }
                  |]
                else
                  [citems|$items:its|]
    return ((casei, Yes), func)

cgalt env switch fvp (ADef it v e) =
    let env' = (v, FP fvp 0) : env
    in do
      ((inline, ypn), func) <- cge env' e
      let its = if ypn /= Yes then
                  inline ++ [citems| STGRETURN0();|]
                else inline
      let casei = if switch then
                    [citems|
                      default: $comment:("// " ++ v ++ " ->") {
                        $items:its
                      }
                    |]
                  else
                    [citems| $items:its|]
      return ((casei, Yes), func)

-- ****************************************************************
-- buildHeapObj is only invoked by ELet so TLDs not built

buildHeapObj :: Env -> Obj InfoTab -> ([BlockItem], [BlockItem])
buildHeapObj env o =
    let rval = bho env o
        name = oname o
        decl = [citems| $id:name->op = stgNewHeapObj($id:("&it_" ++ name)); |]
        decl2 = if useArgType then
                  [citems| $id:name->argType = HEAPOBJ; |]
                else []

    in (decl ++ decl2, rval)


bho :: Env -> Obj InfoTab -> [BlockItem]
bho env (FUN it vs e name) =
  loadPayloadFVs env (map fst $ fvs it) 0 (name ++ "->op")

bho env (PAP it f as name) = error "unsupported explicit PAP"

bho env (CON it c as name) =
  [ [citem| $comment:("// " ++ showa a)
            $id:name->op->payload[$int:i] = $exp:(fst $ cga env a);
    |] | (i,a) <- indexFrom 0 (projectAtoms as) ]

bho env (THUNK it e name) =
    let top = [citems| $id:name->op->payload[0].op = NULL; |] ++
              if useArgType then
                [citems| $id:name->op->payload[0].argType = HEAPOBJ; |]
              else []
    in top ++ loadPayloadFVs env (map fst (fvs it)) 1 (name ++ "->op")

--BH bho env (BLACKHOLE it name) = []

loadPayloadFVs :: Env -> [String] -> Int -> String -> [BlockItem]
loadPayloadFVs env fvs ind name =
  [ [citem| $comment:("// " ++ v)
            $id:name->payload[$int:i] = $exp:(fst $ cgv env v);
          |] | (i,v) <- indexFrom ind fvs]

loadPayloadAtoms :: Env -> [Atom] -> Int -> String -> [BlockItem]
loadPayloadAtoms env as ind name =
  [ [citem| $comment:("// " ++ showa a)
            $id:name->payload[$int:i] = $exp:(fst $ cga env a);
          |] | (i,a) <- indexFrom ind as]

showas :: [Atom] -> String
showas as = unwords $ map showa as

showa (Var  v) = v
showa (LitI i) = show i
showa (LitL l) = show l
showa (LitF f) = show f
showa (LitD d) = show d
showa (LitC c) = "con_" ++ c
showa (LitStr s) = s
-- showa at = error $ "CodeGen.showa: not expecting Atom - " ++ show at

indexFrom :: Int -> [a] -> [(Int, a)]
indexFrom i xs = zip [i..] xs
