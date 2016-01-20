{-# LANGUAGE CPP #-}
#include "../options.h"

import Util
import Data.List(intercalate)
import System.Environment(getExecutablePath)
import Data.List.Split
import STGbits

data Strictness = Nonstrict
                | Strict1   -- evaluate args first, then fun
                  deriving(Eq)

main = do
         binaryPath <- getExecutablePath
         let binaryDir = intercalate "/" $ init $ splitOn "/" binaryPath
         mapM (dumpStgApply 6 binaryDir) [Nonstrict, Strict1]

name strictness =
    case strictness of
      Nonstrict -> "nonstrict"
      Strict1 -> "strict"

dumpStgApply n binDir strictness =
    let (forward, macros, fun) = genAllstgApply strictness n
    in do writeFile (binDir ++ "/../include/stgApply.h") (includehtop ++ forward ++ macros ++ includehbot)
          writeFile (binDir ++ "/../stgApply/" ++ name strictness ++ ".c") (includec ++ fun)
          return ()
        where
          includehtop =
              "#ifndef stgApply_h\n" ++
              "#define stgApply_h\n" ++
              "#include \"stg.h\"\n"
          includehbot =
              "#endif\n"
          includec =
              "#include \"stg.h\"\n" ++
              "#include \"cmm.h\"\n" ++
              "#include \"stgutils.h\"\n" ++
              "#include \"stgApply.h\"\n" ++
              "#include <stdlib.h>\n" ++
              "#include <stdio.h>\n\n"

genAlldebug strictness n =
    let (forwards, macros, funs) = unzip3 $ map (genN strictness) [1..n]
    in putStrLn $ concat forwards ++ "\n\n" ++ concat funs

genAllstgApply strictness n =
    let (forwards, macros, funs) = unzip3 $ map (genN strictness) [1..n]
    in (concat forwards, concat macros, concat funs)

genN strictness n =
    let (forwards, macros, funs) = unzip3 $ map (gen strictness) (pns n)
    in (concat forwards, concat macros, concat funs)

pns n | n == 0 = [""]
      | n > 0 = pref $ pns (n-1)
      where
        pref [] = []
        pref (x:xs) = ('N':x):('P':x):pref xs
pns _ = error "invalid param in pns"

optSwitch scrut lo hi f =
  if lo > hi then
      "// empty switch(" ++ scrut ++ ") range [" ++ show lo ++ ".." ++ show hi ++ "]\n" else
  if lo == hi then
      "// " ++ scrut ++ " can only be " ++ show lo ++ "\n" ++
      f lo
  else
      "switch (" ++ scrut ++ ") {\n" ++
      concat ["case " ++ show c ++ ": {\n" ++
                 indent 2 (f c) ++
              "  break;\n" ++
              "} // case " ++ show c ++ "\n"
              | c <- [lo..hi]] ++
      "  default: fprintf(stderr, \"switch on " ++ scrut ++
                                   " reached default!\\n\"); exit(0);\n" ++
      "} // switch(" ++ scrut ++ ")\n"

-- debugc code =
--     "#ifdef DEBUGSTGAPPLY\n" ++
--     code ++
--     "#endif\n"

debugp (x:xs) =
#ifdef DEBUGSTGAPPLY
    "fprintf(stderr, \"" ++ x ++ "\"" ++ concatMap (", " ++) xs ++ ");\n"
#else
    ""
#endif
debugp [] = ""


debugc code =
    "#ifdef DEBUGSTGAPPLY\n" ++
    code ++
    "#endif\n"

-- npstring is args, which start at argv[1]
evalps npstring =
    concat [ "STGEVAL(argv[" ++ show (i+1) ++ "]);\n" ++
             "// stgCurVal possibly less indirect\n" ++
             "argv[" ++ show (i+1) ++ "] = stgCurVal;\n"
             | i <- [0..(length npstring - 1)], npstring!!i == 'P' ]

gen strictness npstring =
  (forward, macro, fun)
  where
    argc = length npstring
    fname = "stgApply" ++ npstring
    forward = "FnPtr " ++ fname ++ "();\n"
    arglist = 'f' : concat [',':'v':show i | i <- [1..argc]]
    macro = ""
    fun = "FnPtr " ++ fname ++ "() {\n" ++ indent 2 body ++ "}\n\n"
    body =
     "// STACKCONT with actual parameters\n" ++
     "Cont *argframe = stgGetStackArgp();\n" ++
     "// continuations don't move in GC\n" ++
     "PtrOrLiteral * const argv = argframe->payload;\n" ++
     "// STACKCONT to be constructed for call/jump\n" ++
     "Cont *newframe;\n" ++
     "// pointer to args in argframe\n" ++
     "const int argc = " ++ show argc ++ ";\n" ++
      debugp [fname ++ " %s\\n", "getInfoPtr(argv[0].op)->name"] ++

     (if strictness == Strict1 then evalps npstring else "") ++

     "argv[0].op = derefPoL(argv[0]);\n" ++
     "// this if just saves a possibly unneeded call\n" ++
     "if (getObjType(argv[0].op) == THUNK) {\n" ++
        indent 2 (debugp [fname ++ " THUNK\\n"]) ++
     "  STGEVAL(argv[0]);\n" ++
     "  // this works because stgCurVal is a GC root\n" ++
     "  argv[0].op = derefPoL(stgCurVal);\n" ++
     "} // if THUNK\n\n" ++

     "switch (getObjType(argv[0].op)) {\n\n" ++

     "case FUN: {\n" ++
     "  int arity = getInfoPtr(argv[0].op)->funFields.arity;\n" ++
        indent 2 (debugp ["FUN %s arity %d\\n",
                          "getInfoPtr(argv[0].op)->name",
                          "getInfoPtr(argv[0].op)->funFields.arity"]) ++
     "  int excess = argc - arity;  // may be negative\n\n" ++

     (if argc == 1 then
        "  // too many args not possible\n"
      else
        "  // too many args?\n" ++
        "  if (excess > 0) {\n" ++
             indent 4 (debugp [fname ++ " FUN too many args\\n"]) ++
             indent 4 (optSwitch "excess"
                                 1
                                 (argc-1)
                                 (\excess -> funpos npstring excess)) ++
        "  } else \n") ++
     "\n" ++
     "  // just right?\n" ++
     "  if (excess == 0) {\n" ++
          indent 4 (debugp [fname ++ " FUN just right\\n"]) ++
          indent 4 (funeq npstring) ++
     "  }\n" ++
     "  // excess < 0, too few args\n" ++
     "  else {\n" ++
          indent 4 (debugp [fname ++ " FUN too few args\\n"]) ++
          indent 4 (funneg npstring) ++
     "  } // if excess\n" ++
     "} // case FUN\n" ++
     "\n\n" ++

     "case PAP: {\n" ++
     "  int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount + \n" ++
     "                getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;\n" ++
     "  Bitmap64 bitmap = argv[0].op->payload[fvCount].b;\n" ++
     "  Bitmap64 bitmap2;\n" ++
     "  int argCount = BMSIZE(bitmap);\n" ++
     "  int arity = getInfoPtr(argv[0].op)->funFields.arity - argCount;\n" ++
        indent 2 (debugp ["PAP/FUN %s arity %d\\n",
                          "getInfoPtr(argv[0].op)->name",
                          "getInfoPtr(argv[0].op)->funFields.arity"]) ++
     "  int excess = argc - arity;    // may be negative\n" ++
     "\n" ++
     (if argc == 1 then
        "  // too many args not possible\n"
      else
        "  // too many args?\n" ++
        "  if (excess > 0) {\n" ++
             indent 4 (debugp [fname ++ " PAP too many args\\n"]) ++
             indent 4 (optSwitch "excess"
                                 1
                                 (argc-1)
                                 (\excess -> pappos npstring excess)) ++
        "  } else \n") ++
     "\n" ++
     "  // just right?\n" ++
     "  if (excess == 0) {\n" ++
          indent 4 (debugp [fname ++ " PAP just right\\n"]) ++
          indent 4 (papeq npstring) ++
     "\n" ++
     "  // excess < 0, too few args\n" ++
     "  } else {\n" ++
          indent 4 (debugp [fname ++ " PAP too few args\\n"]) ++
          indent 4 (papneg npstring) ++
     "  } // if excess\n" ++
     "} // case PAP\n" ++
     "\n" ++

     "case BLACKHOLE: {\n" ++
     "  fprintf(stderr, \"infinite loop detected in " ++ fname ++ "!\\n\");\n" ++
     "  showStgHeap();\n" ++
     "  assert(0);\n" ++
     "} // case BLACKHOLE\n" ++

     "\n" ++
     "default:\n" ++
     "  fprintf(stderr, \"" ++ fname ++ " not a THUNK, FUN, or PAP\\n\");\n" ++
     "  exit(0);\n" ++
     "}  // switch\n"

funpos npstring excess =
  let arity = (length npstring) - excess
  in debugp ["stgApply FUN " ++ show excess ++ " excess args\\n"] ++

     "// arity args\n" ++
     "newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1+arity );\n" ++
     "newframe->layout = " ++ npStrToBMStr ('P' : take arity npstring) ++ ";\n" ++
     "memcpy(newframe->payload, argv, (1+arity) * sizeof(PtrOrLiteral));\n" ++
     "// call-with-return the FUN\n" ++
      debugp [ "stgApply" ++ npstring ++ " CALLing " ++ " %s\\n",
               "getInfoPtr(argv[0].op)->name"] ++
     "STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);\n" ++
      debugp [ "stgApply" ++ npstring ++ " back from CALLing " ++ " %s\\n",
               "getInfoPtr(argv[0].op)->name"] ++
     "argv[0] = stgCurVal;\n" ++

     "// excess args\n" ++
     "newframe = stgAllocCallOrStackCont( &it_stgStackCont, 1+excess );\n" ++
     "newframe->layout = " ++ npStrToBMStr ('P' : drop arity npstring) ++ ";\n" ++
     "newframe->payload[0] = argv[0];" ++
     "memcpy(&newframe->payload[1],\n" ++
     "       &argv[1+arity],\n" ++
     "       excess * sizeof(PtrOrLiteral));\n" ++
     "// try again - tail call stgApply\n" ++
     "// jump means replace stgApply stack frame with new\n" ++
     "stgJumpAdjust();\n" ++
     "STGJUMP0(stgApply" ++ drop arity npstring  ++ ");\n"

funeq npstring =
  "newframe = stgAllocCallOrStackCont( &it_stgStackCont, argc+1 );\n" ++
  "newframe->layout = " ++ npStrToBMStr ('P' : npstring) ++ ";\n" ++
  "memcpy(newframe->payload, argv, (argc+1) * sizeof(PtrOrLiteral));\n" ++
  "// stgJumpAdjust invalidates argv and newframe\n" ++
  "newframe = stgJumpAdjust();\n" ++
  "STGJUMP0(getInfoPtr(newframe->payload[0].op)->funFields.trueEntryCode);\n"

funneg npstring =
  let arity = length npstring
  in "int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount + \n" ++
     "              getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;\n" ++
     "// stgNewHeapPAPmask puts layout info at payload[fvCount]\n" ++
     "Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), " ++
                 npStrToBMStr npstring ++ ");\n" ++
     "// copy fvs\n" ++
     debugp ["stgApply FUN inserting %d FVs into new PAP\\n", "fvCount"] ++
     "copyargs(&pap->payload[0], &argv[0].op->payload[0], fvCount);\n" ++
     "// copy args to just after fvs and layout info\n" ++
     debugp ["stgApply FUN inserting " ++ show arity ++ " args into new PAP\\n"] ++
     "copyargs(&pap->payload[fvCount+1], &argv[1], " ++ show arity ++ ");\n" ++
     "stgCurVal = HOTOPL(pap);\n" ++
     "// pop stgApply cont\n" ++
     "stgPopCont();\n" ++
     "STGRETURN0();\n"

{-
typedef struct Bitmap64proto {
  uintptr_t mask : 58;
  unsigned int size : 6;
} Bitmap64proto;

typedef union Bitmap64 {
  uintptr_t bits;
  Bitmap64proto bitmap;
} Bitmap64;

define BMSIZE(bm) (bm.bitmap.size)
define BMMAP(bm) (bm.bitmap.mask)
-}

-- C argCount = number of args already in PAP
-- C arity
-- C excess
-- C bitmap
-- C fvCount
pappos npstring excess =
  let arity = length npstring - excess -- effective arity of PAP
  in debugp ["stgApply PAP to " ++ show excess ++ " excess args\\n"] ++
     "// stash excess args\n" ++
--     callContArgvSave (arity+1) (drop arity npstring) ++  -- PAP at index 0
     -- new STACKFRAME
--   1.  push needed new args
--   2.  push args already in pap from PAP[fvCount+1...]
--     "// push needed args\n" ++
--     "pushargs(arity, &argv[1]);\n" ++
--     "// push args already in PAP\n" ++
--     "pushargs(argCount, &argv[0].op->payload[fvCount+1]);\n" ++
--     "// push self\n" ++
--     "pushargs(1, argv);\n" ++

     -- 1.  shift bitmap left by one for slot for FUN-like obj
     -- 2.  add bit for FUN-like obj
     -- 4.  update size
     -- 3.  add bits for new args
     "bitmap.bitmap.mask <<= 1;\n" ++
     "bitmap.bitmap.mask |= 0x1;\n" ++
     "bitmap.bitmap.size += 1;\n" ++
     "bitmap2 = " ++ npStrToBMStr (take arity npstring) ++ ";\n" ++
     "bitmap2.bitmap.mask <<= (argCount + 1);\n" ++
     "bitmap.bits += bitmap2.bits;\n" ++

     "newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1+argCount+arity );\n" ++
     "newframe->layout = bitmap;\n" ++
     "newframe->payload[0] = argv[0]; // self\n" ++
     "memcpy(&newframe->payload[1], " ++
            "&argv[0].op->payload[1 + fvCount], " ++
            "argCount * sizeof(PtrOrLiteral)); // old args\n" ++
     "memcpy(&newframe->payload[1 + argCount], " ++
            "&argv[1], " ++
            "arity * sizeof(PtrOrLiteral));\n" ++

     "// call-with-return the FUN-oid\n" ++
     "STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);\n" ++
--     "// pop newframe\n" ++
--     "stgPopCont();\n" ++

     "// stash the FUN-oid\n" ++
     "argv[0] = stgCurVal;\n" ++
     "newframe = stgAllocCallOrStackCont(&it_stgStackCont, 1 + excess);\n" ++
     "newframe->layout = " ++ npStrToBMStr ('P' : drop arity npstring) ++ ";\n" ++
     "// fun-oid\n" ++
     "newframe->payload[0] = argv[0];\n" ++
     "memcpy(&newframe->payload[1], " ++
            "&argv[1+arity], " ++
            "excess * sizeof(PtrOrLiteral));\n" ++

     "// try again - tail call stgApply \n" ++
     "stgJumpAdjust();\n" ++
     "STGJUMP0(stgApply" ++ drop arity npstring  ++ ");\n"

papeq npstring =
    "// push new args\n" ++
    debugp ["PAP just right:  %d args in PAP, %d new args\\n", "argCount", "arity"] ++
{-
    debugp ["pushing new args\\n"] ++
    "pushargs(arity, &argv[1]);\n" ++
    "// push args already in PAP\n" ++
    debugp ["pushing existing args\\n"] ++
    "pushargs(argCount, &argv[0].op->payload[fvCount+1]);\n" ++
    "// push the FUN\n" ++
    "pushargs(1, &argv[0]);\n" ++
-}

     -- new STACKFRAME
     -- 1.  shift bitmap left by one for slot for FUN-like obj
     -- 2.  add bit for FUN-like obj
     -- 4.  update size
     -- 3.  add bits for new args
     "bitmap.bitmap.mask <<= 1;\n" ++
     "bitmap.bitmap.mask |= 0x1;\n" ++
     "bitmap.bitmap.size += 1;\n" ++
     "bitmap2 = " ++ npStrToBMStr npstring ++ ";\n" ++
     "bitmap2.bitmap.mask <<= (argCount + 1);\n" ++
     "bitmap.bits += bitmap2.bits;\n" ++

     "newframe = stgAllocCallOrStackCont( &it_stgStackCont, argCount+1+arity );\n" ++
     "newframe->layout = bitmap;\n" ++
     "// self\n" ++
     "newframe->payload[0] = argv[0];\n" ++
     "// old args\n" ++
     "memcpy(&newframe->payload[1], " ++
            "&argv[0].op->payload[1 + fvCount], " ++
            "argCount * sizeof(PtrOrLiteral));\n" ++
     "// new args\n" ++
     "memcpy(&newframe->payload[1 + argCount], " ++
            "&argv[1], " ++
            "arity * sizeof(PtrOrLiteral));\n" ++

     "// stgJumpAdjust invalidates argv and newframe\n" ++
     "newframe = stgJumpAdjust();\n" ++
     "// tail call the FUN\n" ++
     "STGJUMP0(getInfoPtr(newframe->payload[0].op)->funFields.trueEntryCode);\n"

papneg npstring =
  let newargc = length npstring
  in "// bitmap for new args\n" ++
     "Bitmap64 bmold = argv[0].op->payload[fvCount].b;\n" ++
     "Bitmap64 bmnew = " ++ npStrToBMStr npstring ++ ";\n" ++
     "// shift mask by known only at runtime #existing PAP args\n" ++
     "bmnew.bitmap.mask <<= argCount;\n" ++
     "bmnew.bits += bmold.bits;\n" ++
     "// stgNewHeapPAP puts layout info at payload[fvCount]\n" ++
     "Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), bmnew);\n" ++
     "// copy fvs\n" ++
     debugp ["stgApply PAP inserting %d FVs into new PAP\\n", "fvCount"] ++
     "copyargs(&pap->payload[0], &argv[0].op->payload[0], fvCount);\n" ++
     "// copy old args\n" ++
     debugp ["stgApply PAP inserting %d old args into new PAP\\n", "argCount"] ++
     "copyargs(&pap->payload[fvCount+1], &argv[0].op->payload[fvCount+1], argCount);\n" ++
     "// copy new args to just after fvs, layout info, and old args\n" ++
     debugp ["stgApply PAP inserting " ++ show newargc ++ " new args into new PAP\\n"] ++
     "copyargs(&pap->payload[fvCount+1+argCount], &argv[1], " ++ show newargc ++ ");\n" ++
     "stgCurVal = HOTOPL(pap);\n" ++
     "stgPopCont();\n" ++
     "STGRETURN0();\n"
