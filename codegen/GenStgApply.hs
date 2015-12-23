module GenStgApply (
  doit
) where

import Util
import Data.List(intercalate)
import STGbits

data Strictness = Nonstrict
                | Strict1   -- evaluate args first, then fun
                  deriving(Eq)
    
doit = mapM (dumpStgApply 6) [Nonstrict, Strict1]

name strictness =
    case strictness of
      Nonstrict -> "nonstrict"
      Strict1 -> "strict"

dumpStgApply n strictness = 
    let (forward, macros, fun) = genAllstgApply strictness n
    in do writeFile "../stgApply/stgApply.h" (includehtop ++ forward ++ macros ++ includehbot)
          writeFile ("../stgApply/" ++ name strictness ++ ".c") (includec ++ fun)
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

callContArgvSave offset npstring =
    "callContSave( &argv[" ++ show offset ++ "], " ++ npStrToBMStr npstring ++ " );\n" 

callContArgvRestore offset =
    "callContRestore( &argv[" ++ show offset ++ "] );\n"

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

debugp (x:xs) = 
    "#ifdef DEBUGSTGAPPLY\n" ++
    "fprintf(stderr, \"" ++ x ++ "\"" ++ concatMap (", " ++) xs ++ ");\n" ++
    "#endif\n"

debugc code = 
    "#ifdef DEBUGSTGAPPLY\n" ++
    code ++
    "#endif\n"

-- npstring is args, which start at argv[1]
evalps npstring =
    concat [ callContArgvSave 0 ('P':npstring) ++
             "STGEVAL(argv[" ++ show (i+1) ++ "]);\n" ++
             callContArgvRestore 0 ++
             "argv[" ++ show (i+1) ++ "] = stgCurVal;  // stgCurVal possibly less indirect\n"
             | i <- [0..(length npstring - 1)], npstring!!i == 'P' ]

gen strictness npstring =
  (forward, macro, fun)
  where
    argc = length npstring
    fname = "stgApply" ++ npstring
    forward = "FnPtr " ++ fname ++ "();\n"
    arglist = 'f' : concat [',':'v':show i | i <- [1..argc]]
    macro = ""
    fun = 
     "DEFUN0(" ++ fname ++ ") {\n" ++
--     "  Cont *argframe;  // pointer to STACKCONT with actual parameters\n" ++
     "  Cont *newframe;  // pointer to STACKCONT to be constructed for call/jump\n" ++
     "  const int argc = " ++ show argc ++ ";\n" ++
     "  PtrOrLiteral argv[argc+1]; // argv[0] is the FUN/PAP/THUNK/BLACKHOLE\n" ++


--     "  popargs(argc+1, argv);\n" ++
-- new STACKFRAME
     "  popFrameArgs(argc+1, argv);\n" ++


        indent 2 (debugp [fname ++ " %s\\n", "getInfoPtr(argv[0].op)->name"]) ++

     -- now the function and its arguments are in C "argv[argc+1]"
     (if strictness == Strict1 then indent 2 (evalps npstring) else "") ++

     "\n" ++
     "  argv[0].op = derefPoL(argv[0]);\n" ++
     -- this if just saves a possibly unneeded call cont save
     "  if (getObjType(argv[0].op) == THUNK) {\n" ++
          indent 4 (callContArgvSave 0 ('P':npstring)) ++
          indent 4 (debugp [fname ++ " THUNK\\n"]) ++
     "    STGEVAL(argv[0]);\n" ++
          indent 4 (callContArgvRestore 0) ++
     "    // this works because stgCurVal is a GC root\n" ++
     "    argv[0].op = derefPoL(stgCurVal);\n" ++
     "  } // if THUNK\n" ++

     "\n" ++
     "  switch (getObjType(argv[0].op)) {\n" ++


     "  case FUN: {\n" ++
     "    int arity = getInfoPtr(argv[0].op)->funFields.arity;\n" ++
          indent 4 (debugp ["FUN %s arity %d\\n", 
                            "getInfoPtr(argv[0].op)->name", 
                            "getInfoPtr(argv[0].op)->funFields.arity"]) ++
     "    int excess = argc - arity;  // may be negative\n" ++
     "\n" ++
     (if argc == 1 then 
        "    // too many args not possible\n"
      else 
        "    // too many args?\n" ++
        "    if (excess > 0) {\n" ++
               indent 6 (debugp [fname ++ " FUN too many args\\n"]) ++
               indent 6 (optSwitch "excess" 
                                   1 
                                   (argc-1) 
                                   (\excess -> funpos npstring excess)) ++
        "    } else \n") ++
     "\n" ++
     "    // just right?\n" ++
     "    if (excess == 0) {\n" ++
            indent 6 (debugp [fname ++ " FUN just right\\n"]) ++
            (indent 6 $ funeq npstring) ++
     "    }\n" ++
     "    // excess < 0, too few args\n" ++
     "    else {\n" ++
            indent 6 (debugp [fname ++ " FUN too few args\\n"]) ++
            (indent 6 $ funneg npstring) ++
     "    } // if excess\n" ++
     "  } // case FUN\n" ++
     "\n" ++


     "  case PAP: {\n" ++
     "    int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount + \n" ++
     "                  getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;\n" ++
     "    Bitmap64 bitmap = argv[0].op->payload[fvCount].b;\n" ++
     "    Bitmap64 bitmap2;\n" ++
     "    int argCount = BMSIZE(bitmap);\n" ++
     "    int arity = getInfoPtr(argv[0].op)->funFields.arity - argCount;\n" ++
          indent 4 (debugp ["PAP/FUN %s arity %d\\n", 
                            "getInfoPtr(argv[0].op)->name", 
                            "getInfoPtr(argv[0].op)->funFields.arity"]) ++
     "    int excess = argc - arity;    // may be negative\n" ++
     "\n" ++
     (if argc == 1 then 
        "    // too many args not possible\n"
      else 
        "    // too many args?\n" ++
        "    if (excess > 0) {\n" ++
               indent 6 (debugp [fname ++ " PAP too many args\\n"]) ++
               indent 6 (optSwitch "excess" 
                                   1 
                                   (argc-1) 
                                   (\excess -> pappos npstring excess)) ++
        "    } else \n") ++
     "\n" ++
     "    // just right?\n" ++
     "    if (excess == 0) {\n" ++
            indent 6 (debugp [fname ++ " PAP just right\\n"]) ++
            (indent 6 $ papeq npstring) ++
     "\n" ++
     "    // excess < 0, too few args\n" ++
     "    } else {\n" ++
            indent 6 (debugp [fname ++ " PAP too few args\\n"]) ++
            (indent 6 $ papneg npstring) ++
     "    } // if excess\n" ++
     "  } // case PAP\n" ++
     "\n" ++


     "  case BLACKHOLE: {\n" ++
     "    fprintf(stderr, \"infinite loop detected in " ++ fname ++ "!\\n\");\n" ++
     "    showStgHeap();\n" ++
     "    assert(0);\n" ++
     "  } // case BLACKHOLE\n" ++
     "\n" ++
     "  default:\n" ++
     "    fprintf(stderr, \"" ++ fname ++ " not a THUNK, FUN, or PAP\\n\");\n" ++
     "    exit(0);\n" ++
     "  }  // switch\n" ++
     "  ENDFUN;\n" ++
     "}\n\n"

funpos npstring excess =
  let arity = (length npstring) - excess
  in debugp ["stgApply FUN " ++ show excess ++ " excess args\\n"] ++
     "// stash excess args\n" ++
     callContArgvSave (arity+1) (drop arity npstring) ++  -- FUN at index 0
     -- new STACKFRAME
     "// push needed args\n" ++
--     "pushargs(arity+1, argv);\n" ++

     "newframe = stgAllocStackCont( &it_stgStackCont, arity+1 );\n" ++
     "newframe->layout = " ++ npStrToBMStr ('P' : take arity npstring) ++ ";\n" ++
     "memcpy(newframe->payload, argv, (arity+1) * sizeof(PtrOrLiteral));\n" ++
--     "newframe = stgPopCont();\n" ++

     "// call-with-return the FUN\n" ++
     "STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);\n" ++
     "// restore excess args left shifted into argv\n" ++
     callContArgvRestore 1 ++
     "argv[0] = stgCurVal;\n" ++

     -- new STACKFRAME
     "// push excess args\n" ++
--     "pushargs(excess+1, argv);\n" ++ 
     "newframe = stgAllocStackCont( &it_stgStackCont, excess+1 );\n" ++
     "newframe->layout = " ++ npStrToBMStr ('P' : drop arity npstring) ++ ";\n" ++
     "memcpy(newframe->payload, argv, (excess+1) * sizeof(PtrOrLiteral));\n" ++
--     "newframe = stgPopCont();\n" ++

     "// try again - tail call stgApply\n" ++
     "STGJUMP0(stgApply" ++ drop arity npstring  ++ ");\n"


funeq npstring =
   -- new STACKFRAME
--  "pushargs(argc+1, argv);\n" ++
  "newframe = stgAllocStackCont( &it_stgStackCont, argc+1 );\n" ++
  "newframe->layout = " ++ npStrToBMStr ('P' : npstring) ++ ";\n" ++
  "memcpy(newframe->payload, argv, (argc+1) * sizeof(PtrOrLiteral));\n" ++
--  "newframe = stgPopCont();\n" ++

  "STGJUMP0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);\n"

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
     "STGRETURN1(HOTOPL(pap));\n"

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
pappos npstring excess =
  let arity = length npstring - excess -- effective arity of PAP
  in debugp ["stgApply PAP to " ++ show excess ++ " excess args\\n"] ++
     "// stash excess args\n" ++
     callContArgvSave (arity+1) (drop arity npstring) ++  -- PAP at index 0
     -- new STACKFRAME
--   1.  push new needed args
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

     "newframe = stgAllocStackCont( &it_stgStackCont, argCount+arity+1 );\n" ++
     "newframe->layout = bitmap;\n" ++
     "newframe->payload[0] = argv[0]; // self\n" ++
     "memcpy(&newframe->payload[1], " ++
            "&argv[0].op->payload[fvCount+1], " ++
            "argCount * sizeof(PtrOrLiteral)); // old args\n" ++
     "memcpy(&newframe->payload[1 + argCount], " ++ 
            "&argv[1], " ++ 
            "arity * sizeof(PtrOrLiteral));\n" ++
--     "newframe = stgPopCont();\n" ++

     "// call-with-return the FUN\n" ++
     "STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);\n" ++
     "// stash the FUN-oid\n" ++
     "argv[0] = stgCurVal;\n" ++
     "// restore excess args left shifted into argv[1]\n" ++
     callContArgvRestore 1 ++
     "// push FUN-oid and excess args\n" ++

     -- new STACKFRAME
--     "pushargs(excess + 1, argv);\n" ++ 
     "newframe = stgAllocStackCont(&it_stgStackCont, 1 + excess);\n" ++
     "newframe->layout = " ++ npStrToBMStr ('P' : drop arity npstring) ++ ";\n" ++
     "memcpy(&newframe->payload[0], " ++
            "&argv[0], " ++
            "(1 + excess) * sizeof(PtrOrLiteral));\n" ++
--     "newframe = stgPopCont();\n" ++

     "// try again - tail call stgApply \n" ++
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

     "newframe = stgAllocStackCont( &it_stgStackCont, argCount+arity+1 );\n" ++
     "newframe->layout = bitmap;\n" ++
     "newframe->payload[0] = argv[0]; // self\n" ++
     "memcpy(&newframe->payload[1], " ++
            "&argv[0].op->payload[fvCount+1], " ++
            "argCount * sizeof(PtrOrLiteral)); // old args\n" ++
     "memcpy(&newframe->payload[1 + argCount], " ++ 
            "&argv[1], " ++ 
            "arity * sizeof(PtrOrLiteral));\n" ++
--     "newframe = stgPopCont();\n" ++

     "// tail call the FUN\n" ++
     "STGJUMP0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);\n"

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
     "STGRETURN1(HOTOPL(pap));\n"
