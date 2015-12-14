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

--  Cont *cc = stgAllocCallCont( &it_stgCallCont, argc );
{-
evalps nps pinds =
    -- only need to put all but the one to be evaluated in a ccont
    -- but for a quick test...
    concat [ debugp [ "strict arg eval\\n  " ] ++
             debugc ("showStgVal(pargv[" ++ show i ++ "]);\n") ++
             debugp [ "\\n" ] ++
             callContSave nps ++
             "STGEVAL(pargv[" ++ show i ++ "]);\n" ++
             callContAndArgvRestore nps pinds
             | i <- [0..nps-1] ]
-}

{-
evalps nps pinds =
    -- only need to put all but the one to be evaluated in a ccont
    -- but for a quick test...
    if nps > 0 then
      concat [ callContSave (nps+1) ++
               "STGEVAL(pargv[" ++ show i ++ "]);\n" ++
               callContAndArgvRestore (nps+1) pinds  -- nps+1 just so not 0
               | i <- [0..nps-1] ] ++
      "f = pargv[" ++ show nps ++ "];\n"
    else ""
-}

gen strictness npstring =
  (forward, macro, fun)
  where
    argc = length npstring
    fname = "stgApply" ++ npstring
    forward = "FnPtr " ++ fname ++ "();\n"
    arglist = 'f' : concat [',':'v':show i | i <- [1..argc]]
    macro = 
      "#define STGAPPLY" ++ npstring ++ "(" ++ arglist ++ ") \\\n" ++
      "do { \\\n" ++
      "  STGJUMP" ++ show (argc+1) ++ "(stgApply" ++ npstring ++ "," ++ arglist ++ "); \\\n" ++
      "  } while(0)\n\n"

    fun = 
     "DEFUN0(" ++ fname ++ ") {\n" ++
     "  const int argc = " ++ show argc ++ ";\n" ++
     "  PtrOrLiteral argv[argc+1]; // argv[0] is the FUN/PAP/THUNK/BLACKHOLE\n" ++
     "  popargs(argc+1, argv);\n" ++
        indent 2 (debugp [fname ++ " %s\\n", "getInfoPtr(argv[0].op)->name"]) ++

     -- now the function and its arguments are in C "argv[argc+1]"

     -- TODO
     --(if strictness == Strict1 then indent 2 (evalps nps pinds) else "") ++

     -- TODO
     -- STGEVAL is only called from stgApplyXXX so it need not push a call continuation
     "\n" ++
     "  argv[0].op = derefPoL(argv[0]);\n" ++
     -- this if just saves a possibly
     "  if (getObjType(argv[0].op) == THUNK) {\n" ++
          indent 4 (callContArgvSave 0 ('P':npstring)) ++
          indent 4 (debugp [fname ++ " THUNK\\n"]) ++
     "    STGEVAL(argv[0]);\n" ++
          indent 4 (callContArgvRestore 0) ++
     -- this works because stgCurVal is a GC root
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
            (indent 6 funeq) ++
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
     "    int argCount = BMSIZE(argv[0].op->payload[fvCount].b);\n" ++
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
            (indent 6 papeq) ++
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
     "// push needed args\n" ++
     "pushargs(arity, &argv[1]);\n" ++
     "// call-with-return the FUN\n" ++
     "STGCALL1(getInfoPtr(argv[0].op)->funFields.trueEntryCode, argv[0]);\n" ++
     "// restore excess args left shifted into argv\n" ++
     callContArgvRestore 0 ++
     "// push excess args\n" ++
     "pushargs(excess, argv);\n" ++ 
     "// try again - tail call stgApply\n" ++
     "STGJUMP1(stgApply" ++ drop arity npstring  ++ ", stgCurVal);\n"


funeq = 
  "pushargs(argc, &argv[1]);\n" ++
  "STGJUMP1(getInfoPtr(argv[0].op)->funFields.trueEntryCode, argv[0]);\n"

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

--pappos npstring excess = "fprintf(stderr, \"papneg not implemented\\n\");\n"
--papeq = "fprintf(stderr, \"papeq not implemented\\n\");\n"
--papneg npstring = "fprintf(stderr, \"papneg not implemented\\n\");\n"


-- C argCount = number of args already in PAP
-- C arity
-- C excess
pappos npstring excess =
  let arity = length npstring - excess -- effective arity of PAP
  in debugp ["stgApply PAP to " ++ show excess ++ " excess args\\n"] ++
     "// stash excess args\n" ++
     callContArgvSave (arity+1) (drop arity npstring) ++  -- PAP at index 0
--   1.  push new needed args
--   2.  push args already in pap from PAP[fvCount+1...]
     "// push needed args\n" ++
     "pushargs(arity, &argv[1]);\n" ++
     "// push args already in PAP\n" ++
     "pushargs(argCount, &argv[0].op->payload[fvCount+1]);\n" ++
     "// call-with-return the FUN\n" ++
     "STGCALL1(getInfoPtr(argv[0].op)->funFields.trueEntryCode, argv[0]);\n" ++
     "// restore excess args left shifted into argv\n" ++
     callContArgvRestore 0 ++
     "// push excess args\n" ++
     "pushargs(excess, argv);\n" ++ 
     "// try again - tail call stgApply \n" ++
     "STGJUMP1(stgApply" ++ drop arity npstring  ++ ", stgCurVal);\n"

papeq = 
    "// push new args\n" ++
    debugp ["PAP just right:  %d args in PAP, %d new args\\n", "argCount", "arity"] ++
    debugp ["pushing new args\\n"] ++ 
    "pushargs(arity, &argv[1]);\n" ++
    "// push args already in PAP\n" ++
    debugp ["pushing existing args\\n"] ++ 
    "pushargs(argCount, &argv[0].op->payload[fvCount+1]);\n" ++
     "// tail call the FUN\n" ++
     "STGJUMP1(getInfoPtr(argv[0].op)->funFields.trueEntryCode, argv[0]);\n"

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
      "// 0 new pointers to insert into PAP\n" ++
     "STGRETURN1(HOTOPL(pap));\n"
