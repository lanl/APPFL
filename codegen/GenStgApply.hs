module GenStgApply (
  dumpStgApply
) where

import Util
import Data.List(intercalate)

dumpStgApply n = 
    let (forward, fun) = genAllstgApply n
    in do writeFile "stgApply.h" $ includehtop ++ forward ++ includehbot
          writeFile "stgApply.c" $ includec ++ fun
          return ()
        where
          includehtop =
              "#ifndef stgApply_h\n" ++
              "#define stgApply_h\n" ++
              "#include \"../runtime/stg.h\"\n"
          includehbot =
              "#endif\n"
          includec =
              "#include \"../runtime/stg.h\"\n" ++
              "#include \"../runtime/cmm.h\"\n" ++
              "#include \"../runtime/stgutils.h\"\n" ++
              "#include \"stgApply.h\"\n" ++
              "#include <stdlib.h>\n" ++
              "#include <stdio.h>\n\n" 

genAlldebug n = let (forwards, funs) = unzip $ map genN [1..n]
                   in putStrLn $ concat forwards ++ "\n\n" ++ concat funs

genAllstgApply n = let (forwards, funs) = unzip $ map genN [1..n]
                   in (concat forwards, concat funs)

genN n = let (forwards, funs) = unzip $ map gen (pns n)
         in (concat forwards, concat funs)

pns n | n == 0 = [""]
      | n > 0 = pref $ pns (n-1)
      where
        pref [] = []
        pref (x:xs) = ('N':x):('P':x):pref xs

callContSave nps =
    if nps > 0 then 
        "callContSave(" ++ show nps ++ ", pargv);\n" 
    else 
        "// no pointer args to save\n"

callContAndArgvRestore nps pinds =
    if nps > 0 then 
        "callContRestore(pargv);\n" ++
        "// restore argv\n" ++
        concat ["argv[" ++ show ind ++ "] = pargv[" ++ show i ++ "];\n"
                | (i,ind) <- zip [0..] pinds] 
    else 
        "// no pointer args to restore\n"

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

gen s =
  (forward, fun)
  where
    argc = length s
    nps = length $ filter (=='P') s
    nns = argc - nps
    pinds = [ i | (i,c) <- zip [0..] s, c == 'P' ]
    ninds = [ i | (i,c) <- zip [0..] s, c == 'N' ]
    fname = "stgApply" ++ s
    forward = "FnPtr " ++ fname ++ "();\n"
    fun = 
     "DEFUN2(" ++ fname ++ ", N, f) {\n" ++
     "  assert(N.argType == INT);\n" ++
     "  const int argc = N.i;\n" ++
     "  assert(argc == " ++ show argc ++ ");\n" ++
     "  PtrOrLiteral argv[" ++ show argc ++ "];\n" ++
     "  popargs(argc, argv);\n" ++
     "  const int nps = " ++ show nps ++ ";\n" ++
     (if nps > 0 then 
        "  PtrOrLiteral pargv[" ++ show nps ++ "];\n" ++
        concat ["  pargv[" ++ show i ++ "] = argv[" ++ show ind ++ "];\n"
                | (i,ind) <- zip [0..] pinds]
     else
       "  // no pointer args to save\n") ++
     (if nns > 0 then 
        "  PtrOrLiteral nargv[" ++ show nns ++ "];\n" ++
        concat ["  nargv[" ++ show i ++ "] = argv[" ++ show ind ++ "];\n"
                | (i,ind) <- zip [0..] ninds]
     else
       "  // no non-pointer args to save\n") ++
     "\n" ++
     "  f.op = derefPoL(f);\n" ++
     "  if (f.op->objType == THUNK) {\n" ++
          indent 4 (callContSave nps) ++
     "    while (f.op->objType == THUNK) {\n" ++
     "      fprintf(stderr, \"stgApply THUNK\\n\");\n" ++
     "      STGEVAL(f);\n" ++
     "      // f.op = derefPoL(f);\n" ++
     "      f.op = derefPoL(stgCurVal); \n" ++
     "    } // while THUNK\n" ++
          indent 4 (callContAndArgvRestore nps pinds) ++
     "  } // if THUNK\n" ++
     "\n" ++
     "  switch (f.op->objType) {\n" ++
     "  case FUN: {\n" ++
     "    int arity = f.op->infoPtr->funFields.arity;\n" ++
     "    int excess = argc - arity;  // may be negative\n" ++
     "\n" ++
     (if argc == 1 then "    // too many args not possible\n"
      else 
        "    // too many args\n" ++
        "    if (excess > 0) {\n" ++
               indent 6 (optSwitch "excess" 1 (argc-1) 
                      (\excess -> funpos excess s pinds argc)) ++
        "    } else \n") ++
     "\n" ++
     "    // just right\n" ++
     "    if (excess == 0) {\n" ++
     "      fprintf(stderr, \"stgApply FUN just right\\n\");\n" ++
            (indent 6 $ funeq s argc) ++
     "    }\n" ++
     "    // excess < 0, too few args\n" ++
     "    else {\n" ++
     "      fprintf(stderr, \"stgApply FUN too few args\\n\");\n" ++
            (indent 6 $ funneg s pinds argc nps nns) ++
     "    } // if excess\n" ++
     "  } // case FUN\n" ++
     "\n" ++
     "  case PAP: {\n" ++
     "    int fvCount = f.op->infoPtr->fvCount;\n" ++
     "    int pappargc, papnargc;\n" ++
     "    PNUNPACK(f.op->payload[fvCount].i, pappargc, papnargc);\n" ++
     "    int argCount = pappargc + papnargc;\n" ++
     "    assert(argCount == f.op->argCount && \"stgApply:  PAP error 1\");\n" ++
     "    int arity = f.op->infoPtr->funFields.arity - argCount;\n" ++
     "    int excess = argc - arity;\n" ++
     "\n" ++
     (if argc == 1 then "    // too many args not possible\n"
      else 
        "    // too many args\n" ++
        "    if (excess > 0) {\n" ++
        "      fprintf(stderr, \"stgApply PAP too many args\\n\");\n" ++
               indent 6 (optSwitch "excess" 1 (argc-1) 
                             (\n -> pappos n s pinds argc)) ++
        "    } else \n") ++
     "\n" ++
     "    // just right\n" ++
     "    if (excess == 0) {\n" ++
     "      fprintf(stderr, \"stgApply FUN just right\\n\");\n" ++
            (indent 6 $ papeq s argc) ++
     "\n" ++
     "    // excess < 0, too few args\n" ++
     "    } else {\n" ++
     "      fprintf(stderr, \"stgApply PAP too few args\\n\");\n" ++
            (indent 6 $ papneg s pinds argc nps nns) ++
     "    } // if excess\n" ++
     "  } // case PAP\n" ++
     "\n" ++
     "  case BLACKHOLE: {\n" ++
     "    fprintf(stderr, \"infinite loop detected in stgApply!\\n\");\n" ++
     "    showStgHeap();\n" ++
     "    exit(0);\n" ++
     "  } // case BLACKHOLE\n" ++
     "\n" ++
     "  default:\n" ++
     "    fprintf(stderr, \"stgApply not a THUNK, FUN, or PAP\\n\");\n" ++
     "    exit(0);\n" ++
     "  }  // switch\n" ++
     "  ENDFUN;\n" ++
     "}\n\n"


funpos excess s pinds argc =
  let usedParamCount = argc - excess
      usedPParamCount = length $ filter (=='P') (take usedParamCount s)
      usedNParamCount = usedParamCount - usedPParamCount
      excessParams = drop (length s - excess) s
      excessPParamCount = length $ filter (=='P') excessParams
  in "fprintf(stderr, \"stgApply FUN %d excess args\\n\", " ++ show (length excessParams) ++ ");\n" ++
     "// stash excess pointer args\n" ++
     (if excessPParamCount > 0 then
        "callContSave(" ++ show excessPParamCount ++ ", " ++
                      "&pargv[" ++ show usedPParamCount ++ "]);\n"
      else "// no excess pointer params to save\n") ++
     "// push needed args\n" ++
--   "pushargs(arity, argv);\n" ++
--   1. push usedNParamCount args from nargv
--   2. push usedPParamCount args from pargv
     (if usedNParamCount > 0 then 
          "pushargs(" ++ show usedNParamCount ++ ", nargv);\n" 
      else "// 0 non-pointers to push\n") ++
     (if usedPParamCount > 0 then
          "pushargs(" ++ show usedPParamCount ++ ", pargv);\n" 
      else "// 0 pointers to push\n") ++
     "// call-with-return the FUN\n" ++
     "STGCALL1(f.op->infoPtr->entryCode, f);\n" ++
     "// restore excess args\n" ++
     (if excessPParamCount > 0 then
        "callContRestore(&pargv[" ++ show usedPParamCount ++ "]);\n" ++
        "// restore argv\n" ++
        concat ["argv[" ++ show ind ++ "] = pargv[" ++ show i ++ "];\n"
                | (i,ind) <- zip [usedPParamCount..] (drop usedPParamCount pinds)]
      else "// no excess pointer params to restore\n") ++
     "// grab obj just returned\n" ++
     "f = stgCurVal;\n" ++
     "// new argc\n" ++
     "N.i = excess;\n" ++
     "// push excess args\n" ++
     "pushargs(excess, &argv[" ++ show usedParamCount ++ "]);\n" ++ 
     "// try again - tail call stgApply\n" ++
--     optSwitch "excess" 1 (argc-1) 
--                   (\i -> "STGJUMP2(stgApply" ++ drop (argc - i) s  ++ ", N, f);\n")
     "STGJUMP2(stgApply" ++ drop (argc - excess) s  ++ ", N, f);\n"


funeq s argc = 
  let usedPParamCount = length $ filter (=='P') s
      usedNParamCount = argc - usedPParamCount
  in (if usedNParamCount > 0 then
          "pushargs(" ++ show usedNParamCount ++ ", nargv);\n"
      else "// 0 non-pointers to push\n") ++
     (if usedPParamCount > 0 then
          "pushargs(" ++ show usedPParamCount ++ ", pargv);\n"
      else "// 0 pointers to push\n") ++
     "// tail call the fun\n" ++
     "STGJUMP1(f.op->infoPtr->entryCode, f);\n"

funneg s pinds argc nps nns = 
     "int fvCount = f.op->infoPtr->fvCount;\n" ++
     "// stgNewHeapPAP puts layout info at payload[fvCount]\n" ++
     "Obj *pap = stgNewHeapPAP(f.op->infoPtr, " ++ 
                               show nps ++ ", " ++ show nns ++ ");\n" ++
     "pap->argCount = argc;\n" ++
     "// copy fvs\n" ++
     "fprintf(stderr, \"stgApply FUN inserting %d FVs into new PAP\\n\", fvCount);\n" ++
     "copyargs(&pap->payload[0], &f.op->payload[0], fvCount);\n" ++
     "// copy pargs to just after fvs and layout info\n" ++
     "fprintf(stderr, \"stgApply FUN inserting %d pointers into new PAP\\n\", " ++
                      show nps ++ ");\n" ++
     (if nps > 0 then
          "copyargs(&pap->payload[fvCount+1], pargv, " ++ show nps ++ ");\n" 
      else "// 0 pointers to insert into PAP\n") ++
     "fprintf(stderr, \"stgApply FUN inserting %d non-pointers into new PAP\\n\", " ++
                      show nns ++ ");\n" ++
     (if nns > 0 then
          "copyargs(&pap->payload[fvCount+1+" ++ show nps ++ "], nargv, " ++ show nns ++ ");\n"
      else "// 0 non-pointers to insert into PAP\n") ++
     "STGRETURN1(HOTOPL(pap));\n"

-- excess - #excess args, also in C
-- s      - "NNPPNP" of new args
-- pinds  - list of indexes of Ps in argc
-- argc   - #new args
-- pappargc - in C, #pointer args in pap
-- papnargc - in C, #non-pointer args in pap
pappos excess s pinds argc =
  let usedParamCount = argc - excess
      usedPParamCount = length $ filter (=='P') (take usedParamCount s)
      usedNParamCount = usedParamCount - usedPParamCount
      excessParams = drop (length s - excess) s
      excessPParamCount = length $ filter (=='P') excessParams
  in "fprintf(stderr, \"stgApply PAP %d excess args\\n\", " ++ show (length excessParams) ++ ");\n" ++
     "// stash excess pointer args\n" ++
     (if excessPParamCount > 0 then
        "callContSave(" ++ show excessPParamCount ++ ", " ++
                      "&pargv[" ++ show usedPParamCount ++ "]);\n"
      else "// no excess pointer params to save\n") ++
     "// push needed args\n" ++
--   "pushargs(arity, argv);\n" ++
--   non-pointers first, last first
--   1. push usedNParamCount args from nargv
--   2. push papnargc args from PAP[fvCount+1+pappargc..]
--   3. push usedPParamCount args from pargv
--   4. push pappargc args from PAP[fvCount+1..]
     (if usedNParamCount > 0 then 
          "pushargs(" ++ show usedNParamCount ++ ", nargv);\n" 
      else "// 0 non-pointers to push\n") ++
     "pushargs(papnargc, &f.op->payload[fvCount+1+pappargc]);\n" ++
     (if usedPParamCount > 0 then
          "pushargs(" ++ show usedPParamCount ++ ", pargv);\n" 
      else "// 0 pointers to push\n") ++
     "pushargs(pappargc, &f.op->payload[fvCount+1]);\n" ++
     "// call-with-return the FUN\n" ++
     "STGCALL1(f.op->infoPtr->entryCode, f);\n" ++
     "// restore excess args\n" ++
     (if excessPParamCount > 0 then
        "callContRestore(&pargv[" ++ show usedPParamCount ++ "]);\n" ++
        "// restore argv\n" ++
        concat ["argv[" ++ show ind ++ "] = pargv[" ++ show i ++ "];\n"
                | (i,ind) <- zip [usedPParamCount..] (drop usedPParamCount pinds)]
      else "// no excess pointer params to restore\n") ++
     "// grab obj just returned\n" ++
     "f = stgCurVal;\n" ++
     "// new argc\n" ++
     "N.i = excess;\n" ++
     "// push excess args\n" ++
     "pushargs(excess, &argv[" ++ show usedParamCount ++ "]);\n" ++ 
     "// try again - tail call stgApply \n" ++
     "STGJUMP2(stgApply" ++ drop (argc - excess) s  ++ ", N, f);\n"

-- s      - "NNPPNP" of new args
-- argc   - #new args
-- pappargc - in C, #pointer args in pap
-- papnargc - in C, #non-pointer args in pap
papeq s argc = 
  let usedPParamCount = length $ filter (=='P') s
      usedNParamCount = argc - usedPParamCount
  in (if usedNParamCount > 0 then
       "pushargs(" ++ show usedNParamCount ++ ", nargv);\n"
      else "// 0 non-pointer args\n") ++
     "pushargs(papnargc, &f.op->payload[fvCount+1+pappargc]);\n" ++
     (if usedPParamCount > 0 then
       "pushargs(" ++ show usedPParamCount ++ ", pargv);\n"
      else "// 0 non-pointer args\n") ++
     "pushargs(pappargc, &f.op->payload[fvCount+1]);\n" ++
     "// tail call the FUN\n" ++
     "STGJUMP1(f.op->infoPtr->entryCode, f);\n"

-- pappargc - in C, #pointer args in pap
-- papnargc - in C, #non-pointer args in pap
papneg s pinds argc nps nns =
  let usedPParamCount = length $ filter (=='P') s
      usedNParamCount = argc - usedPParamCount
  in "// stgNewHeapPAP puts layout info at payload[fvCount]\n" ++
     "Obj *pap = stgNewHeapPAP(f.op->infoPtr, " ++ 
                               show nps ++ " + pappargc, " ++ 
                               show nns ++ " + papnargc);\n" ++
     "pap->argCount = argc + pappargc + papnargc;\n" ++
     "// copy fvs\n" ++
     "fprintf(stderr, \"stgApply PAP inserting %d FVs into new PAP\\n\", fvCount);\n" ++
     "copyargs(&pap->payload[0], &f.op->payload[0], fvCount);\n" ++
     "// copy pap pointers to just after fvs and layout info\n" ++
     "fprintf(stderr, \"stgApply PAP inserting %d old pointers into new PAP\\n\", pappargc);\n" ++
     "copyargs(&pap->payload[fvCount+1], &f.op->payload[fvCount+1], pappargc);\n" ++
     "// copy pargs to just after fvs, layout info, and old pointers\n" ++
     "fprintf(stderr, \"stgApply PAP inserting " ++ show nps ++ 
                      " new pointers into new PAP\\n\");\n" ++
     (if nps > 0 then
       "copyargs(&pap->payload[fvCount+1+pappargc], pargv, " ++ show nps ++ ");\n"
      else "// 0 new pointers to insert into PAP\n") ++

     "fprintf(stderr, \"stgApply PAP inserting %d old non-pointers into new PAP\\n\", papnargc);\n" ++
     "copyargs(&pap->payload[fvCount+1+pappargc+" ++ show nps ++ "], " ++
                            "&f.op->payload[fvCount+1+pappargc], papnargc);\n" ++
     "fprintf(stderr, \"stgApply PAP inserting " ++ show nns ++ 
                      " new non-pointers into new PAP\\n\");\n" ++
     (if nns > 0 then
          "copyargs(&pap->payload[fvCount+1+pappargc+" ++ show nps ++ "+papnargc], nargv, " ++ 
                    show nns ++ ");\n"
      else "// 0 new non-pointers to insert into PAP\n") ++
     "STGRETURN1(HOTOPL(pap));\n"


