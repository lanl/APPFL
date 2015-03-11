DEFUN2(stgApply, N, f) {
  assert(N.argType == INT);
  const int argc = N.i;
  PtrOrLiteral argv[16];
  popargs(argc, argv);

  f.op = derefPoL(f);
  switch (f.op->objType) {

  case THUNK: { // seems more efficient to do while(THUNK)
    fprintf(stderr, "stgApply THUNK\n");
    // thunks don't take args (eval-apply)
    callContSave(argc, argv);
    do {
      STGCALL1(f.op->infoPtr->entryCode, f);  // result in stgCurVal
      f = stgCurVal;  // new f
      f.op = derefPoL(f);
    } while (f.op->objType == THUNK);
    // restore args
    callContRestore(argv);
    // now pass the args
    pushargs(argc, argv);
    STGJUMP2(stgApply, N, f);
    break;
  } // case THUNK

  case FUN: {
    int arity = f.op->infoPtr->funFields.arity;
    int excess = argc - arity;  // may be negative

    // too many args
    if (excess > 0) {
      fprintf(stderr, "stgApply FUN too many args\n");
      // stash excess args
      callContSave(excess, &argv[arity]);
      // push needed args
      pushargs(arity, argv);
      // call the FUN
      STGCALL1(f.op->infoPtr->entryCode, f);
      // restore excess args
      callContRestore(argv);  // !! they're shifted
      // grab obj just returned
      f = stgCurVal;
      // new argc
      N.i = excess;
      // push excess args, they've been shifted left in argv
      pushargs(excess, argv);
      // try again.
      STGJUMP2(stgApply, N, f);

      // just right
    } else if (excess == 0) { 
      fprintf(stderr, "stgApply FUN just right\n");
      // push args
      pushargs(argc, argv);
      // tail call the fun
      STGJUMP1(f.op->infoPtr->entryCode, f);

    // excess < 0, too few args
    } else { 
      fprintf(stderr, "stgApply FUN too few args\n");
      Obj *pap = stgNewHeapObj();
      *pap = *f.op;  // quick and dirty
      pap->objType = PAP;
      // copy args to just after fvs
      pap->argCount = argc;
      fprintf(stderr, "stgApply FUN inserting %d args into new PAP\n", argc);
      copyargs(&pap->payload[pap->infoPtr->fvCount], argv, argc);
      STGRETURN1(POLHO(pap));
    } // if excess
  } // case FUN

  case PAP: {
    int arity = f.op->infoPtr->funFields.arity - f.op->argCount;
    int excess = argc - arity;

    // too many
    if (excess > 0) { 
      fprintf(stderr, "stgApply PAP too many args: \n");
      // stash excess args
      callContSave(excess, &argv[arity]);
      // push correct number of new args
      pushargs(arity, argv);
      // push args already in PAP object, just beyond fvs
      pushargs(f.op->argCount, &f.op->payload[f.op->infoPtr->fvCount]);
      // call the FUN
      STGCALL1(f.op->infoPtr->entryCode, f);
      // restore excess args
      callContRestore(argv);  // !! they're shifted
      // grab obj just returned
      f = stgCurVal;
      // new argc
      N.i = excess;
      // push excess args, they've been shifted left in argv
      pushargs(excess, argv);
      // try again
      STGJUMP2(stgApply, N, f);

      // just right
    } else if (excess == 0) {
      fprintf(stderr, "stgApply1 PAP just right\n");
      // push new args
      pushargs(arity, argv);
      // push args already in PAP object, just beyond fvs
      pushargs(f.op->argCount, &f.op->payload[f.op->infoPtr->fvCount]);
      // leave f.op->argCount unchanged?
      // tail call the FUN
      STGJUMP1(f.op->infoPtr->entryCode, f);

      // excess < 0, too few args
    } else {
      fprintf(stderr, "stgApply1 PAP too few args\n");
      // for now re-use the PAP object
      // add new args to PAP after fvs and existing args
      copyargs(&f.op->payload[f.op->infoPtr->fvCount + f.op->argCount],
	       argv, argc);
      f.op->argCount += argc;
      STGRETURN1(f);
    } // if excess
  } // case PAP

  default:
    fprintf(stderr, "stgApply not a THUNK, FUN, or PAP\n");
    exit(0);
  }  // switch
  ENDFUN;
}
