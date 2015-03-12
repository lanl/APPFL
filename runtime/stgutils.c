#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "stgutils.h"

void callContSave(int argc, PtrOrLiteral argv[]) {
  Obj callCont;
  callCont.infoPtr = &it_stgCallCont;
  callCont.payload[0] = (PtrOrLiteral) {.argType = INT, .i = argc};
  for (int i = 0; i != argc; i++) callCont.payload[i+1] = argv[i];
  stgPushCont(callCont);
}

void callContRestore(PtrOrLiteral argv[]) {
  Obj callCont;
  callCont = stgPopCont();
  assert(callCont.payload[0].argType == INT);
  for (int i = 0; i != callCont.payload[0].i; i++) argv[i] = callCont.payload[i+1];
}

Obj *derefHO(Obj *op) {
  while (op->objType == INDIRECT)
    op = op->payload[0].op;
  return op;
}

Obj* derefPoL(PtrOrLiteral f) {
  assert(f.argType == HEAPOBJ && "derefPoL: not a HEAPOBJ");
  return derefHO(f.op);
}

void derefStgCurVal() {
  while (stgCurVal.argType == HEAPOBJ && stgCurVal.op->objType == INDIRECT)
    stgCurVal = stgCurVal.op->payload[0];
// is this a good place to check for BLACKHOLE?
}

DEFUN0(whiteHole) {
  fprintf(stderr,"in whiteHole, something not right or initialized somewhere, exiting\n");
  exit(0);
  RETURN0();
  ENDFUN;
}

// ****************************************************************
// since we always jump through the top of the stg stack we need some
// place to go when we're done this continuation is special, dropping 
// from stg land back to cmm land via RETURN0() rather than STGRETURN(0)


DEFUN0(stgShowResultCont) {
  fprintf(stderr,"done!\n");
  stgPopCont();  // clean up
  fprintf(stderr,"The answer is\n");
  showStgVal(stgCurVal);  
  RETURN0();
  ENDFUN;
}
InfoTab it_stgShowResultCont =
  { .name               = "stgShowResultCont",
    .entryCode          = &stgShowResultCont,
    .objType            = CALLCONT,
  };
Obj sho_stgShowResultCont = {
  .infoPtr = &it_stgShowResultCont,
  .objType = CALLCONT,
};

// ****************************************************************
// stgApply
// to make this fully general we do explicit heap manipulation

// argv points to the beginning of the arg list, but push backwards...
void pushargs(int argc, PtrOrLiteral argv[]) {
  for (int i = argc-1; i != -1; i--) _PUSH(argv[i]);
}
// ...and pop forwards
void popargs(int argc, PtrOrLiteral argv[]) {
  for (int i = 0; i != argc; i++) _POP(argv[i]);
}

void copyargs(PtrOrLiteral *dest, const PtrOrLiteral *src, int count) {
  for (int i = 0; i != count; i++) dest[i] = src[i];
}


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
      STGRETURN1(HOTOPL(pap));
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

  case BLACKHOLE: {
    fprintf(stderr, "infinite loop detected in stgApply!\n");
    exit(0);
  } // case BLACKHOLE

  default:
    fprintf(stderr, "stgApply not a THUNK, FUN, or PAP\n");
    exit(0);
  }  // switch
  ENDFUN;
}

// ****************************************************************
// early instance of stgApply

DEFUN2(stgApplyPap1, f, x1) {
  // now our predefined function/macro approach breaks down somewhat
  // since we don't statically know the arity of the PAP's underlying
  // function and we don't want a giant switch, so explicitly push onto stack
  fprintf(stderr, "stgApplyPap1");
  assert(f.argType == HEAPOBJ && f.op->objType == PAP);
  assert(f.op->infoPtr->funFields.arity == f.op->argCount + 1); // 1 is Pap1
  _PUSH(x1); // push last arg
  for (int i = f.op->infoPtr->fvCount + // skip past free variables
	       f.op->argCount - 1;      // index of last arg
            // i != -1;  WRONG, must have left some crap on the stack
               i != f.op->infoPtr->fvCount - 1;
       i--) _PUSH(f.op->payload[i]);
  _PUSH(f);  // push self
  STGJUMP0(f.op->infoPtr->entryCode);
  ENDFUN;
}

DEFUN2(stgApply1, f, x1) {
  f.op = derefPoL(f);

  switch (f.op->objType) {

  case THUNK: { // seems like it would be more efficient to do while(THUNK)
    fprintf(stderr, "stgApply1 THUNK\n");
    Obj callCont = {.infoPtr = &it_stgCallCont, .payload[0] = x1};
    stgPushCont(callCont);
    STGCALL1(f.op->infoPtr->entryCode, f);  // result in stgCurVal
    f = stgCurVal;  // new f
    callCont = stgPopCont();
    x1 = callCont.payload[0];
    STGJUMP2(stgApply1, f, x1);
    break;
  } // case THUNK

  case FUN: {
    switch(f.op->infoPtr->funFields.arity) {
    case 1: { //just right
      fprintf(stderr, "stgApply1 FUN just right\n");
      STGJUMP2(f.op->infoPtr->entryCode, f, x1);
      break;
    } // case 1
    default: { // arity > 1, too few args
      fprintf(stderr, "stgApply1 FUN too few args\n");
      // build a PAP -- CAREFUL, COULD TRIGGER GC
      // need to solve this problem for "let" in general
      // one solution would be to have GC only possibly triggered by STGCALL
      Obj *pap = stgNewHeapObj();
      *pap = *f.op;  // quick and dirty
      pap->objType = PAP;
      pap->argCount = 1;
      pap->payload[pap->infoPtr->fvCount] = x1; // just after the fvs
      STGRETURN1(HOTOPL(pap));
      break;
    } // default
    } // switch
  } // case FUN

  case PAP: {
    switch(f.op->infoPtr->funFields.arity - f.op->argCount) {
    case 1: { // just right
      fprintf(stderr, "stgApply1 PAP just right\n");
      STGJUMP2(stgApplyPap1, f, x1);
      break;
    } // case 1
    default: { // too few args
      fprintf(stderr, "stgApply1 PAP too few args\n");
      f.op->payload[f.op->infoPtr->fvCount + f.op->argCount] = x1;  // reuse PAP for now
      f.op->argCount += 1;
      STGRETURN1(f);
      break;
    } // default
    } // switch
  } // case PAP

  default: {
    fprintf(stderr, "stgApply not a THUNK, FUN, or PAP\n");
    exit(0);
  } // default
  }  // switch
  ENDFUN;
}


