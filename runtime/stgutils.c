#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "stgutils.h"
#include "stg.h"

// ****************************************************************
// since we always jump through the top of the stg stack we need some
// place to go when we're done this continuation is special, dropping 
// from stg land back to cmm land via RETURN0() rather than STGRETURN(0)

DEFUN0(fun_stgShowResultCont) {
  fprintf(stderr,"done!\n");
  stgPopCont();  // clean up
  fprintf(stderr,"The answer is\n");
  showStgVal(stgCurVal);  
  RETURN0();
  ENDFUN;
}

InfoTab it_stgShowResultCont =
  { .name       = "fun_showResultCont",
    .fvCount    = 0,
    .entryCode  = &fun_stgShowResultCont,
    .objType    = CALLCONT,
  };


DEFUN0(stgCallCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgCallCont returning\n");
  RETURN0();
  ENDFUN;
}

InfoTab it_stgCallCont =
  { .name = "stgCallCont",
    .fvCount = 0,
    .entryCode = &stgCallCont,
    .objType = CALLCONT,
  };

DEFUN0(stgUpdateCont) {
  Obj *contp = stgPopCont();
  assert(contp->objType == UPDCONT && "I'm not an UPDCONT!");
  PtrOrLiteral p = contp->payload[0];
  assert(p.argType == HEAPOBJ && "not a HEAPOBJ!");
  assert(p.op->objType == BLACKHOLE && "not a BLACKHOLE!");
  fprintf(stderr, "stgUpdateCont updating\n  ");
  showStgObj(p.op);
  fprintf(stderr, "with\n  ");
  showStgObj(stgCurVal.op);
  p.op->objType = INDIRECT;
  p.op->payload[0] = stgCurVal;
  STGRETURN0();
  ENDFUN
}

InfoTab it_stgUpdateCont =
  { .name = "default stgUpdateCont",
    .fvCount = 0,
    .entryCode = &stgUpdateCont,
    .objType = UPDCONT,
    .layoutInfo.payloadSize = 1, // self
  };

void stgThunk(PtrOrLiteral self) {
  assert(self.argType == HEAPOBJ && "stgThunk:  not HEAPOJ\n");
  Obj *contp = stgAllocCont(&it_stgUpdateCont);
  contp->payload[0] = self;
  strcpy(contp->ident, self.op->ident); //override default
  self.op->objType = BLACKHOLE;	
}

Obj *derefPoL(PtrOrLiteral f) {
  assert(f.argType == HEAPOBJ && "derefPoL: not a HEAPOBJ");
  return derefHO(f.op);
}

void derefStgCurVal() {
  while (stgCurVal.argType == HEAPOBJ && stgCurVal.op->objType == INDIRECT)
    stgCurVal = stgCurVal.op->payload[0];
// is this a good place to check for BLACKHOLE?
}

Obj *derefHO(Obj *op) {
  while (op->objType == INDIRECT)
    op = op->payload[0].op;
  return op;
}

DEFUN0(whiteHole) {
  fprintf(stderr,"in whiteHole, something not right or initialized somewhere, exiting\n");
  exit(0);
  RETURN0();
  ENDFUN;
}

DEFUN0(stg_constructorcall) {
  fprintf(stderr,"we are not using the call-everything convention!\n");
  exit(0);
  RETURN0();
  ENDFUN;
}

void callContSave(int argc, PtrOrLiteral argv[]) {
  Obj *cc = stgAllocCallCont2( &it_stgCallCont, argc );
  for (int i = 0; i != argc; i++) 
    cc->payload[i+1] = argv[i];
}

void callContRestore(PtrOrLiteral argv[]) {
  Obj *cc = stgPopCont();
  assert(cc->objType == CALLCONT);
  assert(cc->payload[0].argType == INT);
  for (int i = 0; i != cc->payload[0].i; i++) 
    argv[i] = cc->payload[i+1];
}

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
  PtrOrLiteral argv[64];

  popargs(argc, argv);

  /*
  f.op = derefPoL(f);
  switch (f.op->objType) {

  case THUNK: { // seems more efficient to do while(THUNK)
    fprintf(stderr, "stgApply THUNK\n");
    // thunks don't take args (eval-apply)
    callContSave(argc, argv);
    STGEVAL(f);
    callContRestore(argv);
    // now pass the args
    pushargs(argc, argv);
    STGJUMP2(stgApply, N, f);
    break;
  } // case THUNK
  */


  f.op = derefPoL(f);
  if (f.op->objType == THUNK) {  
    callContSave(argc, argv);
    while (f.op->objType == THUNK) {
      fprintf(stderr, "stgApply THUNK\n");
      STGEVAL(f);
      //      f.op = derefPoL(f);  // derefPoL(stgCurVal) less dereferencing?
      f.op = derefPoL(stgCurVal); 
    } // while THUNK
    callContRestore(argv);
  } // if THUNK

  switch (f.op->objType) {
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
      int fvCount = f.op->infoPtr->fvCount;
      Obj *pap = stgNewHeapPAP(f.op->infoPtr, argc, 0); // all "pointers" for now
      pap->argCount = argc + 0;
      // copy fvs
      fprintf(stderr, "stgApply FUN inserting %d FVs into new PAP\n", fvCount);
      copyargs(&pap->payload[0], &f.op->payload[0], fvCount);
      // copy args to just after fvs and layout info
      fprintf(stderr, "stgApply FUN inserting %d args into new PAP\n", argc);
      copyargs(&pap->payload[fvCount+1], argv, argc);
      STGRETURN1(HOTOPL(pap));
    } // if excess
  } // case FUN

  case PAP: {
    int fvCount = f.op->infoPtr->fvCount;
    int pargc, nargc;
    PNUNPACK(f.op->payload[fvCount].i, pargc, nargc);
    int argCount = pargc + nargc;
    assert(argCount == f.op->argCount && "stgApply:  PAP error 1");
    int arity = f.op->infoPtr->funFields.arity - argCount;
    int excess = argc - arity;

    // too many
    if (excess > 0) { 
      fprintf(stderr, "stgApply PAP too many args: \n");
      // stash excess args
      callContSave(excess, &argv[arity]);
      // push correct number of new args
      pushargs(arity, argv);
      // push args already in PAP object, just beyond fvs
      pushargs(argCount, &f.op->payload[fvCount + 1]);
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
      STGJUMP2(stgApply, N, f);  //stgApply derefs f

      // just right
    } else if (excess == 0) {
      fprintf(stderr, "stgApply1 PAP just right\n");
      // push new args
      pushargs(arity, argv);
      // push args already in PAP object, just beyond fvs
      pushargs(argCount, &f.op->payload[fvCount + 1]);
      // tail call the FUN
      STGJUMP1(f.op->infoPtr->entryCode, f);

      // excess < 0, too few args
    } else {
      fprintf(stderr, "stgApply1 PAP too few args\n");
      Obj *pap = stgNewHeapPAP(f.op->infoPtr, argCount + argc, 0);
      // copy fvs and existing args
      fprintf(stderr, "stgApply PAP inserting %d FVs into new PAP\n", fvCount);
      copyargs(&pap->payload[0], &f.op->payload[0], fvCount);
      fprintf(stderr, "stgApply PAP inserting %d old args into new PAP\n", argCount);
      copyargs(&pap->payload[fvCount+1], &f.op->payload[fvCount+1], argCount);
      // copy new args to just after
      fprintf(stderr, "stgApply PAP inserting %d new args into new PAP\n", argc);
      pap->argCount = f.op->argCount;
      copyargs(&pap->payload[fvCount + argCount + 1], argv, argc);
      pap->argCount += argc;
      STGRETURN1(HOTOPL(pap));
    } // if excess
  } // case PAP

  case BLACKHOLE: {
    fprintf(stderr, "infinite loop detected in stgApply!\n");
    showStgHeap();
    exit(0);
  } // case BLACKHOLE

  default:
    fprintf(stderr, "stgApply not a THUNK, FUN, or PAP\n");
    exit(0);
  }  // switch
  ENDFUN;
}

