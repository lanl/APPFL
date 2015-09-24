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
  stgPopCont();  // clean up--normally the job of the returnee
  fprintf(stderr,"The answer is\n");
  showStgVal(stgCurVal);  
  RETURN0();
  ENDFUN;
}

InfoTab it_stgShowResultCont __attribute__((aligned(8))) =
  { .name       = "fun_showResultCont",
    //    .fvCount    = 0,
    .entryCode  = &fun_stgShowResultCont,
    .objType    = CALLCONT,
    .layoutInfo.boxedCount = -1,  // shouldn't be using this
    .layoutInfo.unboxedCount = -1,  // shouldn't be using this
  };


DEFUN0(stgCallCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgCallCont returning\n");
  RETURN0();  // fall back to the cmm trampoline
  ENDFUN;
}

InfoTab it_stgCallCont __attribute__((aligned(8))) =
  { .name = "stgCallCont",
    //    .fvCount = 0,
    .entryCode = &stgCallCont,
    .objType = CALLCONT,
    .layoutInfo.boxedCount = -1,  // shouldn't be using this
    .layoutInfo.unboxedCount = -1,  // shouldn't be using this
  };

DEFUN0(stgUpdateCont) {
  Obj *contp = stgPopCont();
  assert(contp->objType == UPDCONT && "I'm not an UPDCONT!");
  PtrOrLiteral p = contp->payload[0];
  assert(p.argType == HEAPOBJ && "not a HEAPOBJ!");
  fprintf(stderr, "stgUpdateCont updating\n  ");
  showStgObj(p.op);
  fprintf(stderr, "with\n  ");
  showStgObj(stgCurVal.op);
  if (p.op->objType != BLACKHOLE) {
    fprintf(stderr, "but updatee is not a BLACKHOLE!\n");
    showStgHeap();
    assert(p.op->objType == BLACKHOLE);
  }
  p.op->payload[0] = stgCurVal;
  p.op->objType = INDIRECT;
  STGRETURN0();
  ENDFUN
}

InfoTab it_stgUpdateCont __attribute__((aligned(8))) =
  { .name = "default stgUpdateCont",
    //    .fvCount = 1, // self
    .entryCode = &stgUpdateCont,
    .objType = UPDCONT,
    .layoutInfo.payloadSize = 1, // self
    .layoutInfo.boxedCount = 1,
    .layoutInfo.unboxedCount = 0,
  };

void stgThunk(PtrOrLiteral self) {
  assert(self.argType == HEAPOBJ && "stgThunk:  not HEAPOBJ\n");
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

