#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "stgutils.h"
#include "stg.h"
#include "obj.h"

// ****************************************************************
// since we always jump through the top of the stg stack we need some
// place to go when we're done this continuation is special, dropping 
// from stg land back to cmm land via RETURN0() rather than STGRETURN(0)

Obj *derefPoL(PtrOrLiteral f) {
  assert(isBoxed(f) && "derefPoL: not a HEAPOBJ");
  return derefHO(f.op);
}

void derefStgCurVal() {
  while (isBoxed(stgCurVal) && getObjType(stgCurVal.op) == INDIRECT) {
    stgCurVal = stgCurVal.op->payload[0];
  }
}

Obj *derefHO(Obj *op) {
  while (getObjType(op) == INDIRECT)
    op = op->payload[0].op;
  return op;
}

// stg_case_not_exhaustive = FUN( x ->  );
DEFUN2(stg_case_not_exhaustive, self, x) {
  fprintf(stderr, "stg_case_not_exhaustive: ");
  showStgVal(x);
  fprintf(stderr, "\n");
  showStgHeap();
  exit(0);
  ENDFUN;
}

InfoTab it_stg_case_not_exhaustive __attribute__((aligned(8))) = {
  .name = "stg_case_not_exhaustive",
  .entryCode = &stg_case_not_exhaustive,
  .objType = FUN,
  //  .fvCount = 0,
  .funFields.arity = 1,
  .layoutInfo.boxedCount = 0,
  .layoutInfo.unboxedCount = 0,
};

Obj sho_stg_case_not_exhaustive = {
#if USE_OBJTYPE
  .objType = FUN,
#endif
  .infoPtr = &it_stg_case_not_exhaustive,
  .ident = "stg_case_not_exhaustive",
};

DEFUN1(stg_funcall, self) {
  fprintf(stderr,"stg_funcall, returning self\n");
  stgCurVal = self;
  STGRETURN0();
  ENDFUN;
}

DEFUN1(stg_papcall, self) {
  fprintf(stderr,"top-level PAP call, returning self\n");
  stgCurVal = self;
  STGRETURN0();
  ENDFUN;
}

DEFUN1(stg_concall, self) {
  fprintf(stderr,"stg_concall, returning self\n");
  stgCurVal = self;
  STGRETURN0();
  ENDFUN;
}

DEFUN1(stgBlackhole, self) {
  fprintf(stderr, "stgBlackhole, exiting!\n");
  exit(0);
  ENDFUN;
}

// we can't use this until FVs are stashed first
InfoTab it_stgBlackhole __attribute__((aligned(8))) =
  { .name = "default stgBlackhole",
    //    .fvCount = 1, // self
    .entryCode = &stgBlackhole,
    .objType = BLACKHOLE,
    .layoutInfo.payloadSize = 1, // space for indirect
    .layoutInfo.boxedCount = 1,
    .layoutInfo.unboxedCount = 0,
  };

DEFUN1(stgIndirect, self) {
  fprintf(stderr,"stgIndirect, jumping through indirection\n");
  PtrOrLiteral next = self.op->payload[0];
  STGJUMP1(getInfoPtr(next.op)->entryCode, next);
  RETURN0();
  ENDFUN;
}

InfoTab it_stgIndirect __attribute__((aligned(8))) =
  { .name = "stgIndirect",
    // .fvCount = 1, // target of indirect
    .entryCode = &stgIndirect,
    .objType = INDIRECT,
    .layoutInfo.payloadSize = 1, // target of indirect
    .layoutInfo.boxedCount = 1,
    .layoutInfo.unboxedCount = 0,
  };

DEFUN0(stgUpdateCont) {
  Cont *contp = stgPopCont();
  assert(getContType(contp) == UPDCONT && "I'm not an UPDCONT!");
  PtrOrLiteral p = contp->payload[0];
  assert(isBoxed(p) && "not a HEAPOBJ!");
  fprintf(stderr, "stgUpdateCont updating\n  ");
  showStgObj(p.op);
  fprintf(stderr, "with\n  ");
  showStgObj(stgCurVal.op);
  if (getObjType(p.op) != BLACKHOLE) {
    fprintf(stderr, "but updatee is %s not a BLACKHOLE!\n", objTypeNames[getObjType(p.op)]);
    showStgHeap();
    assert(getObjType(p.op) == BLACKHOLE);
  }
  // the order of the following two operations is important for concurrency
  p.op->payload[0] = stgCurVal;
  // now zero out the payload

  p.op->infoPtr = &it_stgIndirect; // this will supersede the # if below

  // TOFIX--size determination should be more centralized
  size_t objSize = sizeof(Obj) + it_stgIndirect.layoutInfo.payloadSize * sizeof(PtrOrLiteral);
  objSize = ((objSize + 7)/8)*8;
  p.op->_objSize = objSize;
  strcpy( p.op->ident, it_stgIndirect.name );

#if USE_OBJTYPE
  p.op->objType = INDIRECT;
#else
  p.op->infoPtr = setLSB3(p.op->infoPtr);
#endif
  fprintf(stderr, "stgUpdateCont leaving...\n  ");
  STGRETURN0();
  ENDFUN
}

CInfoTab it_stgUpdateCont __attribute__((aligned(8))) =
  { .name = "default stgUpdateCont",
    //    .fvCount = 1, // self
    .entryCode = &stgUpdateCont,
    .contType = UPDCONT,
    .layoutInfo.payloadSize = 1, // self
    .layoutInfo.boxedCount = 1,
    .layoutInfo.unboxedCount = 0,
  };

DEFUN0(fun_stgShowResultCont) {
  fprintf(stderr,"done!\n");
  stgPopCont();  // clean up--normally the job of the returnee
  fprintf(stderr,"The answer is\n");
#if USE_ARGTYPE
  showStgVal(stgCurVal);
#else
  showStgObj(stgCurVal.op);
#endif
  RETURN0();
  ENDFUN;
}

CInfoTab it_stgShowResultCont __attribute__((aligned(8))) =
  { .name       = "fun_showResultCont",
    //    .fvCount    = 0,
    .entryCode  = &fun_stgShowResultCont,
    .contType    = CALLCONT,
    .layoutInfo.boxedCount = -1,  // shouldn't be using this
    .layoutInfo.unboxedCount = -1,  // shouldn't be using this
  };

void stgThunk(PtrOrLiteral self) {
  assert(isBoxed(self) && "stgThunk:  not HEAPOBJ\n");
  Cont *contp = stgAllocCont(&it_stgUpdateCont);
  contp->payload[0] = self;
  strcpy(contp->ident, self.op->ident); //override default
  // can't do this until we capture the variables in a stack frame
  // self.op->infoPtr = &it_stgBlackHole;
#if USE_OBJTYPE
  self.op->objType = BLACKHOLE;
#else
  self.op->infoPtr = setLSB2(self.op->infoPtr); // update bit to say this is a Blackhole
#endif

}

DEFUN0(stgCallCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgCallCont returning\n");
  RETURN0();  // fall back to the cmm trampoline
  ENDFUN;
}

CInfoTab it_stgCallCont __attribute__((aligned(8))) =
  { .name = "stgCallCont",
    //    .fvCount = 0,
    .entryCode = &stgCallCont,
    .contType = CALLCONT,
    .layoutInfo.boxedCount = -1,  // shouldn't be using this
    .layoutInfo.unboxedCount = -1,  // shouldn't be using this
  };

/*
void callContSave(int argc, PtrOrLiteral argv[]) {
  Cont *cc = stgAllocCallCont( &it_stgCallCont, argc );
  for (int i = 0; i != argc; i++) 
    cc->payload[i+1] = argv[i];
}
*/

void callContSave(PtrOrLiteral argv[], Bitmap64 layout) {
  int argc = BMSIZE(layout);
  Cont *cc = stgAllocCallCont( &it_stgCallCont, argc );
  cc->layout = layout;
  for (int i = 0; i != argc; i++) 
    cc->payload[i+1] = argv[i];
}

void callContRestore(PtrOrLiteral argv[]) {
  Cont *cc = stgPopCont();
  assert(getContType(cc) == CALLCONT);
#if USE_ARGTYPE
  assert(cc->payload[0].argType == INT);
#endif
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

