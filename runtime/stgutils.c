#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "stgutils.h"
#include "stg.h"
#include "cmm.h"
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

/*
// stg_case_not_exhaustive = FUN( x ->  );
DEFUN2(stg_case_not_exhaustive, self, x) {
  fprintf(stderr, "stg_case_not_exhaustive: ");
  showStgVal(x);
  fprintf(stderr, "\n");
  showStgHeap();
  exit(0);
  ENDFUN;
}
*/


DEFUNS2(stg_case_not_exhaustiveP, self, x) {
  fprintf(stderr, "stg_case_not_exhaustive_boxed: ");
  showStgVal(x);
  fprintf(stderr, "\n");
  showStgHeap();
  exit(0);
  ENDFUN;
}

InfoTab it_stg_case_not_exhaustiveP __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "stg_case_not_exhaustiveP",
  .entryCode = &stg_case_not_exhaustiveP,
  .objType = FUN,
  //  .fvCount = 0,
  .funFields.arity = 1,
  .layoutInfo.boxedCount = 0,
  .layoutInfo.unboxedCount = 0,
};

Obj sho_stg_case_not_exhaustiveP = {
#if USE_OBJTYPE
  .objType = FUN,
#endif
  ._infoPtr = &it_stg_case_not_exhaustiveP,
  .ident = "stg_case_not_exhaustiveP",
};

DEFUNS2(stg_case_not_exhaustiveN, self, x) {
  fprintf(stderr, "stg_case_not_exhaustive_unboxed: %lux\n", x.u);
  showStgHeap();
  exit(0);
  ENDFUN;
}

InfoTab it_stg_case_not_exhaustiveN __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "stg_case_not_exhaustiveN",
  .entryCode = &stg_case_not_exhaustiveN,
  .objType = FUN,
  //  .fvCount = 0,
  .funFields.arity = 1,
  .layoutInfo.boxedCount = 0,
  .layoutInfo.unboxedCount = 0,
};

Obj sho_stg_case_not_exhaustiveN = {
#if USE_OBJTYPE
  .objType = FUN,
#endif
  ._infoPtr = &it_stg_case_not_exhaustiveN,
  .ident = "stg_case_not_exhaustiveN",
};

DEFOBJ(stg_funcall) {
  fprintf(stderr,"stg_funcall, returning self\n");
  STGRETURN0();
  ENDFUN;
}

DEFOBJ(stg_papcall) {
  fprintf(stderr,"top-level PAP call, returning self\n");
  STGRETURN0();
  ENDFUN;
}

DEFOBJ(stg_concall) {
  fprintf(stderr,"stg_concall, returning self\n");
  STGRETURN0();
  ENDFUN;
}

DEFOBJ(stgBlackhole) {
  fprintf(stderr, "stgBlackhole, exiting!\n");
  exit(0);
  ENDFUN;
}

// we can't use this until FVs are stashed first
InfoTab it_stgBlackhole __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "default stgBlackhole",
  //    .fvCount = 1, // self
  .entryCode = &stgBlackhole,
  .objType = BLACKHOLE,
  .layoutInfo.payloadSize = 1, // space for indirect
  .layoutInfo.boxedCount = 1,
  .layoutInfo.unboxedCount = 0,
};

DEFOBJ(stgIndirect) {
  fprintf(stderr,"stgIndirect, jumping through indirection\n");
  stgCurVal = stgCurVal.op->payload[0];
  STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);
  ENDFUN;
}


InfoTab it_stgIndirect __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "stgIndirect",
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
    fprintf(stderr, "but updatee is %s not a BLACKHOLE!\n", 
	    objTypeNames[getObjType(p.op)]);
    showStgHeap();
    assert(getObjType(p.op) == BLACKHOLE);
  }

  int oldObjSize = getObjSize(p.op);

  // the order of the following two operations is important for concurrency
  p.op->payload[0] = stgCurVal;
  p.op->_infoPtr = &it_stgIndirect;
#if USE_OBJTYPE
  p.op->objType = INDIRECT;
#endif
  strcpy( p.op->ident, it_stgIndirect.name );
  int newObjSize = getObjSize(p.op);
  assert(newObjSize <= oldObjSize);
  memset((char*)p.op+newObjSize, 0, oldObjSize-newObjSize);
  fprintf(stderr, "stgUpdateCont leaving...\n  ");
  STGRETURN0();
  ENDFUN;
}

CInfoTab it_stgUpdateCont __attribute__((aligned(8))) =
  { .name = "default stgUpdateCont",
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
  fprintf(stderr, "BLACKHOLING %s\n", self.op->ident);
#if USE_OBJTYPE
  self.op->objType = BLACKHOLE;
#endif
  self.op->_infoPtr = setLSB2(self.op->_infoPtr); // this is a Blackhole
  assert(getObjType(self.op) == BLACKHOLE);
}

// self is stgCurVal
void stgThunkSelf() {
  assert(isBoxed(stgCurVal) && "stgThunk:  not HEAPOBJ\n");
  Cont *contp = stgAllocCont(&it_stgUpdateCont);
  contp->payload[0] = stgCurVal;
  strcpy(contp->ident, stgCurVal.op->ident); //override default
  // can't do this until we capture the variables in a stack frame
  // stgCurVal.op->infoPtr = &it_stgBlackHole;
  fprintf(stderr, "BLACKHOLING %s\n", stgCurVal.op->ident);
#if USE_OBJTYPE
  stgCurVal.op->objType = BLACKHOLE;
#endif
  stgCurVal.op->_infoPtr = setLSB2(stgCurVal.op->_infoPtr); // this is a Blackhole
  assert(getObjType(stgCurVal.op) == BLACKHOLE);
}

DEFUN0(stgStackCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgStackCont returning\n");
  RETURN0();  // fall back to the cmm trampoline
  ENDFUN;
}

CInfoTab it_stgStackCont __attribute__((aligned(8))) =
  { .name = "stgStackCont",
    .entryCode = &stgStackCont,
    .contType = STACKCONT,
    .layoutInfo.boxedCount = -1,  // shouldn't be using this
    .layoutInfo.unboxedCount = -1,  // shouldn't be using this
  };

DEFUN0(stgCallCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgCallCont returning\n");
  RETURN0();  // fall back to the cmm trampoline
  ENDFUN;
}

CInfoTab it_stgCallCont __attribute__((aligned(8))) =
  { .name = "stgCallCont",
    .entryCode = &stgCallCont,
    .contType = CALLCONT,
    .layoutInfo.boxedCount = -1,  // shouldn't be using this
    .layoutInfo.unboxedCount = -1,  // shouldn't be using this
  };

// ****************************************************************
// stgApply 

void popFrameArgs(int argc, PtrOrLiteral argv[]) {
  Cont *cp = stgPopCont();
  assert(getContType(cp) == STACKCONT);
  assert(argc == BMSIZE(cp->layout));  // only current use case
  memcpy(argv, cp->payload, argc * sizeof(PtrOrLiteral));
}

void copyargs(PtrOrLiteral *dest, const PtrOrLiteral *src, int count) {
  for (int i = 0; i != count; i++) dest[i] = src[i];
}

