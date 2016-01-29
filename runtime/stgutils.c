#include <inttypes.h>
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
  assert(isBoxed(stgCurVal)); // return stgCurVal
  while (getObjType(stgCurVal.op) == INDIRECT) { // return stgCurVal
    stgCurVal = stgCurVal.op->payload[0]; // return stgCurVal
    assert(isBoxed(stgCurVal)); // return stgCurVal
  }
}

Obj *derefHO(Obj *op) {
  while (getObjType(op) == INDIRECT) {
    PtrOrLiteral v = op->payload[0];
    assert(isBoxed(v));
    op = v.op;
  }
  return op;
}

// stg_case_not_exhaustiveP(self, x)
FnPtr stg_case_not_exhaustiveP() {
  Cont *stg_sc = stgGetStackArgp();
  //  PtrOrLiteral self = stg_sc->payload[0];
  PtrOrLiteral x    = stg_sc->payload[1];
  fprintf(stderr, "stg_case_not_exhaustive_boxed: ");
  showStgVal(x);
  fprintf(stderr, "\n");
  showStgHeap();
  exit(0);
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

// stg_case_not_exhaustiveN(self, x)
FnPtr stg_case_not_exhaustiveN() {
  Cont *stg_sc = stgGetStackArgp();
  //  PtrOrLiteral self = stg_sc->payload[0];
  PtrOrLiteral x    = stg_sc->payload[1];
  fprintf(stderr, "stg_case_not_exhaustive_unboxed: %" PRIx64 "\n", x.u);
  showStgHeap();
  exit(0);
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

FnPtr stg_funcall() {
  PRINTF("stg_funcall, returning self\n");
  STGRETURN0();
}

FnPtr stg_papcall() {
  PRINTF("top-level PAP call, returning self\n");
  STGRETURN0();
}

FnPtr stg_concall() {
  PRINTF("stg_concall, returning self\n");
  STGRETURN0();
}

FnPtr stgBlackhole() {
  PRINTF( "stgBlackhole, exiting!\n");
  exit(0);
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

FnPtr stgIndirect() {
  PRINTF("stgIndirect, jumping through indirection\n");
  stgCurVal = stgCurVal.op->payload[0]; // Obj takes self in stgCurVal
  STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);
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

FnPtr stgUpdateCont() {
  Cont *contp = stgGetStackArgp();
  assert(getContType(contp) == UPDCONT && "I'm not an UPDCONT!");
  PtrOrLiteral p = contp->payload[0];
  assert(isBoxed(p) && "not a HEAPOBJ!");
  PRINTF( "stgUpdateCont updating\n  ");
  showStgObj(p.op);
  PRINTF( "with\n  ");
  showStgObj(stgCurVal.op);
  if (getObjType(p.op) != BLACKHOLE) {
    PRINTF( "but updatee is %s not a BLACKHOLE!\n", 
	    objTypeNames[getObjType(p.op)]);
    showStgHeap();
    assert(getObjType(p.op) == BLACKHOLE);
  }

  int oldObjSize = getObjSize(p.op);

  // the order of the following two operations is important for concurrency
  p.op->payload[0] = stgCurVal;  // return stgCurVal, not the indirect, for efficiency
  p.op->_infoPtr = &it_stgIndirect;
#if USE_OBJTYPE
  p.op->objType = INDIRECT;
#endif
  strcpy( p.op->ident, it_stgIndirect.name );
  int newObjSize = getObjSize(p.op);
  assert(newObjSize <= oldObjSize);

  // this is for displaying a fragmented heap
  memset((char*)p.op+newObjSize, 0, oldObjSize-newObjSize);

  PRINTF("stgUpdateCont leaving...\n  ");
  stgPopCont();
  // this returns stgCurVal, not the indirect, for efficiency
  STGRETURN0();
}

CInfoTab it_stgUpdateCont __attribute__((aligned(8))) =
  { .name = "default stgUpdateCont",
    .entryCode = &stgUpdateCont,
    .contType = UPDCONT,
    .cLayoutInfo.payloadSize = 1, // self
    .cLayoutInfo.boxedCount = 1,
    .cLayoutInfo.unboxedCount = 0,
    //    .cLayoutInfo.bm = (Bitmap64) {.bitmap.mask = 0x1, 
    //				  .bitmap.size = 1},
    .cLayoutInfo.bm.bitmap.mask = 0x1, 
    .cLayoutInfo.bm.bitmap.size = 1,
  };

FnPtr fun_stgShowResultCont() {
  PRINTF("done!\n");
  stgPopCont();  // clean up--normally the job of the returnee
  fprintf(stderr, "The answer is\n");
#if USE_ARGTYPE
  showStgVal(stgCurVal); stgCurVal.op = NULL;
#else
  showStgObj(stgCurVal.op); stgCurVal.op = NULL;
#endif
  RETURN0();
}

CInfoTab it_stgShowResultCont __attribute__((aligned(8))) =
  { .name       = "fun_showResultCont",
    //    .fvCount    = 0,
    .entryCode  = &fun_stgShowResultCont,
    .contType    = CALLCONT,
    .cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

void stgThunk(PtrOrLiteral self) {
  assert(isBoxed(self) && "stgThunk:  not HEAPOBJ\n");
  Cont *contp = stgAllocCont(&it_stgUpdateCont);
  contp->payload[0] = self;
  strcpy(contp->ident, self.op->ident); //override default
  // can't do this until we capture the variables in a stack frame
  // self.op->infoPtr = &it_stgBlackHole;
  PRINTF( "BLACKHOLING %s\n", self.op->ident);
#if USE_OBJTYPE
  self.op->objType = BLACKHOLE;
#endif
  self.op->_infoPtr = setLSB2(self.op->_infoPtr); // this is a Blackhole
  assert(getObjType(self.op) == BLACKHOLE);
}

FnPtr stgStackCont() {
  PRINTF("stgStackCont returning\n");
  stgPopCont();
  STGRETURN0();  // return through continuation stack
}

CInfoTab it_stgStackCont __attribute__((aligned(8))) =
  { .name = "stgStackCont",
    .entryCode = &stgStackCont,
    .contType = STACKCONT,
    .cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

CInfoTab it_stgLetCont __attribute__((aligned(8))) =
  { .name = "stgStackCont",
    .entryCode = &stgStackCont,
    .contType = LETCONT,
    .cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

FnPtr stgPopMeCont() {
  PRINTF("stgPopMeCont returning\n");
  stgPopCont();
  STGRETURN0();  // return through continuation stack
}

void stgPopContIfPopMe() {
  if (getContType(stgGetStackArgp()) == POPMECONT)
    stgPopCont();
}

CInfoTab it_stgPopMeCont __attribute__((aligned(8))) =
  { .name = "stgPopMeCont",
    .entryCode = &stgPopMeCont,
    .contType = POPMECONT,
    .cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

void stgCaseToPopMe(Cont *contp) {
  assert(contp->contType == CASECONT);
  contp->cInfoPtr = &it_stgPopMeCont;
  contp->entryCode = it_stgPopMeCont.entryCode;
  contp->contType = it_stgPopMeCont.contType;
  // keep contp->ident unchanged
}

FnPtr stgCallCont() {
  stgPopCont();
  PRINTF("stgCallCont returning\n");
  RETURN0();  // fall back to the cmm trampoline
}

CInfoTab it_stgCallCont __attribute__((aligned(8))) =
  { .name = "stgCallCont",
    .entryCode = &stgCallCont,
    .contType = CALLCONT,
    .cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

