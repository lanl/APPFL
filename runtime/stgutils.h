#ifndef stgutils_h
#define stgutils_h

#include "stg.h"
#include "cmm.h"
#include "gc.h"
#include "string.h"
#include "options.h"
#include "log.h"

#include <stdlib.h>

extern void stgThunk(PtrOrLiteral self);

Obj *derefHO(Obj *op);
Obj *derefPoL(PtrOrLiteral f);

void derefStgCurVal();

FnPtr stg_funcall();
FnPtr stg_papcall();
FnPtr stg_concall();
// THUNKS are individualized
FnPtr stgBlackhole();
FnPtr stgIndirect();

// this is not a real object, should not need an sho_, TODO fix CodeGen.hs
FnPtr stg_case_not_exhaustiveP();
extern Obj sho_stg_case_not_exhaustiveP;
FnPtr stg_case_not_exhaustiveN();
extern Obj sho_stg_case_not_exhaustiveN;

void stgCaseToPopMe(Cont *contp);
void stgPopContIfPopMe();

#if USE_ARGTYPE
#define HOTOPL(HO) ((PtrOrLiteral) {.argType = HEAPOBJ, .op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.argType = INT,     .i = L   })
#else
#define HOTOPL(HO) ((PtrOrLiteral) {.op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.i = L   })
#endif

// NP = number of PtrOrLiterals NO = Number of Objs
#define STGHEAPAT(NP,NO) ((char*)stgHP - (NP*sizeof(PtrOrLiteral)) - (NO*sizeof(Obj)))

// evaluate Object (not actual function) IN PLACE,
// this should probably only happen in stgApply
#define STGEVAL(e)					     \
  do {							     \
  stgCurVal = e;					     \
  Cont *callCont = stgAllocCallOrStackCont(&it_stgCallCont, 0);     \
  callCont->layout.bits = 0x0UL;			     \
  STGCALL0(getInfoPtr(stgCurVal.op)->entryCode);	     \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {		     \
    LOG(LOG_ERROR, "STGEVAL terminating on BLACKHOLE\n");   \
    showStgVal(LOG_ERROR, stgCurVal);			     \
    exit(0);						     \
  }							     \
  if (getObjType(stgCurVal.op) == THUNK) {		     \
    LOG(LOG_ERROR, "THUNK at end of STGEVAL!\n");	     \
    showStgVal(LOG_ERROR, stgCurVal);			     \
    assert(false);					     \
  }							     \
  GC();							     \
} while (0)

// bye bye!
#define STGJUMP()						     \
  do {								     \
  GC();								     \
  derefStgCurVal();						     \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {			     \
    LOG(LOG_ERROR, "STGJUMP terminating on BLACKHOLE\n");	     \
    showStgVal(LOG_ERROR, stgCurVal);				     \
    exit(0);							     \
  }								     \
  STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);		     \
} while (0)

#endif
