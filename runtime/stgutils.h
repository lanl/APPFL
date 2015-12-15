#ifndef stgutils_h
#define stgutils_h

#include "stg.h"
#include "cmm.h"
#include "gc.h"
#include "string.h"
#include "options.h"

extern void stgThunk(PtrOrLiteral self);

void callContSave(PtrOrLiteral argv[], Bitmap64 layout);
void callContRestore(PtrOrLiteral argv[]);

Obj *derefHO(Obj *op);
Obj *derefPoL(PtrOrLiteral f);

// is this a good place to check for BLACKHOLE?
void derefStgCurVal();
void pushargs(int argc, PtrOrLiteral argv[]);
void popargs(int argc, PtrOrLiteral argv[]);
void copyargs(PtrOrLiteral *dest, const PtrOrLiteral *src, int count);

FnPtr stg_funcall();
FnPtr stg_papcall();
FnPtr stg_concall();
// THUNKS are individualized
FnPtr stgBlackhole();
FnPtr stgIndirect();

// this is not a real object, should not need an sho_, TODO fix CodeGen.hs
FnPtr stg_case_not_exhaustive();
extern Obj sho_stg_case_not_exhaustive;

#if USE_ARGTYPE
#define HOTOPL(HO) ((PtrOrLiteral) {.argType = HEAPOBJ, .op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.argType = INT,     .i = L   })
#else
#define HOTOPL(HO) ((PtrOrLiteral) {.op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.i = L   })
#endif

// NP = number of PtrOrLiterals NO = Number of Objs
#define STGHEAPAT(NP,NO) ((char*)stgHP - (NP*sizeof(PtrOrLiteral)) - (NO*sizeof(Obj)))

// evaluate IN PLACE, this should probably only happen in stgApply
// in which case there's some redundancy in pushing CALLCONTs
#define STGEVAL(e)					    \
do {							    \
  stgCurVal = e;					    \
  STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {		     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n"); \
    showStgVal(stgCurVal);				     \
    assert(false);					     \
  }							     \
  if (getObjType(stgCurVal.op) == THUNK) {		     \
    fprintf(stderr, "THUNK at end of STGEVAL!\n");	     \
    showStgVal(stgCurVal);				     \
    assert(false);					     \
  }							     \
  assert (cmmSP == cmmStack + cmmStackSize && "Non empty cmm stack in stgeval");\
  GC();					\
} while (0)

// bye bye!
#define STGJUMP()						     \
  do {								     \
  GC();								     \
  derefStgCurVal();						     \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {			     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n");	     \
    showStgVal(stgCurVal);					     \
    assert(false);						     \
  }								     \
  STGJUMP1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal);	     \
} while (0)


#endif


