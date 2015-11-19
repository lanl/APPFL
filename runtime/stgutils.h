#ifndef stgutils_h
#define stgutils_h

#include "stg.h"
#include "cmm.h"
#include "gc.h"
#include "string.h"
#include "options.h"

extern void stgThunk(PtrOrLiteral self);

// FnPtr stgCallCont();
extern InfoTab it_stgCallCont;

// FnPtr stgUpdateCont();
extern InfoTab it_stgUpdateCont;

// FnPtr fun_stgShowResultCont();
extern InfoTab it_stgShowResultCont;

void callContSave(int argc, PtrOrLiteral argv[]);
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

/*
// evaluate in place
#define STGEVALworks(e)				\
do {						\
  stgCurVal = e;				\
  derefStgCurVal();				\
  while (stgCurVal.argType == HEAPOBJ &&	\
	 stgCurVal.op->objType == THUNK) {	\
    Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);	\
    strcpy(cont->ident, stgCurVal.op->ident);	\
    STGCALL1(stgCurVal.op->infoPtr->entryCode, stgCurVal); \
    stgPopCont();			        \
    derefStgCurVal();				\
  }						\
  if (stgCurVal.argType == HEAPOBJ &&           \
      stgCurVal.op->objType == BLACKHOLE) {     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n"); \
    showStgVal(stgCurVal);			\
    fprintf(stderr, "\n");			\
    showStgHeap();			        \
    exit(0);                                    \
  }                                             \
} while (0)
*/

// evaluate IN PLACE, this should probably only happen in stgApply
#define STGEVAL(e)					    \
do {							    \
  stgCurVal = e;					    \
  Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);	    \
  strcpy(cont->ident, stgCurVal.op->ident);		    \
  STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
  stgPopCont();						    \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {		     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n"); \
    showStgVal(stgCurVal);				     \
    assert(false);					     \
  }									\
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

#if 0
#define STGEVAL(e)				\
do {						\
  stgCurVal = e;				\
  Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);	\
  strcpy(cont->ident, stgCurVal.op->ident);	\
  STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
  stgPopCont();			        \
  derefStgCurVal();					     \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n"); \
    showStgVal(stgCurVal);			\
    fprintf(stderr, "\n");			\
    showStgHeap();			        \
    assert(false);				\
  }                                             \
  assert (cmmSP == cmmStack + cmmStackSize && "Non empty cmm stack in stgeval");\
  GC();					\
} while (0)
#endif


#if USE_ARGTYPE
#define STGEVAL_works(e)				\
do {						\
  stgCurVal = e;				\
  derefStgCurVal();				\
  if (getObjType(stgCurVal.op) == THUNK) {	        \
    Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);	\
    while (getObjType(stgCurVal.op) == THUNK) {	\
      strcpy(cont->ident, stgCurVal.op->ident);	\
      STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
      derefStgCurVal();				\
    }  						\
    stgPopCont();			        \
  }  						\
  if (getObjType(stgCurVal.op) == BLACKHOLE) {     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n"); \
    showStgVal(stgCurVal);			\
    fprintf(stderr, "\n");			\
    showStgHeap();			        \
    exit(0);                                    \
  }                                             \
  assert (cmmSP == cmmStack + cmmStackSize && "Non empty cmm stack in stgeval");\
  GC();					\
} while (0)
#else
#define STGEVAL_works(e)        \
do {            \
  stgCurVal = e;        \
  derefStgCurVal();       \
  if (getObjType(stgCurVal.op) == THUNK) {         \
    Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);  \
    while (getObjType(stgCurVal.op) == THUNK) {  \
      strcpy(cont->ident, stgCurVal.op->ident); \
      STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
      derefStgCurVal();       \
    }             \
    stgPopCont();             \
  }             \
  if (getObjType(stgCurVal.op) == BLACKHOLE) {     \
    fprintf(stderr, "infinite loop detected in STGEVAL!\n"); \
    showStgVal(stgCurVal);      \
    fprintf(stderr, "\n");      \
    showStgHeap();              \
    exit(0);                                    \
  }                                             \
  assert (cmmSP == cmmStack + cmmStackSize && "Non empty cmm stack in stgeval");\
  GC();         \
} while (0)
#endif




#endif


