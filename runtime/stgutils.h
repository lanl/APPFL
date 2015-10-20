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

FnPtr whiteHole();
FnPtr stg_constructorcall();

FnPtr stgApply();

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
// evaluate in place
#if USE_ARGTYPE
#define STGEVAL(e)				\
do {						\
  stgCurVal = e;				\
  derefStgCurVal();				\
  assert(stgCurVal.argType == HEAPOBJ && "STGEVAL:  non-heap object before STGCALL!"); \
  if (stgCurVal.op->objType == THUNK) {	        \
    Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);	\
    while (stgCurVal.op->objType == THUNK) {	\
      strcpy(cont->ident, stgCurVal.op->ident);	\
      STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
      derefStgCurVal();				\
    }  						\
    stgPopCont();			        \
  }  						\
  assert(stgCurVal.argType == HEAPOBJ && "STGEVAL:  non-heap object after STGCALL!"); \
  if (stgCurVal.op->objType == BLACKHOLE) {     \
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
#define STGEVAL(e)        \
do {            \
  stgCurVal = e;        \
  derefStgCurVal();       \
  if (stgCurVal.op->objType == THUNK) {         \
    Obj* cont = stgAllocCallCont2(&it_stgCallCont, 0);  \
    while (stgCurVal.op->objType == THUNK) {  \
      strcpy(cont->ident, stgCurVal.op->ident); \
      STGCALL1(getInfoPtr(stgCurVal.op)->entryCode, stgCurVal); \
      derefStgCurVal();       \
    }             \
    stgPopCont();             \
  }             \
  if (stgCurVal.op->objType == BLACKHOLE) {     \
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


