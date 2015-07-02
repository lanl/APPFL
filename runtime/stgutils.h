#ifndef stgutils_h
#define stgutils_h

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"

void callContSave(int argc, PtrOrLiteral argv[]);
void callContRestore(PtrOrLiteral argv[]);

Obj *derefHO(Obj *op);
Obj* derefPoL(PtrOrLiteral f);

// is this a good place to check for BLACKHOLE?
void derefStgCurVal();

FnPtr whiteHole();
FnPtr stg_constructorcall();

FnPtr stgShowResultCont();
Cont showResultCont;

FnPtr stgApply();
FnPtr stgApply1();

#define HOTOPL(HO) ((PtrOrLiteral) {.argType = HEAPOBJ, .op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.argType = INT,     .i = L   })

// NP = number of PtrOrLiterals NO = Number of Objs
#define STGHEAPAT(NP,NO) ((char*)stgHP - (NP*sizeof(PtrOrLiteral)) - (NO*sizeof(VarObj)))

// evaluate in place
#define STGEVAL(e)				\
do {						\
  stgCurVal = e;				\
  derefStgCurVal();				\
  while (stgCurVal.argType == HEAPOBJ &&	\
	 stgCurVal.op->objType == THUNK) {	\
    Cont cont = {.retAddr = &stgCallCont,	\
	              .objType = CALLCONT,	\
                      .payload[0] = {0}};	\
    strcpy(cont.ident, stgCurVal.op->ident);    \
    stgPushCont(cont);			        \
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


#endif
