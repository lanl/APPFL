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

FnPtr stgShowResultCont();
Obj sho_stgShowResultCont;

FnPtr stgApply();
FnPtr stgApply1();

#define HOTOPL(HO) ((PtrOrLiteral) {.argType = HEAPOBJ, .op = HO })
#define INTTOPL(L) ((PtrOrLiteral) {.argType = INT,     .i = L   })

#define STGHEAPAT(n) ((Obj*)stgHP + (n))

#endif
