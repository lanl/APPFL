#ifndef stgutils_h
#define stgutils_h

#include "stg.h"
#include "cmm.h"
#include "gc.h"
#include "string.h"
#include "options.h"
#include "log.h"
#include "heap.h"
#include "stack.h"

#include <stdlib.h>

extern void stgThunk(PtrOrLiteral self);


FnPtr stg_funcall();
FnPtr stg_papcall();
FnPtr stg_concall();
// THUNKS are individualized
FnPtr stgBlackhole();
FnPtr stgIndirect();

FnPtr stgCallCont();
extern CInfoTab cit_stgCallCont;

FnPtr stgStackCont();
extern CInfoTab cit_stgStackCont;
extern CInfoTab cit_stgLetCont;

FnPtr stgUpdateCont();
extern CInfoTab cit_stgUpdateCont;

FnPtr fun_stgShowResultCont();
extern CInfoTab cit_stgShowResultCont;

// this is not a real object, should not need an sho_, TODO fix CodeGen.hs
FnPtr stg_case_not_exhaustiveP();
extern Obj sho_stg_case_not_exhaustiveP;
FnPtr stg_case_not_exhaustiveN();
extern Obj sho_stg_case_not_exhaustiveN;

void stgCaseToPopMe(Cont *contp);

FnPtr fun_par();
extern InfoTab it_par;
extern Obj sho_par;


#if USE_ARGTYPE
#define HOTOPL(HO) ((PtrOrLiteral) {.argType = HEAPOBJ, .op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.argType = INT,     .i = L   })
#else
#define HOTOPL(HO) ((PtrOrLiteral) {.op = (HO) })
#define INTTOPL(L) ((PtrOrLiteral) {.i = L   })
#endif


#endif // ifndef stgutils_h
