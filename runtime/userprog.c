#include "stgc.h"
#include "stgApply.h"
void registerSHOs();
FnPtr fun_apply();
FnPtr fun_const();
FnPtr fun_main();
InfoTab it_apply __attribute__((aligned(8))) = 
  { .name                = "apply",
    // fvs []
    .entryCode           = &fun_apply,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_const __attribute__((aligned(8))) = 
  { .name                = "const",
    // fvs []
    .entryCode           = &fun_const,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_true __attribute__((aligned(8))) = 
  { .name                = "true",
    // fvs []
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "True",
  };
InfoTab it_twentytwo __attribute__((aligned(8))) = 
  { .name                = "twentytwo",
    // fvs []
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_main __attribute__((aligned(8))) = 
  { .name                = "main",
    // fvs []
    .entryCode           = &fun_main,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };

extern Obj sho_apply;
extern Obj sho_const;
extern Obj sho_true;
extern Obj sho_twentytwo;
extern Obj sho_main;

Obj sho_apply =
{
  .infoPtr   = &it_apply,
  .objType   = FUN,
  .ident     = "apply",
  .payload = {
    },
};

Obj sho_const =
{
  .infoPtr   = &it_const,
  .objType   = FUN,
  .ident     = "const",
  .payload = {
    },
};

Obj sho_true =
{
  .infoPtr   = &it_true,
  .objType   = CON,
  .ident     = "true",
  .payload = {
    },
};

Obj sho_twentytwo =
{
  .infoPtr   = &it_twentytwo,
  .objType   = CON,
  .ident     = "twentytwo",
  .payload = {
    {.argType = INT, .i = 22},
},
};

Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  .payload = {0}
};

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_apply;
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho_twentytwo;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// forall t5,t6.(t6 -> t5) -> t6 -> t5
// ((["f","x"],[]),([t6 -> t5,t6],[]))
DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  // INDIRECT TAIL CALL f x
  STGAPPLYP(f, x);
  fprintf(stderr, "apply returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t1,t2.t2 -> t1 -> t2
// ((["x","y"],[]),([t2,t1],[]))
DEFUN3(fun_const, self, x, y) {
  fprintf(stderr, "const here\n");
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "const returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL apply const true twentytwo
  STGAPPLYPPP(HOTOPL(&sho_apply), HOTOPL(&sho_const), HOTOPL(&sho_true), HOTOPL(&sho_twentytwo));
  fprintf(stderr, "main returning\n");
  STGRETURN0();
  ENDFUN;
}

DEFUN0(start) {
  registerSHOs();
  Obj *showResultCont = stgAllocCallCont2(&it_stgShowResultCont, 0);
  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));
  STGRETURN0();
  ENDFUN;
}

int main (int argc, char **argv) {
  parseArgs(argc, argv);
  initStg();
  initCmm();
  initGc();
  CALL0_0(start);
  showStgHeap();
  GC();
  return 0;
}

