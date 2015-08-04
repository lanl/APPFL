#include "stgc.h"
void registerSHOs();
FnPtr fun_apply();
FnPtr fun_const();
FnPtr fun_main();
InfoTab it_apply = 
  { .name                = "apply",
    .fvCount             = 0,
    .entryCode           = &fun_apply,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_const = 
  { .name                = "const",
    .fvCount             = 0,
    .entryCode           = &fun_const,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_true = 
  { .name                = "true",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 0,
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "True",
  };
InfoTab it_twentytwo = 
  { .name                = "twentytwo",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 1,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
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
  .payload = {
    },
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_apply;
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho_twentytwo;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// forall t5,t6.(t6 -> t5) -> t6 -> t5
// ((["x","f"],[]),([t6,t6 -> t5],[]))
DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  // f x
  STGAPPLY1(f, x);
  fprintf(stderr, "apply returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t1,t2.t2 -> t1 -> t2
// ((["y","x"],[]),([t1,t2],[]))
DEFUN3(fun_const, self, x, y) {
  fprintf(stderr, "const here\n");
  stgCurVal = x; // x
  STGEVAL(stgCurVal);
  fprintf(stderr, "const returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // apply const true twentytwo
  STGAPPLY3(HOTOPL(&sho_apply), HOTOPL(&sho_const), HOTOPL(&sho_true), HOTOPL(&sho_twentytwo));
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
  initStg();
  initCmm();
  initGc();
  CALL0_0(start);
  showStgHeap();
  return 0;
}

