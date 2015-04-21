#include "stg_header.h"
void registerSHOs();
FnPtr fun_multInt();
FnPtr alts_0();
FnPtr alts_1();
FnPtr alts_2();
FnPtr fun_main();
InfoTab it_three = 
  { .name                = "three",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_four = 
  { .name                = "four",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_multInt = 
  { .name                = "multInt",
    .fvCount             = 0,
    .entryCode           = &fun_multInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
  };

extern Obj sho_three;
extern Obj sho_four;
extern Obj sho_multInt;
extern Obj sho_main;

Obj sho_three =
{
  .infoPtr   = &it_three,
  .objType   = CON,
  .ident     = "three",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 3,
  };
Obj sho_four =
{
  .infoPtr   = &it_four,
  .objType   = CON,
  .ident     = "four",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 4,
  };
Obj sho_multInt =
{
  .infoPtr   = &it_multInt,
  .objType   = FUN,
  .ident     = "multInt",
  };
Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_four;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  Cont ccont_alts_0 = stgPopCont();
  stgPushCont( (Cont)
    { .retAddr = &alts_1,
      // load payload with FVs i
      .payload[0] = ccont_alts_0.payload[0],
    });
  stgCurVal = ccont_alts_0.payload[0];
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_1) {
  Cont ccont_alts_1 = stgPopCont();
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_1.payload[0]).i * (ccont_alts_1.payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_2) {
  Cont ccont_alts_2 = stgPopCont();
  Obj *result_0 = stgNewHeapObj();
  *result_0 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_result_0,
          .ident = "result_0",
          .payload[0] = ccont_alts_2.payload[0],
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1));
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN1(main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_multInt), HOTOPL(&sho_three), HOTOPL(&sho_four));
  STGRETURN0();
  ENDFUN;
}
DEFUN0(start) {
  registerSHOs();
  stgPushCont(showResultCont);
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

