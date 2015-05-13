#include "stg_header.h"
void registerSHOs();
FnPtr fun_main();
FnPtr fun_fac();
FnPtr alts_0();
FnPtr alts_1();
FnPtr fun_fac_h();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_alts_2_exhaust();
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
  };
InfoTab it_fac = 
  { .name                = "fac",
    .fvCount             = 0,
    .entryCode           = &fun_fac,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_res_0 = 
  { .name                = "res_0",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_five = 
  { .name                = "five",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_fac_h = 
  { .name                = "fac_h",
    .fvCount             = 0,
    .entryCode           = &fun_fac_h,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_2_exhaust = 
  { .name                = "alts_2_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_2_exhaust,
    .objType             = THUNK,
  };

extern Obj sho_main;
extern Obj sho_fac;
extern Obj sho_five;
extern Obj sho_fac_h;

Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };
Obj sho_fac =
{
  .infoPtr   = &it_fac,
  .objType   = FUN,
  .ident     = "fac",
  };
Obj sho_five =
{
  .infoPtr   = &it_five,
  .objType   = CON,
  .ident     = "five",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 5,
  };
Obj sho_fac_h =
{
  .infoPtr   = &it_fac_h,
  .objType   = FUN,
  .ident     = "fac_h",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_main;
  stgStatObj[stgStatObjCount++] = &sho_fac;
  stgStatObj[stgStatObjCount++] = &sho_five;
  stgStatObj[stgStatObjCount++] = &sho_fac_h;
}


DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // fac five
  STGAPPLY1(HOTOPL(&sho_fac), HOTOPL(&sho_five));
  STGRETURN0();
  ENDFUN;
}

DEFUN2(fun_fac, self, i) {
  fprintf(stderr, "fac here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      .objType = CASECONT,
      .ident = "CCont for alts_0",
      // no FVs
        });
  stgCurVal = i; // i
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_0 = stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  // I i_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_1,
      .objType = CASECONT,
      .ident = "CCont for alts_1",
      // no FVs
        });
  // fac_h i_h
  STGAPPLY1(HOTOPL(&sho_fac_h), scrut_alts_0.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  // unboxed scrutinee
  Cont ccont_alts_1 = stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // x_h ->
  Obj *res_0 = stgNewHeapObj();
  *res_0 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_res_0,
          .ident = "res_0",
          .payload[0] = scrut_alts_1, // x_h
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_0
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_fac_h, self, i_h) {
  fprintf(stderr, "fac_h here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      .objType = CASECONT,
      .ident = "CCont for alts_2",
      // load payload with FVs i_h
      .payload[0] = i_h, // i_h
    });
  stgCurVal.argType = BOOL;
  stgCurVal.i = (i_h).i == 0;
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  // unboxed scrutinee
  Cont ccont_alts_2 = stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  switch(stgCurVal.i) {
    // True_h  ->
    case 1: {
      stgCurVal = ((PtrOrLiteral){.argType = INT,    .i = 1 }); // 1
      STGRETURN0();
    }
    // False_h  ->
    case 0: {
      stgPushCont( (Cont)
        { .retAddr = &alts_3,
          .objType = CASECONT,
          .ident = "CCont for alts_3",
          // load payload with FVs i_h
          .payload[0] = ccont_alts_2.payload[0], // i_h
        });
      stgCurVal.argType = INT;
      stgCurVal.i = (ccont_alts_2.payload[0]).i - 1;
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_2_exhaust = stgNewHeapObj();
      *alts_2_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_2_exhaust,
              .ident = "alts_2_exhaust",
              .payload[0] = scrut_alts_2, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_2_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  // unboxed scrutinee
  Cont ccont_alts_3 = stgPopCont();
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // im1_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_4,
      .objType = CASECONT,
      .ident = "CCont for alts_4",
      // load payload with FVs i_h
      .payload[0] = ccont_alts_3.payload[0], // i_h
    });
  // fac_h im1_h
  STGAPPLY1(HOTOPL(&sho_fac_h), scrut_alts_3);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_4) {
  fprintf(stderr, "alts_4 here\n");
  // unboxed scrutinee
  Cont ccont_alts_4 = stgPopCont();
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  // f1_h ->
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_4.payload[0]).i * (scrut_alts_4).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN1(fun_alts_2_exhaust, self) {
  fprintf(stderr, "alts_2_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
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

