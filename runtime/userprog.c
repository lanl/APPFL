#include "stg_header.h"
void registerSHOs();
FnPtr fun_fac_h();
FnPtr alts_0();
FnPtr alts_1();
FnPtr alts_2();
FnPtr alts_3();
FnPtr fun_fac();
FnPtr alts_4();
FnPtr alts_5();
FnPtr fun_main();
InfoTab it_fac_h = 
  { .name                = "fac_h",
    .fvCount             = 0,
    .entryCode           = &fun_fac_h,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_0 = 
  { .name                = "alts_0",
    .fvCount             = 1,
    .entryCode           = &alts_0,
  };
InfoTab it_alts_1 = 
  { .name                = "alts_1",
    .fvCount             = 1,
    .entryCode           = &alts_1,
  };
InfoTab it_alts_2 = 
  { .name                = "alts_2",
    .fvCount             = 1,
    .entryCode           = &alts_2,
  };
InfoTab it_alts_3 = 
  { .name                = "alts_3",
    .fvCount             = 0,
    .entryCode           = &alts_3,
  };
InfoTab it_fac = 
  { .name                = "fac",
    .fvCount             = 0,
    .entryCode           = &fun_fac,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_4 = 
  { .name                = "alts_4",
    .fvCount             = 0,
    .entryCode           = &alts_4,
  };
InfoTab it_alts_5 = 
  { .name                = "alts_5",
    .fvCount             = 0,
    .entryCode           = &alts_5,
  };
InfoTab it_res_0 = 
  { .name                = "res_0",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_i12 = 
  { .name                = "i12",
    .fvCount             = 0,
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

extern Obj sho_fac_h;
extern Obj sho_fac;
extern Obj sho_i12;
extern Obj sho_main;

Obj sho_fac_h =
{
  .infoPtr   = &it_fac_h,
  .objType   = FUN,
  .ident     = "fac_h",
  };
Obj sho_fac =
{
  .infoPtr   = &it_fac,
  .objType   = FUN,
  .ident     = "fac",
  };
Obj sho_i12 =
{
  .infoPtr   = &it_i12,
  .objType   = CON,
  .ident     = "i12",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 12,
  };
Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_fac_h;
  stgStatObj[stgStatObjCount++] = &sho_fac;
  stgStatObj[stgStatObjCount++] = &sho_i12;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


DEFUN2(fun_fac_h, self, n_h) {
  fprintf(stderr, "fac_h here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      .objType = CASECONT,
      .ident = "CCont for alts_0",
      // load payload with FVs n_h
      .payload[0] = n_h, // n_h
    });
  stgCurVal = n_h; // n_h
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_0 = stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // 0  ->
    case 0: {
      stgCurVal = ((PtrOrLiteral){.argType = INT,    .i = 1 }); // 1
      STGRETURN0();
    }
    // m_h ->
    default: {
      stgPushCont( (Cont)
        { .retAddr = &alts_1,
          .objType = CASECONT,
          .ident = "CCont for alts_1",
          // load payload with FVs n_h
          .payload[0] = ccont_alts_0.payload[0], // n_h
        });
      stgCurVal.argType = INT;
      stgCurVal.i = (ccont_alts_0.payload[0]).i - 1;
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  // unboxed scrutinee
  Cont ccont_alts_1 = stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // nm1_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      .objType = CASECONT,
      .ident = "CCont for alts_2",
      // load payload with FVs n_h
      .payload[0] = ccont_alts_1.payload[0], // n_h
    });
  // fac_h nm1_h
  STGAPPLY1(HOTOPL(&sho_fac_h), scrut_alts_1);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  // unboxed scrutinee
  Cont ccont_alts_2 = stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // fnm1_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_3,
      .objType = CASECONT,
      .ident = "CCont for alts_3",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_2.payload[0]).i * (scrut_alts_2).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  // unboxed scrutinee
  stgPopCont();
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // res_h ->
  stgCurVal = scrut_alts_3; // res_h
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_fac, self, n) {
  fprintf(stderr, "fac here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_4,
      .objType = CASECONT,
      .ident = "CCont for alts_4",
      // no FVs
        });
  stgCurVal = n; // n
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_4) {
  fprintf(stderr, "alts_4 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  // I n_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_5,
      .objType = CASECONT,
      .ident = "CCont for alts_5",
      // no FVs
        });
  // fac_h n_h
  STGAPPLY1(HOTOPL(&sho_fac_h), scrut_alts_4.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // res_h ->
  Obj *res_0 = stgNewHeapObj();
  *res_0 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_res_0,
          .ident = "res_0",
          .payload[0] = scrut_alts_5, // res_h
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_0
  STGRETURN0();
  ENDFUN;
}


DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // fac i12
  STGAPPLY1(HOTOPL(&sho_fac), HOTOPL(&sho_i12));
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

