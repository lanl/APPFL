#include "stg_header.h"
void registerSHOs();
FnPtr fun_fac_h();
FnPtr alts_23();
FnPtr alts_24();
FnPtr alts_25();
FnPtr alts_26();
FnPtr fun_fac();
FnPtr alts_27();
FnPtr alts_28();
FnPtr fun_main();
InfoTab it_fac_h = 
  { .name                = "fac_h",
    .fvCount             = 0,
    .entryCode           = &fun_fac_h,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_23 = 
  { .name                = "alts_23",
    .fvCount             = 1,
    .entryCode           = &alts_23,
  };
InfoTab it_alts_24 = 
  { .name                = "alts_24",
    .fvCount             = 1,
    .entryCode           = &alts_24,
  };
InfoTab it_alts_25 = 
  { .name                = "alts_25",
    .fvCount             = 1,
    .entryCode           = &alts_25,
  };
InfoTab it_alts_26 = 
  { .name                = "alts_26",
    .fvCount             = 0,
    .entryCode           = &alts_26,
  };
InfoTab it_fac = 
  { .name                = "fac",
    .fvCount             = 0,
    .entryCode           = &fun_fac,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_27 = 
  { .name                = "alts_27",
    .fvCount             = 0,
    .entryCode           = &alts_27,
  };
InfoTab it_alts_28 = 
  { .name                = "alts_28",
    .fvCount             = 0,
    .entryCode           = &alts_28,
  };
InfoTab it_res_1 = 
  { .name                = "res_1",
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
    { .retAddr = &alts_23,
      .objType = CASECONT,
      .ident = "CCont for alts_23",
      // load payload with FVs n_h
      .payload[0] = n_h, // n_h
    });
  stgCurVal = n_h; // n_h
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_23) {
  fprintf(stderr, "alts_23 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_23 = stgPopCont();
  PtrOrLiteral scrut_alts_23 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // 0  ->
    case 0: {
      stgCurVal = ((PtrOrLiteral){.argType = INT,    .i = 1 }); // 1
      STGRETURN0();
    }
    // m_h ->
    default: {
      stgPushCont( (Cont)
        { .retAddr = &alts_24,
          .objType = CASECONT,
          .ident = "CCont for alts_24",
          // load payload with FVs n_h
          .payload[0] = ccont_alts_23.payload[0], // n_h
        });
      stgCurVal.argType = INT;
      stgCurVal.i = (ccont_alts_23.payload[0]).i - 1;
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_24) {
  fprintf(stderr, "alts_24 here\n");
  // unboxed scrutinee
  Cont ccont_alts_24 = stgPopCont();
  PtrOrLiteral scrut_alts_24 = stgCurVal;
  // nm1_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_25,
      .objType = CASECONT,
      .ident = "CCont for alts_25",
      // load payload with FVs n_h
      .payload[0] = ccont_alts_24.payload[0], // n_h
    });
  // fac_h nm1_h
  STGAPPLY1(HOTOPL(&sho_fac_h), scrut_alts_24);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_25) {
  fprintf(stderr, "alts_25 here\n");
  // unboxed scrutinee
  Cont ccont_alts_25 = stgPopCont();
  PtrOrLiteral scrut_alts_25 = stgCurVal;
  // fnm1_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_26,
      .objType = CASECONT,
      .ident = "CCont for alts_26",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_25.payload[0]).i * (scrut_alts_25).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_26) {
  fprintf(stderr, "alts_26 here\n");
  // unboxed scrutinee
  stgPopCont();
  PtrOrLiteral scrut_alts_26 = stgCurVal;
  // res_h ->
  stgCurVal = scrut_alts_26; // res_h
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_fac, self, n) {
  fprintf(stderr, "fac here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_27,
      .objType = CASECONT,
      .ident = "CCont for alts_27",
      // no FVs
        });
  stgCurVal = n; // n
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_27) {
  fprintf(stderr, "alts_27 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_27 = stgCurVal;
  // I n_h ->
  stgPushCont( (Cont)
    { .retAddr = &alts_28,
      .objType = CASECONT,
      .ident = "CCont for alts_28",
      // no FVs
        });
  // fac_h n_h
  STGAPPLY1(HOTOPL(&sho_fac_h), scrut_alts_27.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_28) {
  fprintf(stderr, "alts_28 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_28 = stgCurVal;
  // res_h ->
  Obj *res_1 = stgNewHeapObj();
  *res_1 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_res_1,
          .ident = "res_1",
          .payload[0] = scrut_alts_28, // res_h
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_1
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
  return 0;
}

