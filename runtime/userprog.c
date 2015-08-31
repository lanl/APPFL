#include "stgc.h"
#include "stgApply.h"
void registerSHOs();
FnPtr fun_f();
FnPtr alts_0();
FnPtr alts_1();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr alts_5();
FnPtr fun_main();
InfoTab it_nine = 
  { .name                = "nine",
    // fvs []
    .fvCount             = 0,
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
InfoTab it_seven = 
  { .name                = "seven",
    // fvs []
    .fvCount             = 0,
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
InfoTab it_t1 = 
  { .name                = "t1",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 4,
    // argPerm = [2,0,3,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 2,
    .layoutInfo.permString   = "2031",
    .conFields.arity     = 4,
    .conFields.tag       = 0,
    .conFields.conName   = "TupUBUB",
  };
InfoTab it_f = 
  { .name                = "f",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_f,
    .objType             = THUNK,
    .layoutInfo.payloadSize  = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_0 = 
  { .name                = "alts_0",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_0,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_1 = 
  { .name                = "alts_1",
    // fvs [("b9",Int[B] ),("u6",Int_h[U] ),("u8",Int_h[U] )]
    .fvCount             = 3,
    .entryCode           = &alts_1,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 2,
  };
InfoTab it_alts_2 = 
  { .name                = "alts_2",
    // fvs [("u6",Int_h[U] ),("u7",Int_h[U] ),("u8",Int_h[U] )]
    .fvCount             = 3,
    .entryCode           = &alts_2,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 3,
  };
InfoTab it_alts_3 = 
  { .name                = "alts_3",
    // fvs [("u8",Int_h[U] ),("u9",Int_h[U] )]
    .fvCount             = 2,
    .entryCode           = &alts_3,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 2,
  };
InfoTab it_alts_4 = 
  { .name                = "alts_4",
    // fvs [("i_h",UBInt)]
    .fvCount             = 1,
    .entryCode           = &alts_4,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_5 = 
  { .name                = "alts_5",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_5,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    // fvs [("k_h",UBInt)]
    .fvCount             = 1,
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
InfoTab it_main = 
  { .name                = "main",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
    .layoutInfo.payloadSize  = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };

extern Obj sho_nine;
extern Obj sho_seven;
extern Obj sho_t1;
extern Obj sho_f;
extern Obj sho_main;

Obj sho_nine =
{
  .infoPtr   = &it_nine,
  .objType   = CON,
  .ident     = "nine",
  .payload = {
    {.argType = INT, .i = 9},
  },
};

Obj sho_seven =
{
  .infoPtr   = &it_seven,
  .objType   = CON,
  .ident     = "seven",
  .payload = {
    {.argType = INT, .i = 7},
  },
};

Obj sho_t1 =
{
  .infoPtr   = &it_t1,
  .objType   = CON,
  .ident     = "t1",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_seven},
    {.argType = HEAPOBJ, .op = &sho_nine},
    {.argType = INT, .i = 6},
    {.argType = INT, .i = 8},
  },
};

Obj sho_f =
{
  .infoPtr   = &it_f,
  .objType   = THUNK,
  .ident     = "f",
  .payload = {0}
};

Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  .payload = {0}
};

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_nine;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_t1;
  stgStatObj[stgStatObjCount++] = &sho_f;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// Int[B] 
DEFUN1(fun_f, self) {
  fprintf(stderr, "f here\n");
  stgThunk(self);
  Obj *ccont_alts_0 = stgAllocCont( &it_alts_0);
      // no FVs
    stgCurVal = HOTOPL(&sho_t1); // t1
  STGEVAL(stgCurVal);
  fprintf(stderr, "f returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  // TupUBUB u6 b7 u8 b9 ->
  Obj *ccont_alts_1 = stgAllocCont( &it_alts_1);
      // load payload with FVs b9 u6 u8
    ccont_alts_1->payload[0] = scrut_alts_0.op->payload[1]; // b9
    ccont_alts_1->payload[1] = scrut_alts_0.op->payload[2]; // u6
    ccont_alts_1->payload[2] = scrut_alts_0.op->payload[3]; // u8
  stgCurVal = scrut_alts_0.op->payload[0]; // b7
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  Obj *ccont_alts_1 = stgPopCont();
  PtrOrLiteral b9 = ccont_alts_1->payload[0];
  PtrOrLiteral u6 = ccont_alts_1->payload[1];
  PtrOrLiteral u8 = ccont_alts_1->payload[2];
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // I u7 ->
  Obj *ccont_alts_2 = stgAllocCont( &it_alts_2);
      // load payload with FVs u6 u7 u8
    ccont_alts_2->payload[0] = u6; // u6
    ccont_alts_2->payload[1] = scrut_alts_1.op->payload[0]; // u7
    ccont_alts_2->payload[2] = u8; // u8
  stgCurVal = b9; // b9
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  Obj *ccont_alts_2 = stgPopCont();
  PtrOrLiteral u6 = ccont_alts_2->payload[0];
  PtrOrLiteral u7 = ccont_alts_2->payload[1];
  PtrOrLiteral u8 = ccont_alts_2->payload[2];
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // I u9 ->
  Obj *ccont_alts_3 = stgAllocCont( &it_alts_3);
      // load payload with FVs u8 u9
    ccont_alts_3->payload[0] = u8; // u8
    ccont_alts_3->payload[1] = scrut_alts_2.op->payload[0]; // u9
  stgCurVal.argType = INT;
  stgCurVal.i = (u6).i + (u7).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  Obj *ccont_alts_3 = stgPopCont();
  PtrOrLiteral u8 = ccont_alts_3->payload[0];
  PtrOrLiteral u9 = ccont_alts_3->payload[1];
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // i_h ->
  Obj *ccont_alts_4 = stgAllocCont( &it_alts_4);
      // load payload with FVs i_h
    ccont_alts_4->payload[0] = scrut_alts_3; // i_h
  stgCurVal.argType = INT;
  stgCurVal.i = (u8).i + (u9).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_4) {
  fprintf(stderr, "alts_4 here\n");
  Obj *ccont_alts_4 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_4->payload[0];
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  // j_h ->
  Obj *ccont_alts_5 = stgAllocCont( &it_alts_5);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i + (scrut_alts_4).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // k_h ->
  Obj *result_0 = stgNewHeapObj( &it_result_0 );
  result_0->payload[0] = scrut_alts_5; // k_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_0
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  stgCurVal = HOTOPL(&sho_f); // f
  STGEVAL(stgCurVal);
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
  gc();
  return 0;
}

