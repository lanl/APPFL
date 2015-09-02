#include "stgc.h"
#include "stgApply.h"
void registerSHOs();
FnPtr fun_eqInt();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_multInt();
FnPtr alts_5();
FnPtr alts_6();
FnPtr alts_7();
FnPtr fun_subInt();
FnPtr alts_11();
FnPtr alts_12();
FnPtr alts_13();
FnPtr fun_fac();
FnPtr alts_60();
FnPtr fun_s_0();
FnPtr fun_rec_4();
FnPtr fun_output();
FnPtr fun_main();
InfoTab it_false = 
  { .name                = "false",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "False",
  };
InfoTab it_true = 
  { .name                = "true",
    // fvs []
    .fvCount             = 0,
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
InfoTab it_eqInt = 
  { .name                = "eqInt",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_eqInt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_2 = 
  { .name                = "alts_2",
    // fvs [("y",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &alts_2,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_3 = 
  { .name                = "alts_3",
    // fvs [("i_h",Int_h[U] )]
    .fvCount             = 1,
    .entryCode           = &alts_3,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_4 = 
  { .name                = "alts_4",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_4,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_multInt = 
  { .name                = "multInt",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_multInt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_5 = 
  { .name                = "alts_5",
    // fvs [("y",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &alts_5,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_6 = 
  { .name                = "alts_6",
    // fvs [("i_h",Int_h[U] )]
    .fvCount             = 1,
    .entryCode           = &alts_6,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_7 = 
  { .name                = "alts_7",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_7,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    // fvs [("x_h",UBInt)]
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
InfoTab it_one = 
  { .name                = "one",
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
InfoTab it_subInt = 
  { .name                = "subInt",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_subInt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_11 = 
  { .name                = "alts_11",
    // fvs [("y",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &alts_11,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_12 = 
  { .name                = "alts_12",
    // fvs [("i_h",Int_h[U] )]
    .fvCount             = 1,
    .entryCode           = &alts_12,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_13 = 
  { .name                = "alts_13",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_13,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_2 = 
  { .name                = "result_2",
    // fvs [("x_h",UBInt)]
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
InfoTab it_zero = 
  { .name                = "zero",
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
InfoTab it_fac = 
  { .name                = "fac",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_fac,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_60 = 
  { .name                = "alts_60",
    // fvs [("x",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &alts_60,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_s_0 = 
  { .name                = "s_0",
    // fvs [("x",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &fun_s_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_4 = 
  { .name                = "rec_4",
    // fvs [("s_0",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &fun_rec_4,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
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
InfoTab it_output = 
  { .name                = "output",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_output,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result = 
  { .name                = "result",
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
InfoTab it_main = 
  { .name                = "main",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };

extern Obj sho_false;
extern Obj sho_true;
extern Obj sho_eqInt;
extern Obj sho_multInt;
extern Obj sho_one;
extern Obj sho_subInt;
extern Obj sho_zero;
extern Obj sho_fac;
extern Obj sho_seven;
extern Obj sho_output;
extern Obj sho_result;
extern Obj sho_main;

Obj sho_false =
{
  .infoPtr   = &it_false,
  .objType   = CON,
  .ident     = "false",
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

Obj sho_eqInt =
{
  .infoPtr   = &it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  .payload = {
    },
};

Obj sho_multInt =
{
  .infoPtr   = &it_multInt,
  .objType   = FUN,
  .ident     = "multInt",
  .payload = {
    },
};

Obj sho_one =
{
  .infoPtr   = &it_one,
  .objType   = CON,
  .ident     = "one",
  .payload = {
    {.argType = INT, .i = 1},
},
};

Obj sho_subInt =
{
  .infoPtr   = &it_subInt,
  .objType   = FUN,
  .ident     = "subInt",
  .payload = {
    },
};

Obj sho_zero =
{
  .infoPtr   = &it_zero,
  .objType   = CON,
  .ident     = "zero",
  .payload = {
    {.argType = INT, .i = 0},
},
};

Obj sho_fac =
{
  .infoPtr   = &it_fac,
  .objType   = FUN,
  .ident     = "fac",
  .payload = {
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

Obj sho_output =
{
  .infoPtr   = &it_output,
  .objType   = THUNK,
  .ident     = "output",
  .payload = {0}
};

Obj sho_result =
{
  .infoPtr   = &it_result,
  .objType   = CON,
  .ident     = "result",
  .payload = {
    {.argType = INT, .i = 5040},
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
  stgStatObj[stgStatObjCount++] = &sho_false;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_fac;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_output;
  stgStatObj[stgStatObjCount++] = &sho_result;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// Int[B]  -> Int[B]  -> Bool[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  // scrutinee no heap allocation
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  // Bool[B] 
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // I i_h ->
  // scrutinee no heap allocation
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  // Bool[B] 
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // I j_h ->
  // scrutinee no heap allocation
  stgCurVal.argType = INT;
  stgCurVal.i = (scrut_alts_2.op->payload[0]).i == (scrut_alts_3.op->payload[0]).i;
  // Bool[B] 
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  STGRETURN0();
  STGRETURN0();
  fprintf(stderr, "eqInt returning\n");
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_2) {
  ENDFUN;
}


DEFUN0(alts_3) {
  ENDFUN;
}


DEFUN0(alts_4) {
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
  // scrutinee no heap allocation
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  // Int[B] 
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // I i_h ->
  // scrutinee no heap allocation
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  // Int[B] 
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  // I j_h ->
  // scrutinee no heap allocation
  stgCurVal.argType = INT;
  stgCurVal.i = (scrut_alts_5.op->payload[0]).i * (scrut_alts_6.op->payload[0]).i;
  // Int[B] 
  PtrOrLiteral scrut_alts_7 = stgCurVal;
  // x_h ->
  Obj *result_0 = stgNewHeapObj( &it_result_0 );
  result_0->payload[0] = scrut_alts_7; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  STGRETURN0();
  STGRETURN0();
  fprintf(stderr, "multInt returning\n");
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_5) {
  ENDFUN;
}


DEFUN0(alts_6) {
  ENDFUN;
}


DEFUN0(alts_7) {
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  // scrutinee no heap allocation
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  // Int[B] 
  PtrOrLiteral scrut_alts_11 = stgCurVal;
  // I i_h ->
  // scrutinee no heap allocation
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  // Int[B] 
  PtrOrLiteral scrut_alts_12 = stgCurVal;
  // I j_h ->
  // scrutinee no heap allocation
  stgCurVal.argType = INT;
  stgCurVal.i = (scrut_alts_11.op->payload[0]).i - (scrut_alts_12.op->payload[0]).i;
  // Int[B] 
  PtrOrLiteral scrut_alts_13 = stgCurVal;
  // x_h ->
  Obj *result_2 = stgNewHeapObj( &it_result_2 );
  result_2->payload[0] = scrut_alts_13; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  STGRETURN0();
  STGRETURN0();
  fprintf(stderr, "subInt returning\n");
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_11) {
  ENDFUN;
}


DEFUN0(alts_12) {
  ENDFUN;
}


DEFUN0(alts_13) {
  ENDFUN;
}


// Int[B]  -> Int[B] 
// ((["x"],[]),([Int[B] ],[]))
DEFUN2(fun_fac, self, x) {
  fprintf(stderr, "fac here\n");
  // scrutinee no heap allocation
  // eqInt x zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), x, HOTOPL(&sho_zero));
  // Int[B] 
  PtrOrLiteral scrut_alts_60 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_one); // one
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      Obj *s_0 = stgNewHeapObj( &it_s_0 );
      Obj *rec_4 = stgNewHeapObj( &it_rec_4 );
      s_0->payload[1] = x; // x
      rec_4->payload[1] = HOTOPL((Obj *)STGHEAPAT(4,2)); // s_0
      // multInt x rec_4
      STGAPPLYPP(HOTOPL(&sho_multInt), x, HOTOPL((Obj *)STGHEAPAT(2,1)));
      STGRETURN0();
    }
  }
  fprintf(stderr, "fac returning\n");
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_60) {
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_s_0, self) {
  fprintf(stderr, "s_0 here\n");
  stgThunk(self);
  // subInt x one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "s_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_rec_4, self) {
  fprintf(stderr, "rec_4 here\n");
  stgThunk(self);
  // fac s_0
  STGAPPLYP(HOTOPL(&sho_fac), self.op->payload[1]);
  fprintf(stderr, "rec_4 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_output, self) {
  fprintf(stderr, "output here\n");
  stgThunk(self);
  // fac seven
  STGAPPLYP(HOTOPL(&sho_fac), HOTOPL(&sho_seven));
  fprintf(stderr, "output returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // eqInt output result
  STGAPPLYPP(HOTOPL(&sho_eqInt), HOTOPL(&sho_output), HOTOPL(&sho_result));
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
  return 0;
}

