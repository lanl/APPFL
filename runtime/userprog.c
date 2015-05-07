#include "stg_header.h"
void registerSHOs();
FnPtr fun_main();
FnPtr fun_result_2();
FnPtr fun_f_0();
FnPtr fun_seq();
FnPtr alts_0();
FnPtr fun_forcelist();
FnPtr alts_7();
FnPtr fun_rec_0();
FnPtr fun_alts_7_exhaust();
FnPtr fun_take();
FnPtr alts_8();
FnPtr alts_9();
FnPtr fun_m_0();
FnPtr fun_rec_1();
FnPtr fun_alts_9_exhaust();
FnPtr fun_alts_8_exhaust();
FnPtr fun_eqInt();
FnPtr alts_1();
FnPtr alts_2();
FnPtr alts_3();
FnPtr fun_subInt();
FnPtr alts_4();
FnPtr alts_5();
FnPtr alts_6();
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
  };
InfoTab it_result_2 = 
  { .name                = "result_2",
    .fvCount             = 0,
    .entryCode           = &fun_result_2,
    .objType             = THUNK,
  };
InfoTab it_f_0 = 
  { .name                = "f_0",
    .fvCount             = 1,
    .entryCode           = &fun_f_0,
    .objType             = THUNK,
  };
InfoTab it_three = 
  { .name                = "three",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_ones = 
  { .name                = "ones",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 6,
    .conFields.conName   = "Cons",
  };
InfoTab it_seq = 
  { .name                = "seq",
    .fvCount             = 0,
    .entryCode           = &fun_seq,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_forcelist = 
  { .name                = "forcelist",
    .fvCount             = 0,
    .entryCode           = &fun_forcelist,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_rec_0 = 
  { .name                = "rec_0",
    .fvCount             = 1,
    .entryCode           = &fun_rec_0,
    .objType             = THUNK,
  };
InfoTab it_alts_7_exhaust = 
  { .name                = "alts_7_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_7_exhaust,
    .objType             = THUNK,
  };
InfoTab it_take = 
  { .name                = "take",
    .fvCount             = 0,
    .entryCode           = &fun_take,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_m_0 = 
  { .name                = "m_0",
    .fvCount             = 1,
    .entryCode           = &fun_m_0,
    .objType             = THUNK,
  };
InfoTab it_rec_1 = 
  { .name                = "rec_1",
    .fvCount             = 2,
    .entryCode           = &fun_rec_1,
    .objType             = THUNK,
  };
InfoTab it_result_1 = 
  { .name                = "result_1",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 6,
    .conFields.conName   = "Cons",
  };
InfoTab it_alts_9_exhaust = 
  { .name                = "alts_9_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_9_exhaust,
    .objType             = THUNK,
  };
InfoTab it_alts_8_exhaust = 
  { .name                = "alts_8_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_8_exhaust,
    .objType             = THUNK,
  };
InfoTab it_zero = 
  { .name                = "zero",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 4,
    .conFields.conName   = "Unit",
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 5,
    .conFields.conName   = "Nil",
  };
InfoTab it_eqInt = 
  { .name                = "eqInt",
    .fvCount             = 0,
    .entryCode           = &fun_eqInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_subInt = 
  { .name                = "subInt",
    .fvCount             = 0,
    .entryCode           = &fun_subInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };

extern Obj sho_main;
extern Obj sho_three;
extern Obj sho_ones;
extern Obj sho_seq;
extern Obj sho_forcelist;
extern Obj sho_take;
extern Obj sho_zero;
extern Obj sho_one;
extern Obj sho_unit;
extern Obj sho_nil;
extern Obj sho_eqInt;
extern Obj sho_subInt;

Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };
Obj sho_three =
{
  .infoPtr   = &it_three,
  .objType   = CON,
  .ident     = "three",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 3,
  };
Obj sho_ones =
{
  .infoPtr   = &it_ones,
  .objType   = CON,
  .ident     = "ones",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_one,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_ones,
  };
Obj sho_seq =
{
  .infoPtr   = &it_seq,
  .objType   = FUN,
  .ident     = "seq",
  };
Obj sho_forcelist =
{
  .infoPtr   = &it_forcelist,
  .objType   = FUN,
  .ident     = "forcelist",
  };
Obj sho_take =
{
  .infoPtr   = &it_take,
  .objType   = FUN,
  .ident     = "take",
  };
Obj sho_zero =
{
  .infoPtr   = &it_zero,
  .objType   = CON,
  .ident     = "zero",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 0,
  };
Obj sho_one =
{
  .infoPtr   = &it_one,
  .objType   = CON,
  .ident     = "one",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 1,
  };
Obj sho_unit =
{
  .infoPtr   = &it_unit,
  .objType   = CON,
  .ident     = "unit",
  };
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
  };
Obj sho_eqInt =
{
  .infoPtr   = &it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  };
Obj sho_subInt =
{
  .infoPtr   = &it_subInt,
  .objType   = FUN,
  .ident     = "subInt",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_main;
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_ones;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_take;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
}


DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  Obj *result_2 = stgNewHeapObj();
  Obj *f_0 = stgNewHeapObj();
  *result_2 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_result_2,
          .ident = "result_2",
                };
  *f_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_f_0,
          .ident = "f_0",
          .payload[0] = HOTOPL(STGHEAPAT(-2)), // result_2
        };
  // seq f_0 result_2
  STGAPPLY2(HOTOPL(&sho_seq), HOTOPL(STGHEAPAT(-1)), HOTOPL(STGHEAPAT(-2)));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_result_2, self) {
  fprintf(stderr, "result_2 here\n");
  stgThunk(self);
  // take three ones
  STGAPPLY2(HOTOPL(&sho_take), HOTOPL(&sho_three), HOTOPL(&sho_ones));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_f_0, self) {
  fprintf(stderr, "f_0 here\n");
  stgThunk(self);
  // forcelist result_2
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      .objType = CASECONT,
      .ident = "CCont for alts_0",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_0 = stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  // z ->
  stgCurVal = ccont_alts_0.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_7,
      .objType = CASECONT,
      .ident = "CCont for alts_7",
      // no FVs
        });
  stgCurVal = list; // list
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_7) {
  fprintf(stderr, "alts_7 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_7 = stgPopCont();
  PtrOrLiteral scrut_alts_7 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 5: {
      stgCurVal = HOTOPL(&sho_unit); // unit
      STGRETURN0();
    }
    // Cons h t ->
    case 6: {
      Obj *rec_0 = stgNewHeapObj();
      *rec_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_0,
              .ident = "rec_0",
              .payload[0] = scrut_alts_7.op->payload[1], // t
            };
      // seq h rec_0
      STGAPPLY2(HOTOPL(&sho_seq), scrut_alts_7.op->payload[0], HOTOPL(STGHEAPAT(-1)));
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_7_exhaust = stgNewHeapObj();
      *alts_7_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_7_exhaust,
              .ident = "alts_7_exhaust",
              .payload[0] = scrut_alts_7, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_7_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");
  stgThunk(self);
  // forcelist t
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_7_exhaust, self) {
  fprintf(stderr, "alts_7_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_8,
      .objType = CASECONT,
      .ident = "CCont for alts_8",
      // load payload with FVs xs n
      .payload[0] = xs, // xs
      .payload[1] = n, // n
    });
  // eqInt n zero
  STGAPPLY2(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_8) {
  fprintf(stderr, "alts_8 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_8 = stgPopCont();
  PtrOrLiteral scrut_alts_8 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // False  ->
    case 0: {
      stgPushCont( (Cont)
        { .retAddr = &alts_9,
          .objType = CASECONT,
          .ident = "CCont for alts_9",
          // load payload with FVs n
          .payload[0] = ccont_alts_8.payload[1], // n
        });
      stgCurVal = ccont_alts_8.payload[0]; // xs
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_8_exhaust = stgNewHeapObj();
      *alts_8_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_8_exhaust,
              .ident = "alts_8_exhaust",
              .payload[0] = scrut_alts_8, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_8_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_9) {
  fprintf(stderr, "alts_9 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_9 = stgPopCont();
  PtrOrLiteral scrut_alts_9 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 5: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // Cons hd tl ->
    case 6: {
      Obj *m_0 = stgNewHeapObj();
      Obj *rec_1 = stgNewHeapObj();
      *m_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_m_0,
              .ident = "m_0",
              .payload[0] = ccont_alts_9.payload[0], // n
            };
      *rec_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_1,
              .ident = "rec_1",
              .payload[0] = HOTOPL(STGHEAPAT(-2)), // m_0
              .payload[1] = scrut_alts_9.op->payload[1], // tl
            };
      Obj *result_1 = stgNewHeapObj();
      *result_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_1,
              .ident = "result_1",
              .payload[0] = scrut_alts_9.op->payload[0], // hd
              .payload[1] = HOTOPL(STGHEAPAT(-2)), // rec_1
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_1
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_9_exhaust = stgNewHeapObj();
      *alts_9_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_9_exhaust,
              .ident = "alts_9_exhaust",
              .payload[0] = scrut_alts_9, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_9_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_m_0, self) {
  fprintf(stderr, "m_0 here\n");
  stgThunk(self);
  // subInt n one
  STGAPPLY2(HOTOPL(&sho_subInt), self.op->payload[0], HOTOPL(&sho_one));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  // take m_0 tl
  STGAPPLY2(HOTOPL(&sho_take), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_9_exhaust, self) {
  fprintf(stderr, "alts_9_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_8_exhaust, self) {
  fprintf(stderr, "alts_8_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_1,
      .objType = CASECONT,
      .ident = "CCont for alts_1",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_1 = stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // I i ->
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      .objType = CASECONT,
      .ident = "CCont for alts_2",
      // load payload with FVs i
      .payload[0] = scrut_alts_1.op->payload[0], // i
    });
  stgCurVal = ccont_alts_1.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_2 = stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // I j ->
  stgPushCont( (Cont)
    { .retAddr = &alts_3,
      .objType = CASECONT,
      .ident = "CCont for alts_3",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_2.payload[0]).i == (scrut_alts_2.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_3 = stgPopCont();
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // x ->
  stgCurVal = (scrut_alts_3).i?HOTOPL(&sho_true):HOTOPL(&sho_false);
  STGRETURN0();
  ENDFUN;
}


DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_4,
      .objType = CASECONT,
      .ident = "CCont for alts_4",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_4) {
  fprintf(stderr, "alts_4 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_4 = stgPopCont();
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  // I i ->
  stgPushCont( (Cont)
    { .retAddr = &alts_5,
      .objType = CASECONT,
      .ident = "CCont for alts_5",
      // load payload with FVs i
      .payload[0] = scrut_alts_4.op->payload[0], // i
    });
  stgCurVal = ccont_alts_4.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_5 = stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // I j ->
  stgPushCont( (Cont)
    { .retAddr = &alts_6,
      .objType = CASECONT,
      .ident = "CCont for alts_6",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_5.payload[0]).i - (scrut_alts_5.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_6) {
  fprintf(stderr, "alts_6 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_6 = stgPopCont();
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  // x ->
  Obj *result_0 = stgNewHeapObj();
  *result_0 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_result_0,
          .ident = "result_0",
          .payload[0] = scrut_alts_6, // x
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_0
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

