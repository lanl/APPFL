#include "stg_header.h"
void registerSHOs();
FnPtr fun_seq();
FnPtr alts_0();
FnPtr fun_eqInt();
FnPtr alts_1();
FnPtr alts_2();
FnPtr alts_3();
FnPtr fun_subInt();
FnPtr alts_4();
FnPtr alts_5();
FnPtr alts_6();
FnPtr fun_forcelist();
FnPtr alts_7();
FnPtr fun_rec_0();
FnPtr fun_take();
FnPtr alts_8();
FnPtr alts_9();
FnPtr fun_m_0();
FnPtr fun_rec_1();
FnPtr fun_main();
FnPtr fun_result_2();
FnPtr fun_f_0();
InfoTab it_zero = 
  { .name                = "zero",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 2,
    .conFields.conName   = "I",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 2,
    .conFields.conName   = "I",
  };
InfoTab it_three = 
  { .name                = "three",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 2,
    .conFields.conName   = "I",
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 3,
    .conFields.conName   = "Unit",
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 4,
    .conFields.conName   = "Nil",
  };
InfoTab it_ones = 
  { .name                = "ones",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 5,
    .conFields.conName   = "Cons",
  };
InfoTab it_seq = 
  { .name                = "seq",
    .fvCount             = 0,
    .entryCode           = &fun_seq,
    .objType             = FUN,
    .funFields.arity     = 2,
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
    .conFields.tag       = 2,
    .conFields.conName   = "I",
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
    .conFields.tag       = 5,
    .conFields.conName   = "Cons",
  };
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

extern Obj sho_zero;
extern Obj sho_one;
extern Obj sho_three;
extern Obj sho_unit;
extern Obj sho_nil;
extern Obj sho_ones;
extern Obj sho_seq;
extern Obj sho_eqInt;
extern Obj sho_subInt;
extern Obj sho_forcelist;
extern Obj sho_take;
extern Obj sho_main;

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
Obj sho_three =
{
  .infoPtr   = &it_three,
  .objType   = CON,
  .ident     = "three",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 3,
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
Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_ones;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_take;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
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
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  stgCurVal = ccont_alts_0.payload[0];
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_1,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_1) {
  Cont ccont_alts_1 = stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      // load payload with FVs i
      .payload[0] = scrut_alts_1.op->payload[0],
    });
  stgCurVal = ccont_alts_1.payload[0];
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_2) {
  Cont ccont_alts_2 = stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  stgPushCont( (Cont)
    { .retAddr = &alts_3,
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_2.payload[0]).i == (scrut_alts_2.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_3) {
  Cont ccont_alts_3 = stgPopCont();
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  stgCurVal = (scrut_alts_3).i?HOTOPL(&sho_true):HOTOPL(&sho_false);
  STGRETURN0();
  ENDFUN;
}


DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_4,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_4) {
  Cont ccont_alts_4 = stgPopCont();
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  stgPushCont( (Cont)
    { .retAddr = &alts_5,
      // load payload with FVs i
      .payload[0] = scrut_alts_4.op->payload[0],
    });
  stgCurVal = ccont_alts_4.payload[0];
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_5) {
  Cont ccont_alts_5 = stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  stgPushCont( (Cont)
    { .retAddr = &alts_6,
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_5.payload[0]).i - (scrut_alts_5.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_6) {
  Cont ccont_alts_6 = stgPopCont();
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  Obj *result_0 = stgNewHeapObj();
  *result_0 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_result_0,
          .ident = "result_0",
          .payload[0] = scrut_alts_6,
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1));
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_7,
      // no FVs
        });
  stgCurVal = list;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_7) {
  Cont ccont_alts_7 = stgPopCont();
  PtrOrLiteral scrut_alts_7 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    case 4: {
      stgCurVal = HOTOPL(&sho_unit);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 5: {
      Obj *rec_0 = stgNewHeapObj();
      *rec_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_0,
              .ident = "rec_0",
              .payload[0] = scrut_alts_7.op->payload[1],
            };
      STGAPPLY2(HOTOPL(&sho_seq), scrut_alts_7.op->payload[0], HOTOPL(STGHEAPAT(-1)));
      STGRETURN0();
    }
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_7);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");
  stgThunk(self);
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_8,
      // load payload with FVs xs n
      .payload[0] = xs,
      .payload[1] = n,
    });
  STGAPPLY2(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_8) {
  Cont ccont_alts_8 = stgPopCont();
  PtrOrLiteral scrut_alts_8 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    case 1: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      stgPushCont( (Cont)
        { .retAddr = &alts_9,
          // load payload with FVs n
          .payload[0] = ccont_alts_8.payload[1],
        });
      stgCurVal = ccont_alts_8.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_8);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_9) {
  Cont ccont_alts_9 = stgPopCont();
  PtrOrLiteral scrut_alts_9 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    case 4: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 5: {
      Obj *m_0 = stgNewHeapObj();
      *m_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_m_0,
              .ident = "m_0",
              .payload[0] = ccont_alts_9.payload[0],
            };
      Obj *rec_1 = stgNewHeapObj();
      *rec_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_1,
              .ident = "rec_1",
              .payload[0] = HOTOPL(STGHEAPAT(-3)),
              .payload[1] = scrut_alts_9.op->payload[1],
            };
      Obj *result_1 = stgNewHeapObj();
      *result_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_1,
              .ident = "result_1",
              .payload[0] = scrut_alts_9.op->payload[0],
              .payload[1] = HOTOPL(STGHEAPAT(-2)),
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_9);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_m_0, self) {
  fprintf(stderr, "m_0 here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_subInt), self.op->payload[0], HOTOPL(&sho_one));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_take), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  Obj *result_2 = stgNewHeapObj();
  *result_2 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_result_2,
          .ident = "result_2",
                };
  Obj *f_0 = stgNewHeapObj();
  *f_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_f_0,
          .ident = "f_0",
          .payload[0] = HOTOPL(STGHEAPAT(-2)),
        };
  STGAPPLY2(HOTOPL(&sho_seq), HOTOPL(STGHEAPAT(-1)), HOTOPL(STGHEAPAT(-2)));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_result_2, self) {
  fprintf(stderr, "result_2 here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_take), HOTOPL(&sho_three), HOTOPL(&sho_ones));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_f_0, self) {
  fprintf(stderr, "f_0 here\n");
  stgThunk(self);
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
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

