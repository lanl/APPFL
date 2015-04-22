#include "stg_header.h"
void registerSHOs();
FnPtr fun_list4();
FnPtr fun_list5();
FnPtr fun_append();
FnPtr alts_0();
FnPtr fun_rec_0();
FnPtr fun_seq();
FnPtr alts_1();
FnPtr fun_forcelist();
FnPtr alts_2();
FnPtr fun_rec_1();
FnPtr fun_main();
FnPtr fun_f_0();
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 2,
    .conFields.conName   = "Unit",
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
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 4,
    .conFields.conName   = "Nil",
  };
InfoTab it_list1 = 
  { .name                = "list1",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 5,
    .conFields.conName   = "Cons",
  };
InfoTab it_list2 = 
  { .name                = "list2",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 5,
    .conFields.conName   = "Cons",
  };
InfoTab it_list3 = 
  { .name                = "list3",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 5,
    .conFields.conName   = "Cons",
  };
InfoTab it_list4 = 
  { .name                = "list4",
    .fvCount             = 0,
    .entryCode           = &fun_list4,
    .objType             = THUNK,
  };
InfoTab it_list5 = 
  { .name                = "list5",
    .fvCount             = 0,
    .entryCode           = &fun_list5,
    .objType             = THUNK,
  };
InfoTab it_append = 
  { .name                = "append",
    .fvCount             = 0,
    .entryCode           = &fun_append,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_rec_0 = 
  { .name                = "rec_0",
    .fvCount             = 2,
    .entryCode           = &fun_rec_0,
    .objType             = THUNK,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    .fvCount             = 2,
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
InfoTab it_forcelist = 
  { .name                = "forcelist",
    .fvCount             = 0,
    .entryCode           = &fun_forcelist,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_rec_1 = 
  { .name                = "rec_1",
    .fvCount             = 1,
    .entryCode           = &fun_rec_1,
    .objType             = THUNK,
  };
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
  };
InfoTab it_f_0 = 
  { .name                = "f_0",
    .fvCount             = 0,
    .entryCode           = &fun_f_0,
    .objType             = THUNK,
  };

extern Obj sho_unit;
extern Obj sho_zero;
extern Obj sho_one;
extern Obj sho_nil;
extern Obj sho_list1;
extern Obj sho_list2;
extern Obj sho_list3;
extern Obj sho_list4;
extern Obj sho_list5;
extern Obj sho_append;
extern Obj sho_seq;
extern Obj sho_forcelist;
extern Obj sho_main;

Obj sho_unit =
{
  .infoPtr   = &it_unit,
  .objType   = CON,
  .ident     = "unit",
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
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
  };
Obj sho_list1 =
{
  .infoPtr   = &it_list1,
  .objType   = CON,
  .ident     = "list1",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_one,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_nil,
  };
Obj sho_list2 =
{
  .infoPtr   = &it_list2,
  .objType   = CON,
  .ident     = "list2",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_one,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_list1,
  };
Obj sho_list3 =
{
  .infoPtr   = &it_list3,
  .objType   = CON,
  .ident     = "list3",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_zero,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_list2,
  };
Obj sho_list4 =
{
  .infoPtr   = &it_list4,
  .objType   = THUNK,
  .ident     = "list4",
  };
Obj sho_list5 =
{
  .infoPtr   = &it_list5,
  .objType   = THUNK,
  .ident     = "list5",
  };
Obj sho_append =
{
  .infoPtr   = &it_append,
  .objType   = FUN,
  .ident     = "append",
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
Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_list1;
  stgStatObj[stgStatObjCount++] = &sho_list2;
  stgStatObj[stgStatObjCount++] = &sho_list3;
  stgStatObj[stgStatObjCount++] = &sho_list4;
  stgStatObj[stgStatObjCount++] = &sho_list5;
  stgStatObj[stgStatObjCount++] = &sho_append;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


DEFUN1(fun_list4, self) {
  fprintf(stderr, "list4 here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_append), HOTOPL(&sho_list3), HOTOPL(&sho_list3));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_list5, self) {
  fprintf(stderr, "list5 here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_append), HOTOPL(&sho_list4), HOTOPL(&sho_list4));
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_append, self, l1, l2) {
  fprintf(stderr, "append here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      // load payload with FVs l2
      .payload[0] = l2,
    });
  stgCurVal = l1;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  Cont ccont_alts_0 = stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    case 4: {
      stgCurVal = ccont_alts_0.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 5: {
      Obj *rec_0 = stgNewHeapObj();
      *rec_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_0,
              .ident = "rec_0",
              .payload[0] = scrut_alts_0.op->payload[1],
              .payload[1] = ccont_alts_0.payload[0],
            };
      Obj *result_0 = stgNewHeapObj();
      *result_0 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_0,
              .ident = "result_0",
              .payload[0] = scrut_alts_0.op->payload[0],
              .payload[1] = HOTOPL(STGHEAPAT(-2)),
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_0);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");
  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_append), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
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
  stgCurVal = ccont_alts_1.payload[0];
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      // no FVs
        });
  stgCurVal = list;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_2) {
  Cont ccont_alts_2 = stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    case 4: {
      stgCurVal = HOTOPL(&sho_unit);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 5: {
      Obj *rec_1 = stgNewHeapObj();
      *rec_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_1,
              .ident = "rec_1",
              .payload[0] = scrut_alts_2.op->payload[1],
            };
      STGAPPLY2(HOTOPL(&sho_seq), scrut_alts_2.op->payload[0], HOTOPL(STGHEAPAT(-1)));
      STGRETURN0();
    }
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_2);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  Obj *f_0 = stgNewHeapObj();
  *f_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_f_0,
          .ident = "f_0",
                };
  STGAPPLY2(HOTOPL(&sho_seq), HOTOPL(STGHEAPAT(-1)), HOTOPL(&sho_list5));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_f_0, self) {
  fprintf(stderr, "f_0 here\n");
  stgThunk(self);
  STGAPPLY1(HOTOPL(&sho_forcelist), HOTOPL(&sho_list5));
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

