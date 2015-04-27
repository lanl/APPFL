#include "stg_header.h"
void registerSHOs();
FnPtr fun_repminlist();
FnPtr fun_mlist_0();
FnPtr fun_m_0();
FnPtr alts_0();
FnPtr alts_1();
FnPtr fun_rep();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_res1_0();
FnPtr fun_intMin();
FnPtr fun_seq();
FnPtr alts_5();
FnPtr fun_forcelist();
FnPtr alts_6();
FnPtr fun_rec_0();
FnPtr fun_main();
FnPtr fun_result_0();
FnPtr fun_f_0();
InfoTab it_repminlist = 
  { .name                = "repminlist",
    .fvCount             = 0,
    .entryCode           = &fun_repminlist,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_mlist_0 = 
  { .name                = "mlist_0",
    .fvCount             = 2,
    .entryCode           = &fun_mlist_0,
    .objType             = THUNK,
  };
InfoTab it_m_0 = 
  { .name                = "m_0",
    .fvCount             = 1,
    .entryCode           = &fun_m_0,
    .objType             = THUNK,
  };
InfoTab it_rep = 
  { .name                = "rep",
    .fvCount             = 0,
    .entryCode           = &fun_rep,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_res_0 = 
  { .name                = "res_0",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 2,
    .conFields.conName   = "Pair",
  };
InfoTab it_mlist_1 = 
  { .name                = "mlist_1",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 5,
    .conFields.conName   = "Con",
  };
InfoTab it_res_1 = 
  { .name                = "res_1",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 2,
    .conFields.conName   = "Pair",
  };
InfoTab it_res1_0 = 
  { .name                = "res1_0",
    .fvCount             = 2,
    .entryCode           = &fun_res1_0,
    .objType             = THUNK,
  };
InfoTab it_res2_0 = 
  { .name                = "res2_0",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 4,
    .conFields.conName   = "Cons",
  };
InfoTab it_res_2 = 
  { .name                = "res_2",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 2,
    .conFields.conName   = "Pair",
  };
InfoTab it_intMin = 
  { .name                = "intMin",
    .fvCount             = 0,
    .entryCode           = &fun_intMin,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_error = 
  { .name                = "error",
    .fvCount             = 0,
    .entryCode           = &stg_error,
    .objType             = BLACKHOLE,
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 6,
    .conFields.conName   = "Unit",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 7,
    .conFields.conName   = "I",
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 3,
    .conFields.conName   = "Nil",
  };
InfoTab it_list1 = 
  { .name                = "list1",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 4,
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
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    .fvCount             = 0,
    .entryCode           = &fun_result_0,
    .objType             = THUNK,
  };
InfoTab it_f_0 = 
  { .name                = "f_0",
    .fvCount             = 1,
    .entryCode           = &fun_f_0,
    .objType             = THUNK,
  };

extern Obj sho_repminlist;
extern Obj sho_rep;
extern Obj sho_intMin;
extern Obj sho_error;
extern Obj sho_unit;
extern Obj sho_one;
extern Obj sho_nil;
extern Obj sho_list1;
extern Obj sho_seq;
extern Obj sho_forcelist;
extern Obj sho_main;

Obj sho_repminlist =
{
  .infoPtr   = &it_repminlist,
  .objType   = FUN,
  .ident     = "repminlist",
  };
Obj sho_rep =
{
  .infoPtr   = &it_rep,
  .objType   = FUN,
  .ident     = "rep",
  };
Obj sho_intMin =
{
  .infoPtr   = &it_intMin,
  .objType   = FUN,
  .ident     = "intMin",
  };
Obj sho_error =
{
  .infoPtr   = &it_error,
  .objType   = BLACKHOLE,
  .ident     = "error",
  };
Obj sho_unit =
{
  .infoPtr   = &it_unit,
  .objType   = CON,
  .ident     = "unit",
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
  stgStatObj[stgStatObjCount++] = &sho_repminlist;
  stgStatObj[stgStatObjCount++] = &sho_rep;
  stgStatObj[stgStatObjCount++] = &sho_intMin;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_list1;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


DEFUN2(fun_repminlist, self, xs) {
  fprintf(stderr, "repminlist here\n");
  Obj *mlist_0 = stgNewHeapObj();
  Obj *m_0 = stgNewHeapObj();
  *mlist_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_mlist_0,
          .ident = "mlist_0",
          .payload[0] = HOTOPL(STGHEAPAT(-1)), // m_0
          .payload[1] = xs, // xs
        };
  *m_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_m_0,
          .ident = "m_0",
          .payload[0] = HOTOPL(STGHEAPAT(-2)), // mlist_0
        };
  stgPushCont( (Cont)
    { .retAddr = &alts_1,
      .objType = CASECONT,
      .ident = "CCont for alts_1",
      // no FVs
        });
  stgCurVal = HOTOPL(STGHEAPAT(-2)); // mlist_0
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_mlist_0, self) {
  fprintf(stderr, "mlist_0 here\n");
  stgThunk(self);
  // rep m_0 xs
  STGAPPLY2(HOTOPL(&sho_rep), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_m_0, self) {
  fprintf(stderr, "m_0 here\n");
  stgThunk(self);
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      .objType = CASECONT,
      .ident = "CCont for alts_0",
      // no FVs
        });
  stgCurVal = self.op->payload[0]; // mlist_0
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_0 = stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Pair m xxx ->
    case 2: {
      stgCurVal = scrut_alts_0.op->payload[0]; // m
      STGRETURN0();
    }
    // x ->
    default: {
      // stg_case_not_exhaustive x
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_0);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_1) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_1 = stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Pair yyy list ->
    case 2: {
      stgCurVal = scrut_alts_1.op->payload[1]; // list
      STGRETURN0();
    }
    // x ->
    default: {
      // stg_case_not_exhaustive x
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_1);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_rep, self, m, xs) {
  fprintf(stderr, "rep here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_2,
      .objType = CASECONT,
      .ident = "CCont for alts_2",
      // load payload with FVs m
      .payload[0] = m, // m
    });
  stgCurVal = xs; // xs
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_2) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_2 = stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 3: {
      Obj *res_0 = stgNewHeapObj();
      *res_0 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_res_0,
              .ident = "res_0",
              .payload[0] = HOTOPL(&sho_error), // error
              .payload[1] = HOTOPL(&sho_nil), // nil
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_0
      STGRETURN0();
    }
    // Cons y ys ->
    case 4: {
      stgPushCont( (Cont)
        { .retAddr = &alts_3,
          .objType = CASECONT,
          .ident = "CCont for alts_3",
          // load payload with FVs m y ys
          .payload[0] = ccont_alts_2.payload[0], // m
          .payload[1] = scrut_alts_2.op->payload[0], // y
          .payload[2] = scrut_alts_2.op->payload[1], // ys
        });
      stgCurVal = scrut_alts_2.op->payload[1]; // ys
      STGRETURN0();
    }
    // x ->
    default: {
      // stg_case_not_exhaustive x
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_2);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_3) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_3 = stgPopCont();
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 3: {
      Obj *mlist_1 = stgNewHeapObj();
      Obj *res_1 = stgNewHeapObj();
      *mlist_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_mlist_1,
              .ident = "mlist_1",
              .payload[0] = ccont_alts_3.payload[0], // m
              .payload[1] = HOTOPL(&sho_nil), // nil
            };
      *res_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_res_1,
              .ident = "res_1",
              .payload[0] = ccont_alts_3.payload[1], // y
              .payload[1] = HOTOPL(STGHEAPAT(-2)), // mlist_1
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_1
      STGRETURN0();
    }
    // xxx ->
    default: {
      stgPushCont( (Cont)
        { .retAddr = &alts_4,
          .objType = CASECONT,
          .ident = "CCont for alts_4",
          // load payload with FVs y m
          .payload[0] = ccont_alts_3.payload[1], // y
          .payload[1] = ccont_alts_3.payload[0], // m
        });
      // rep m ys
      STGAPPLY2(HOTOPL(&sho_rep), ccont_alts_3.payload[0], ccont_alts_3.payload[2]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_4) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_4 = stgPopCont();
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Pair mp list ->
    case 2: {
      Obj *res1_0 = stgNewHeapObj();
      Obj *res2_0 = stgNewHeapObj();
      Obj *res_2 = stgNewHeapObj();
      *res1_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_res1_0,
              .ident = "res1_0",
              .payload[0] = scrut_alts_4.op->payload[0], // mp
              .payload[1] = ccont_alts_4.payload[0], // y
            };
      *res2_0 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_res2_0,
              .ident = "res2_0",
              .payload[0] = ccont_alts_4.payload[1], // m
              .payload[1] = scrut_alts_4.op->payload[1], // list
            };
      *res_2 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_res_2,
              .ident = "res_2",
              .payload[0] = HOTOPL(STGHEAPAT(-3)), // res1_0
              .payload[1] = HOTOPL(STGHEAPAT(-2)), // res2_0
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_2
      STGRETURN0();
    }
    // x ->
    default: {
      // stg_case_not_exhaustive x
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_4);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_res1_0, self) {
  fprintf(stderr, "res1_0 here\n");
  stgThunk(self);
  // intMin mp y
  STGAPPLY2(HOTOPL(&sho_intMin), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_intMin, self, x, y) {
  fprintf(stderr, "intMin here\n");
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_5,
      .objType = CASECONT,
      .ident = "CCont for alts_5",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_5) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_5 = stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // z ->
  stgCurVal = ccont_alts_5.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_6,
      .objType = CASECONT,
      .ident = "CCont for alts_6",
      // no FVs
        });
  stgCurVal = list; // list
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_6) {
  STGEVAL(stgCurVal);
  Cont ccont_alts_6 = stgPopCont();
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 3: {
      stgCurVal = HOTOPL(&sho_unit); // unit
      STGRETURN0();
    }
    // Cons h t ->
    case 4: {
      Obj *rec_0 = stgNewHeapObj();
      *rec_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_0,
              .ident = "rec_0",
              .payload[0] = scrut_alts_6.op->payload[1], // t
            };
      // seq h rec_0
      STGAPPLY2(HOTOPL(&sho_seq), scrut_alts_6.op->payload[0], HOTOPL(STGHEAPAT(-1)));
      STGRETURN0();
    }
    // x ->
    default: {
      // stg_case_not_exhaustive x
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrut_alts_6);
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

DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  Obj *result_0 = stgNewHeapObj();
  Obj *f_0 = stgNewHeapObj();
  *result_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_result_0,
          .ident = "result_0",
                };
  *f_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_f_0,
          .ident = "f_0",
          .payload[0] = HOTOPL(STGHEAPAT(-2)), // result_0
        };
  // seq f_0 result_0
  STGAPPLY2(HOTOPL(&sho_seq), HOTOPL(STGHEAPAT(-1)), HOTOPL(STGHEAPAT(-2)));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_result_0, self) {
  fprintf(stderr, "result_0 here\n");
  stgThunk(self);
  // repminlist list1
  STGAPPLY1(HOTOPL(&sho_repminlist), HOTOPL(&sho_list1));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_f_0, self) {
  fprintf(stderr, "f_0 here\n");
  stgThunk(self);
  // forcelist result_0
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

