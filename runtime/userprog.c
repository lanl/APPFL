#include "stg_header.h"
void registerSHOs();
FnPtr fun_repminlist();
FnPtr fun_mlist_0();
FnPtr fun_m_0();
FnPtr alts_0();
FnPtr fun_alts_0_exhaust();
FnPtr alts_1();
FnPtr fun_alts_1_exhaust();
FnPtr fun_rep();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_res1_0();
FnPtr fun_alts_4_exhaust();
FnPtr fun_alts_2_exhaust();
FnPtr fun_minInt();
FnPtr alts_5();
FnPtr fun_seq();
FnPtr alts_6();
FnPtr fun_forcelist();
FnPtr alts_7();
FnPtr fun_rec_0();
FnPtr fun_alts_7_exhaust();
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
InfoTab it_alts_0_exhaust = 
  { .name                = "alts_0_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_0_exhaust,
    .objType             = THUNK,
  };
InfoTab it_alts_1_exhaust = 
  { .name                = "alts_1_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_1_exhaust,
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
    .conFields.tag       = 4,
    .conFields.conName   = "Cons",
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
InfoTab it_alts_4_exhaust = 
  { .name                = "alts_4_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_4_exhaust,
    .objType             = THUNK,
  };
InfoTab it_alts_2_exhaust = 
  { .name                = "alts_2_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_2_exhaust,
    .objType             = THUNK,
  };
InfoTab it_minInt = 
  { .name                = "minInt",
    .fvCount             = 0,
    .entryCode           = &fun_minInt,
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
    .conFields.tag       = 5,
    .conFields.conName   = "Unit",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 6,
    .conFields.conName   = "I",
  };
InfoTab it_two = 
  { .name                = "two",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 6,
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
InfoTab it_list2 = 
  { .name                = "list2",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 4,
    .conFields.conName   = "Cons",
  };
InfoTab it_list12 = 
  { .name                = "list12",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 4,
    .conFields.conName   = "Cons",
  };
InfoTab it_list212 = 
  { .name                = "list212",
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
InfoTab it_alts_7_exhaust = 
  { .name                = "alts_7_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_7_exhaust,
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
extern Obj sho_minInt;
extern Obj sho_error;
extern Obj sho_unit;
extern Obj sho_one;
extern Obj sho_two;
extern Obj sho_nil;
extern Obj sho_list2;
extern Obj sho_list12;
extern Obj sho_list212;
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
Obj sho_minInt =
{
  .infoPtr   = &it_minInt,
  .objType   = FUN,
  .ident     = "minInt",
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
Obj sho_two =
{
  .infoPtr   = &it_two,
  .objType   = CON,
  .ident     = "two",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 2,
  };
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
  };
Obj sho_list2 =
{
  .infoPtr   = &it_list2,
  .objType   = CON,
  .ident     = "list2",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_two,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_nil,
  };
Obj sho_list12 =
{
  .infoPtr   = &it_list12,
  .objType   = CON,
  .ident     = "list12",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_one,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_list2,
  };
Obj sho_list212 =
{
  .infoPtr   = &it_list212,
  .objType   = CON,
  .ident     = "list212",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_two,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_list12,
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
  stgStatObj[stgStatObjCount++] = &sho_minInt;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_list2;
  stgStatObj[stgStatObjCount++] = &sho_list12;
  stgStatObj[stgStatObjCount++] = &sho_list212;
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
  fprintf(stderr, "alts_0 here\n");
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
      Obj *alts_0_exhaust = stgNewHeapObj();
      *alts_0_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_0_exhaust,
              .ident = "alts_0_exhaust",
              .payload[0] = scrut_alts_0, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_0_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_alts_0_exhaust, self) {
  fprintf(stderr, "alts_0_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
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
      Obj *alts_1_exhaust = stgNewHeapObj();
      *alts_1_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_1_exhaust,
              .ident = "alts_1_exhaust",
              .payload[0] = scrut_alts_1, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_1_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_alts_1_exhaust, self) {
  fprintf(stderr, "alts_1_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
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
  fprintf(stderr, "alts_2 here\n");
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
  fprintf(stderr, "alts_4 here\n");
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
      Obj *alts_4_exhaust = stgNewHeapObj();
      *alts_4_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_4_exhaust,
              .ident = "alts_4_exhaust",
              .payload[0] = scrut_alts_4, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_4_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_res1_0, self) {
  fprintf(stderr, "res1_0 here\n");
  stgThunk(self);
  // minInt mp y
  STGAPPLY2(HOTOPL(&sho_minInt), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_4_exhaust, self) {
  fprintf(stderr, "alts_4_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
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

DEFUN3(fun_minInt, self, x, y) {
  fprintf(stderr, "minInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_5,
      .objType = CASECONT,
      .ident = "CCont for alts_5",
      // no FVs
        });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_5 = stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // x ->
  stgCurVal = scrut_alts_5; // x
  STGRETURN0();
  ENDFUN;
}


DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_6,
      .objType = CASECONT,
      .ident = "CCont for alts_6",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_6) {
  fprintf(stderr, "alts_6 here\n");
  STGEVAL(stgCurVal);
  Cont ccont_alts_6 = stgPopCont();
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  // z ->
  stgCurVal = ccont_alts_6.payload[0]; // y
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
  // repminlist list212
  STGAPPLY1(HOTOPL(&sho_repminlist), HOTOPL(&sho_list212));
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

