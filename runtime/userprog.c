#include "stgc.h"
#include "stgApply.h"
void registerSHOs();
FnPtr fun_const();
FnPtr fun_eqInt();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_head();
FnPtr alts_30();
FnPtr fun_alts_30_exhaust();
FnPtr fun_map();
FnPtr alts_29();
FnPtr fun_rec_1();
FnPtr fun_x_0();
FnPtr fun_output();
FnPtr fun_mc_0();
FnPtr fun_constSeven_0();
FnPtr fun_main();
InfoTab it_const __attribute__((aligned(8))) = 
  { .name                = "const",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_const,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_false __attribute__((aligned(8))) = 
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
InfoTab it_true __attribute__((aligned(8))) = 
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
InfoTab it_eqInt __attribute__((aligned(8))) = 
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
InfoTab it_alts_2 __attribute__((aligned(8))) = 
  { .name                = "alts_2",
    // fvs [("y",Int[B] )]
    .fvCount             = 1,
    .entryCode           = &alts_2,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_3 __attribute__((aligned(8))) = 
  { .name                = "alts_3",
    // fvs [("i_h",Int_h[U] )]
    .fvCount             = 1,
    .entryCode           = &alts_3,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_4 __attribute__((aligned(8))) = 
  { .name                = "alts_4",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_4,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_head __attribute__((aligned(8))) = 
  { .name                = "head",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_head,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_30 __attribute__((aligned(8))) = 
  { .name                = "alts_30",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &alts_30,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_30_exhaust __attribute__((aligned(8))) = 
  { .name                = "alts_30_exhaust",
    // fvs [("x",List[B] t54)]
    .fvCount             = 1,
    .entryCode           = &fun_alts_30_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_nil __attribute__((aligned(8))) = 
  { .name                = "nil",
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
    .conFields.conName   = "Nil",
  };
InfoTab it_one __attribute__((aligned(8))) = 
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
InfoTab it_list1 __attribute__((aligned(8))) = 
  { .name                = "list1",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_two __attribute__((aligned(8))) = 
  { .name                = "two",
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
InfoTab it_list2 __attribute__((aligned(8))) = 
  { .name                = "list2",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_seven __attribute__((aligned(8))) = 
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
InfoTab it_list3 __attribute__((aligned(8))) = 
  { .name                = "list3",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_map __attribute__((aligned(8))) = 
  { .name                = "map",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_map,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_29 __attribute__((aligned(8))) = 
  { .name                = "alts_29",
    // fvs [("f",t13 -> t53)]
    .fvCount             = 1,
    .entryCode           = &alts_29,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_1 __attribute__((aligned(8))) = 
  { .name                = "rec_1",
    // fvs [("f",t13 -> t53),("t",List[B] t13)]
    .fvCount             = 2,
    .entryCode           = &fun_rec_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_x_0 __attribute__((aligned(8))) = 
  { .name                = "x_0",
    // fvs [("f",t13 -> t53),("h",t13)]
    .fvCount             = 2,
    .entryCode           = &fun_x_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_res_0 __attribute__((aligned(8))) = 
  { .name                = "res_0",
    // fvs [("rec_1",List[B] t53),("x_0",t53)]
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_ten __attribute__((aligned(8))) = 
  { .name                = "ten",
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
InfoTab it_output __attribute__((aligned(8))) = 
  { .name                = "output",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_output,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_mc_0 __attribute__((aligned(8))) = 
  { .name                = "mc_0",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_mc_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_constSeven_0 __attribute__((aligned(8))) = 
  { .name                = "constSeven_0",
    // fvs [("mc_0",List[B] t56 -> Int[B] )]
    .fvCount             = 1,
    .entryCode           = &fun_constSeven_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_main __attribute__((aligned(8))) = 
  { .name                = "main",
    // fvs []
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };

extern Obj sho_const;
extern Obj sho_false;
extern Obj sho_true;
extern Obj sho_eqInt;
extern Obj sho_head;
extern Obj sho_nil;
extern Obj sho_one;
extern Obj sho_list1;
extern Obj sho_two;
extern Obj sho_list2;
extern Obj sho_seven;
extern Obj sho_list3;
extern Obj sho_map;
extern Obj sho_ten;
extern Obj sho_output;
extern Obj sho_main;

Obj sho_const =
{
  .infoPtr   = &it_const,
  .objType   = FUN,
  .ident     = "const",
  .payload = {
    },
};

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

Obj sho_head =
{
  .infoPtr   = &it_head,
  .objType   = FUN,
  .ident     = "head",
  .payload = {
    },
};

Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
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

Obj sho_list1 =
{
  .infoPtr   = &it_list1,
  .objType   = CON,
  .ident     = "list1",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_one},
    {.argType = HEAPOBJ, .op = &sho_nil},
},
};

Obj sho_two =
{
  .infoPtr   = &it_two,
  .objType   = CON,
  .ident     = "two",
  .payload = {
    {.argType = INT, .i = 2},
},
};

Obj sho_list2 =
{
  .infoPtr   = &it_list2,
  .objType   = CON,
  .ident     = "list2",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_two},
    {.argType = HEAPOBJ, .op = &sho_list1},
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

Obj sho_list3 =
{
  .infoPtr   = &it_list3,
  .objType   = CON,
  .ident     = "list3",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_seven},
    {.argType = HEAPOBJ, .op = &sho_list2},
},
};

Obj sho_map =
{
  .infoPtr   = &it_map,
  .objType   = FUN,
  .ident     = "map",
  .payload = {
    },
};

Obj sho_ten =
{
  .infoPtr   = &it_ten,
  .objType   = CON,
  .ident     = "ten",
  .payload = {
    {.argType = INT, .i = 10},
},
};

Obj sho_output =
{
  .infoPtr   = &it_output,
  .objType   = THUNK,
  .ident     = "output",
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
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_false;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_head;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_list1;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_list2;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_list3;
  stgStatObj[stgStatObjCount++] = &sho_map;
  stgStatObj[stgStatObjCount++] = &sho_ten;
  stgStatObj[stgStatObjCount++] = &sho_output;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// forall t32,t33.t33 -> t32 -> t33
// ((["x","y"],[]),([t33,t32],[]))
DEFUN3(fun_const, self, x, y) {
  fprintf(stderr, "const here\n");
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "const returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Bool[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_2 = stgAllocCont( &it_alts_2);
      // load payload with FVs y
    ccont_alts_2->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "eqInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  Obj *ccont_alts_2 = stgPopCont();
  PtrOrLiteral y = ccont_alts_2->payload[0];
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_3 = stgAllocCont( &it_alts_3);
      // load payload with FVs i_h
    ccont_alts_3->payload[0] = scrut_alts_2.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  Obj *ccont_alts_3 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_3->payload[0];
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // I j_h ->
  // scrutinee no heap allocation
  stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i == (scrut_alts_3.op->payload[0]).i;
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
  ENDFUN;
}


DEFUN0(alts_4) {
  ENDFUN;
}


// forall t54.List[B] t54 -> t54
// ((["xs"],[]),([List[B] t54],[]))
DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_30 = stgAllocCont( &it_alts_30);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "head returning\n");
  STGRETURN0();
  ENDFUN;
}

// t54
DEFUN0(alts_30) {
  fprintf(stderr, "alts_30 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_30 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_30.op->payload[0]; // hd
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_30_exhaust = stgNewHeapObj( &it_alts_30_exhaust );
      alts_30_exhaust->payload[1] = scrut_alts_30; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_30_exhaust
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t28.t28
DEFUN1(fun_alts_30_exhaust, self) {
  fprintf(stderr, "alts_30_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_30_exhaust returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t13,t53.(t13 -> t53) -> List[B] t13 -> List[B] t53
// ((["f","list"],[]),([t13 -> t53,List[B] t13],[]))
DEFUN3(fun_map, self, f, list) {
  fprintf(stderr, "map here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_29 = stgAllocCont( &it_alts_29);
      // load payload with FVs f
    ccont_alts_29->payload[0] = f; // f
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "map returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t53
DEFUN0(alts_29) {
  fprintf(stderr, "alts_29 here\n");
  Obj *ccont_alts_29 = stgPopCont();
  PtrOrLiteral f = ccont_alts_29->payload[0];
  PtrOrLiteral scrut_alts_29 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *rec_1 = stgNewHeapObj( &it_rec_1 );
      Obj *x_0 = stgNewHeapObj( &it_x_0 );
      Obj *res_0 = stgNewHeapObj( &it_res_0 );
      rec_1->payload[1] = f; // f
      rec_1->payload[2] = scrut_alts_29.op->payload[1]; // t
      x_0->payload[1] = f; // f
      x_0->payload[2] = scrut_alts_29.op->payload[0]; // h
      res_0->payload[0] = HOTOPL((Obj *)STGHEAPAT(5,2)); // x_0
      res_0->payload[1] = HOTOPL((Obj *)STGHEAPAT(8,3)); // rec_1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // res_0
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t53
DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  // map f t
  STGAPPLYPP(HOTOPL(&sho_map), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "rec_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// t53
DEFUN1(fun_x_0, self) {
  fprintf(stderr, "x_0 here\n");
  stgThunk(self);
  // f h
  STGAPPLYP(self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "x_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_output, self) {
  fprintf(stderr, "output here\n");
  stgThunk(self);
  Obj *mc_0 = stgNewHeapObj( &it_mc_0 );
  Obj *constSeven_0 = stgNewHeapObj( &it_constSeven_0 );
  constSeven_0->payload[1] = HOTOPL((Obj *)STGHEAPAT(3,2)); // mc_0
  // constSeven_0 ten
  STGAPPLYP(HOTOPL((Obj *)STGHEAPAT(2,1)), HOTOPL(&sho_ten));
  fprintf(stderr, "output returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t56.List[B] t56 -> Int[B] 
DEFUN1(fun_mc_0, self) {
  fprintf(stderr, "mc_0 here\n");
  stgThunk(self);
  // map const list3
  STGAPPLYPP(HOTOPL(&sho_map), HOTOPL(&sho_const), HOTOPL(&sho_list3));
  fprintf(stderr, "mc_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t61.t61 -> Int[B] 
DEFUN1(fun_constSeven_0, self) {
  fprintf(stderr, "constSeven_0 here\n");
  stgThunk(self);
  // head mc_0
  STGAPPLYP(HOTOPL(&sho_head), self.op->payload[1]);
  fprintf(stderr, "constSeven_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // eqInt output seven
  STGAPPLYPP(HOTOPL(&sho_eqInt), HOTOPL(&sho_output), HOTOPL(&sho_seven));
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

