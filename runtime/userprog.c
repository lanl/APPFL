#include "stg_header.h"
void registerSHOs();
FnPtr tnk_main();
FnPtr tnk_main1();
FnPtr fun_multInt();
FnPtr alts_0();
FnPtr alts_1();
FnPtr alts_2();
FnPtr fun_plusInt();
FnPtr alts_3();
FnPtr alts_4();
FnPtr alts_5();
FnPtr fun_subInt();
FnPtr alts_6();
FnPtr alts_7();
FnPtr alts_8();
FnPtr fun_eqInt();
FnPtr alts_9();
FnPtr alts_10();
FnPtr alts_11();
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &tnk_main,
    .objType             = THUNK,
  };
InfoTab it_main1 = 
  { .name                = "main1",
    .fvCount             = 0,
    .entryCode           = &tnk_main1,
    .objType             = THUNK,
  };
InfoTab it_list1 = 
  { .name                = "list1",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_list2 = 
  { .name                = "list2",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_list3 = 
  { .name                = "list3",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_error = 
  { .name                = "error",
    .fvCount             = 0,
    .entryCode           = &bhl_error,
    .objType             = BLACKHOLE,
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "Unit",
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 2,
    .conFields.conName   = "Nil",
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
InfoTab it_two = 
  { .name                = "two",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
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
InfoTab it_four = 
  { .name                = "four",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_five = 
  { .name                = "five",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_six = 
  { .name                = "six",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_seven = 
  { .name                = "seven",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_eight = 
  { .name                = "eight",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_nine = 
  { .name                = "nine",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_ten = 
  { .name                = "ten",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_multInt = 
  { .name                = "multInt",
    .fvCount             = 0,
    .entryCode           = &fun_multInt,
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
InfoTab it_plusInt = 
  { .name                = "plusInt",
    .fvCount             = 0,
    .entryCode           = &fun_plusInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_1 = 
  { .name                = "result_1",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_subInt = 
  { .name                = "subInt",
    .fvCount             = 0,
    .entryCode           = &fun_subInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_2 = 
  { .name                = "result_2",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_eqInt = 
  { .name                = "eqInt",
    .fvCount             = 0,
    .entryCode           = &fun_eqInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };

extern Obj sho_main;
extern Obj sho_main1;
extern Obj sho_list1;
extern Obj sho_list2;
extern Obj sho_list3;
extern Obj sho_error;
extern Obj sho_unit;
extern Obj sho_nil;
extern Obj sho_zero;
extern Obj sho_one;
extern Obj sho_two;
extern Obj sho_three;
extern Obj sho_four;
extern Obj sho_five;
extern Obj sho_six;
extern Obj sho_seven;
extern Obj sho_eight;
extern Obj sho_nine;
extern Obj sho_ten;
extern Obj sho_multInt;
extern Obj sho_plusInt;
extern Obj sho_subInt;
extern Obj sho_eqInt;

Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };
Obj sho_main1 =
{
  .infoPtr   = &it_main1,
  .objType   = THUNK,
  .ident     = "main1",
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
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
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
Obj sho_two =
{
  .infoPtr   = &it_two,
  .objType   = CON,
  .ident     = "two",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 2,
  };
Obj sho_three =
{
  .infoPtr   = &it_three,
  .objType   = CON,
  .ident     = "three",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 3,
  };
Obj sho_four =
{
  .infoPtr   = &it_four,
  .objType   = CON,
  .ident     = "four",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 4,
  };
Obj sho_five =
{
  .infoPtr   = &it_five,
  .objType   = CON,
  .ident     = "five",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 5,
  };
Obj sho_six =
{
  .infoPtr   = &it_six,
  .objType   = CON,
  .ident     = "six",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 6,
  };
Obj sho_seven =
{
  .infoPtr   = &it_seven,
  .objType   = CON,
  .ident     = "seven",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 7,
  };
Obj sho_eight =
{
  .infoPtr   = &it_eight,
  .objType   = CON,
  .ident     = "eight",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 8,
  };
Obj sho_nine =
{
  .infoPtr   = &it_nine,
  .objType   = CON,
  .ident     = "nine",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 9,
  };
Obj sho_ten =
{
  .infoPtr   = &it_ten,
  .objType   = CON,
  .ident     = "ten",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 10,
  };
Obj sho_multInt =
{
  .infoPtr   = &it_multInt,
  .objType   = FUN,
  .ident     = "multInt",
  };
Obj sho_plusInt =
{
  .infoPtr   = &it_plusInt,
  .objType   = FUN,
  .ident     = "plusInt",
  };
Obj sho_subInt =
{
  .infoPtr   = &it_subInt,
  .objType   = FUN,
  .ident     = "subInt",
  };
Obj sho_eqInt =
{
  .infoPtr   = &it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_main;
  stgStatObj[stgStatObjCount++] = &sho_main1;
  stgStatObj[stgStatObjCount++] = &sho_list1;
  stgStatObj[stgStatObjCount++] = &sho_list2;
  stgStatObj[stgStatObjCount++] = &sho_list3;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_four;
  stgStatObj[stgStatObjCount++] = &sho_five;
  stgStatObj[stgStatObjCount++] = &sho_six;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_eight;
  stgStatObj[stgStatObjCount++] = &sho_nine;
  stgStatObj[stgStatObjCount++] = &sho_ten;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_plusInt;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
}


DEFUN1(tnk_main, self) {
  fprintf(stderr, "main here\n");  stgThunk(self);
  stgCurVal = HOTOPL(&sho_main1);
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_main1, self) {
  fprintf(stderr, "main1 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_multInt), HOTOPL(&sho_three), HOTOPL(&sho_four));
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
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
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_1,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_1) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_2,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i * (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_2) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      Obj *result_0 = stgNewHeapObj();
      *result_0 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_0,
              .ident = "result_0",
              .payload[0] = scrutinee,
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_plusInt, self, x, y) {
  fprintf(stderr, "plusInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_3,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_3) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_4,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_4) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_5,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i + (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_5) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      Obj *result_1 = stgNewHeapObj();
      *result_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_1,
              .ident = "result_1",
              .payload[0] = scrutinee,
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_6,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_6) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_7,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_7) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_8,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i - (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_8) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      Obj *result_2 = stgNewHeapObj();
      *result_2 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_2,
              .ident = "result_2",
              .payload[0] = scrutinee,
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_9,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_9) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_10,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_10) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_11,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i - (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_11) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      stgCurVal = (scrutinee).i?HOTOPL(&sho_True):HOTOPL(&sho_False);
      STGRETURN0();
    }
  }
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

