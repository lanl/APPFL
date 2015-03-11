#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"
#include "predefs.h"

/*
  Every bind--global and local--in the program has a static InfoTab.
  Thus the program must be scanned to identify all such binds for codegen.
  Thus HeapObj to InfoTab is N-to-1 (where N may be 0);

  Top-level (global) binds have dynamically pre-allocated HeapObj.
  Thus the codegen for allocating HeapObj's can be uniform, but the
  code for the top-level allocations will be called before the user
  program starts.

  This file is hand-coded "codegen" to establish the data structures
  and patterns.
*/

DEFUN0(whiteHole) {
  fprintf(stderr,"in whiteHole, something not initialized somewhere, exiting\n");
  exit(0);
  RETURN0();
  ENDFUN;
}

// ****************************************************************

// since we always jump through the top of the stg stack we need some
// place to go when we're done
// this continuation is special, dropping from stg land to cmm land via RETURN0()
DEFUN0(stgShowResultCont) {
  fprintf(stderr,"done!\n");
  stgPopCont();  // clean up
  fprintf(stderr,"The answer is\n");
  showStgVal(stgCurVal);  
  RETURN0();
  ENDFUN;
}
InfoTab it_stgShowResultCont =
  { .name               = "stgShowResultCont",
    .entryCode          = &stgShowResultCont,
    .objType            = CALLCONT
  };
Obj sho_stgShowResultCont = {
  .infoPtr = &it_stgShowResultCont
};

// ****************************************************************

// unit = CON(Unit)
DEFUN0(Unit) {
  fprintf(stderr, "Unit here, this should not happen!\n");
  STGRETURN0();
  ENDFUN;
}
// each contstructor has a single infotab entry
InfoTab it_Unit =
  { .name               = "Unit",
    .entryCode          = &Unit,
    .objType            = CON,
    .conFields.tag      = TagUnit,
    .conFields.argCount = 0
  };
Obj sho_unit =
  { .objType = CON,
    .infoPtr = &it_Unit
  };


// ****************************************************************
// main_unit = 
//  THUNK(unit)

DEFUN1(main_unit, self) {
  //THUNK
  stgThunk(self);
  // unit
  stgCurVal = (PtrOrLiteral) { .argType = HEAPOBJ, .op = &sho_unit };
  STGRETURN0();
  ENDFUN;
}
InfoTab it_main_unit =
  { .name               = "main_unit",
    .entryCode          = &main_unit,
    .objType            = THUNK
  };
Obj sho_main_unit =
  { .objType = THUNK,
    .infoPtr = &it_main_unit,
  };

// ****************************************************************

// CON I
InfoTab it_I =
  { .name               = "I",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagI,
    .conFields.argCount = 1
  };


// one = CON(I 1)
Obj sho_one = 
  { .objType = CON,
    .infoPtr = &it_I,
    .payload[0].argType = INT,
    .payload[0].i = 1
  };

// two = CON(I 2)
Obj sho_two = 
  { .objType = CON,
    .infoPtr = &it_I,
    .payload[0].argType = INT,
    .payload[0].i = 2
  };

#define STGHEAPAT(n) ((Obj*)stgHP + (n))

// ****************************************************************
// main1 = 
//   THUNK(let { x = CON (I 3) } in x)

DEFUN1(main1, self) {
  stgThunk(self);
  // let { x = CON (I 3) }
  Obj *x = stgNewHeapObj();
  *x = (Obj) 
    { .objType = CON,
      .infoPtr = &it_I,
      .payload[0] = (PtrOrLiteral) {.argType = INT, .i = 3}
    };
  // in x
  stgCurVal = (PtrOrLiteral) { .argType = HEAPOBJ, .op = STGHEAPAT(-1) };

  STGRETURN0();
  ENDFUN;
}
InfoTab it_main1 =
  { .name               = "main1",
    .entryCode          = &main1,
    .objType            = THUNK
  };
Obj sho_main1 =
  { .objType = THUNK,
    .infoPtr = &it_main1,
  };

// ****************************************************************
// main2 = 
//   THUNK(let { x = CON (I 3);          x -3
//               y = THUNK( x );         y -2
//               z = THUNK( y ) } in z)  z -1

DEFUN1(y, self) {
  // THUNK
  stgThunk(self);
  // x is a non-global free variable
  stgCurVal = self.op->payload[0];
  STGRETURN0();  
  ENDFUN;
}

InfoTab it_y = {
  .name               = "y",
  .entryCode          = &y,
  .objType            = THUNK
};

DEFUN1(z, self) {
  // THUNK
  stgThunk(self);
  // y is a non-global free variable
  stgCurVal = self.op->payload[0];
  STGRETURN0();  
  ENDFUN;
}

InfoTab it_z =
  { .name               = "z",
    .entryCode          = &z,
    .objType            = THUNK
  };

DEFUN1(main2, self) {
  stgThunk(self);
  // let { x = CON (I 3) }
  Obj *x = stgNewHeapObj();
  *x = (Obj) 
    { .objType = CON,
      .infoPtr = &it_I,
      .payload[0] = (PtrOrLiteral) {.argType = INT, .i = 3}
    };
  // y = THUNK( x );
  Obj *y = stgNewHeapObj();
  *y = (Obj) 
    { .objType = THUNK,
      .infoPtr = &it_y,
      .payload[0] = (PtrOrLiteral) { .argType = HEAPOBJ, 
				     .op = STGHEAPAT(-2) }
    };
  // z = THUNK( y )
  Obj *z = stgNewHeapObj();
  *z = (Obj) 
    { .objType = THUNK,
      .infoPtr = &it_z,
      .payload[0] = (PtrOrLiteral) { .argType = HEAPOBJ, 
				     .op = STGHEAPAT(-2) }
    };
  // in z
  stgCurVal = (PtrOrLiteral) { .argType = HEAPOBJ, .op = STGHEAPAT(-1) };

  STGRETURN0();
  ENDFUN;
}
InfoTab it_main2 =
  { .name               = "main2",
    .entryCode          = &main2,
    .objType            = THUNK
  };
Obj sho_main2 =
  { .objType = THUNK,
    .infoPtr = &it_main2,
       };

// ****************************************************************
// main3 = 
//   THUNK(let { left  = CON(Left one);
//               right = CON(Right two) } in
//         case left of {
//           Left x -> right;
//           Right y -> y;
//           x -> x
//         })
// gotta love the typing--hey, it's polymorphic!

void derefStgCurVal() {
  while (stgCurVal.argType == HEAPOBJ && stgCurVal.op->objType == INDIRECT)
    stgCurVal = stgCurVal.op->payload[0];
}

DEFUN0(alts1) {
  Obj cont = stgPopCont();
  // scrutinee is alway stgCurVal
  // chase down any indirection
  derefStgCurVal();
  // winging it here, not exactly canonical
  if (stgCurVal.argType == HEAPOBJ && stgCurVal.op->objType == BLACKHOLE) {
    fprintf(stderr, "down a black hole!\n");
    exit(0);
  }
  if (stgCurVal.argType != HEAPOBJ ||
      stgCurVal.op->objType != CON ) goto casedefault;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
  case TagLeft:
    // variable saved in casecont
    stgCurVal = cont.payload[0];
    STGRETURN0();
  case TagRight:
    // from constructor
    stgCurVal = stgCurVal.op->payload[0];
    STGRETURN0();
  default:
casedefault:
    // naive compiler
    stgCurVal = stgCurVal;
    STGRETURN0();
  }
  ENDFUN;
}

InfoTab it_alts1 =
  { .name               = "alts1",
    .entryCode          = &alts1,
    .objType            = CASECONT
  };

InfoTab it_Left =
  { .name               = "Left",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagLeft,
    .conFields.argCount = 1
  };

InfoTab it_Right =
  { .name               = "Right",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagRight,
    .conFields.argCount = 1
  };

#define POLHO(HO) ((PtrOrLiteral) {.argType = HEAPOBJ, .op = HO })
#define POLLIT(L) ((PtrOrLiteral) {.argType = INT,     .i = L   })

DEFUN1(main3, self) {
  stgThunk(self);
  // let { left = CON(Left one)
  Obj *left = stgNewHeapObj();
  *left = (Obj) 
    { .objType = CON,
      .infoPtr = &it_Left,
      .payload[0] = (PtrOrLiteral) {.argType = HEAPOBJ, .op = &sho_one}
    };
  // right = CON(Right two)} in
  Obj *right = stgNewHeapObj();
  *right = (Obj) 
    { .objType = CON,
      .infoPtr = &it_Right,
      .payload[0] = (PtrOrLiteral) {.argType = HEAPOBJ, .op = &sho_two}
	 };
  // case 
  stgPushCont( (Obj) { 
      .objType = CASECONT,
	.infoPtr = &it_alts1,
	.payload[0] = POLHO(STGHEAPAT(-1)) });     // stash right
  // left
  stgCurVal = (PtrOrLiteral) {.argType = HEAPOBJ,
				 .op = STGHEAPAT(-2) };
  STGRETURN0(); // return through casecont, could just pop it and inline
  ENDFUN;
}
InfoTab it_main3 =
  { .name               = "main3",
    .entryCode          = &main3,
    .objType            = THUNK
  };
Obj sho_main3 =
  { .objType = THUNK,
    .infoPtr = &it_main3,
  };

// ****************************************************************

Obj *derefHO(Obj *op) {
  while (op->objType == INDIRECT)
    op = op->payload[0].op;
  return op;
}

Obj* derefPoL(PtrOrLiteral f) {
  assert(f.argType == HEAPOBJ && "derefPoL: not a HEAPOBJ");
  return derefHO(f.op);
}

DEFUN2(stgApplyPap1, f, x1) {
  // now our predefined function/macro approach breaks down somewhat
  // since we don't statically know the arity of the PAP's underlying
  // function and we don't want a giant switch, so explicitly push onto stack
  fprintf(stderr, "stgApplyPap1");
  assert(f.argType == HEAPOBJ && f.op->objType == PAP);
  assert(f.op->infoPtr->funFields.arity == f.op->argCount + 1); // 1 is Pap1
  _PUSH(x1); // push last arg
  for (int i = f.op->infoPtr->fvCount + // skip past free variables
	       f.op->argCount - 1;      // index of last arg
            // i != -1;  WRONG, must have left some crap on the stack
               i != f.op->infoPtr->fvCount - 1;
       i--) _PUSH(f.op->payload[i]);
  _PUSH(f);  // push self
  STGJUMP0(f.op->infoPtr->entryCode);
  ENDFUN;
}

DEFUN2(stgApply1, f, x1) {
  f.op = derefPoL(f);

  switch (f.op->objType) {

  case THUNK: { // seems like it would be more efficient to do while(THUNK)
    fprintf(stderr, "stgApply1 THUNK\n");
    Obj callCont = {.infoPtr = &it_stgCallCont, .payload[0] = x1};
    stgPushCont(callCont);
    STGCALL1(f.op->infoPtr->entryCode, f);  // result in stgCurVal
    f = stgCurVal;  // new f
    callCont = stgPopCont();
    x1 = callCont.payload[0];
    STGJUMP2(stgApply1, f, x1);
    break;
  } // case THUNK

  case FUN: {
    switch(f.op->infoPtr->funFields.arity) {
    case 1: { //just right
      fprintf(stderr, "stgApply1 FUN just right\n");
      STGJUMP2(f.op->infoPtr->entryCode, f, x1);
      break;
    } // case 1
    default: { // arity > 1, too few args
      fprintf(stderr, "stgApply1 FUN too few args\n");
      // build a PAP -- CAREFUL, COULD TRIGGER GC
      // need to solve this problem for "let" in general
      // one solution would be to have GC only possibly triggered by STGCALL
      Obj *pap = stgNewHeapObj();
      *pap = *f.op;  // quick and dirty
      pap->objType = PAP;
      pap->argCount = 1;
      pap->payload[pap->infoPtr->fvCount] = x1; // just after the fvs
      STGRETURN1(POLHO(pap));
      break;
    } // default
    } // switch
  } // case FUN

  case PAP: {
    switch(f.op->infoPtr->funFields.arity - f.op->argCount) {
    case 1: { // just right
      fprintf(stderr, "stgApply1 PAP just right\n");
      STGJUMP2(stgApplyPap1, f, x1);
      break;
    } // case 1
    default: { // too few args
      fprintf(stderr, "stgApply1 PAP too few args\n");
      f.op->payload[f.op->infoPtr->fvCount + f.op->argCount] = x1;  // reuse PAP for now
      f.op->argCount += 1;
      STGRETURN1(f);
      break;
    } // default
    } // switch
  } // case PAP

  default: {
    fprintf(stderr, "stgApply not a THUNK, FUN, or PAP\n");
    exit(0);
  } // default
  }  // switch
  ENDFUN;
}

// ****************************************************************
// try a generic stgApply function...

void callContSave(int argc, PtrOrLiteral argv[]) {
  Obj callCont;
  callCont.infoPtr = &it_stgCallCont;
  callCont.payload[0] = (PtrOrLiteral) {.argType = INT, .i = argc};
  for (int i = 0; i != argc; i++) callCont.payload[i+1] = argv[i];
  stgPushCont(callCont);
}

void callContRestore(PtrOrLiteral argv[]) {
  Obj callCont;
  callCont = stgPopCont();
  assert(callCont.payload[0].argType == INT);
  for (int i = 0; i != callCont.payload[0].i; i++) argv[i] = callCont.payload[i+1];
}

// argv points to the beginning of the arg list, but push backwards...
void pushargs(int argc, PtrOrLiteral argv[]) {
  for (int i = argc-1; i != -1; i--) _PUSH(argv[i]);
}
// ...and pop forwards
void popargs(int argc, PtrOrLiteral argv[]) {
  for (int i = 0; i != argc; i++) _POP(argv[i]);
}

void copyargs(PtrOrLiteral *dest, const PtrOrLiteral *src, int count) {
  for (int i = 0; i != count; i++) dest[i] = src[i];
}

#include "stgapply.c"

// ****************************************************************

// id = FUN( x -> x );
DEFUN2(id, self, x) {
  // x
  STGRETURN1(x);
  ENDFUN;
}
InfoTab it_id = {
  .name = "id",
  .entryCode = &id,
  .objType = FUN,
  .funFields.arity = 1
};
Obj sho_id = {
  .objType = FUN,
  .infoPtr = &it_id
};

// main4 =
//   THUNK( id unit )

DEFUN1(main4, self) {
  stgThunk(self);
  // id and unit are top-level
  STGJUMP2(stgApply1, POLHO(&sho_id), POLHO(&sho_unit));
  ENDFUN;
}
InfoTab it_main4 =
  { .name               = "main4",
    .entryCode          = &main4,
    .objType            = THUNK
  };
Obj sho_main4 =
  { .objType = THUNK,
    .infoPtr = &it_main4,
  };

// ****************************************************************
// constf = FUN( x y -> x );
DEFUN3(constf, self, x, y) {
  // x
  fprintf(stderr, "constf here\n");
  STGRETURN1(x);
  ENDFUN;
}
InfoTab it_constf = {
  .name = "constf",
  .entryCode = &constf,
  .objType = FUN,
  .fvCount = 0,
  .funFields.arity = 2
};
Obj sho_constf = {
  .objType = FUN,
  .infoPtr = &it_constf
};

// const_one = THUNK(constf one)
DEFUN1(const_one, self) {
  fprintf(stderr, "THUNK(constf one) here\n");
  stgThunk(self);
  // constf and one are top-level

  // STGJUMP2(stgApply1, POLHO(&sho_constf), POLHO(&sho_one));

  _PUSH(POLHO(&sho_one));
  PtrOrLiteral N = {.argType = INT, .i = 1};
  STGJUMP2(stgApply, N, POLHO(&sho_constf));

  ENDFUN;
}
InfoTab it_const_one =
  { .name               = "const_one",
    .entryCode          = &const_one,
    .objType            = THUNK,
    .fvCount            = 0,
  };
Obj sho_const_one =
  { .objType = THUNK,
    .infoPtr = &it_const_one,
  };

// main5 =
//   THUNK( const_one unit )

DEFUN1(main5, self) {
  fprintf(stderr, "THUNK(const_one unit) here\n");
  stgThunk(self);
  // constf and unit are top-level

  // STGJUMP2(stgApply1, POLHO(&sho_const_one), POLHO(&sho_unit));

  _PUSH(POLHO(&sho_unit));
  PtrOrLiteral N = {.argType = INT, .i = 1};
  STGJUMP2(stgApply, N, POLHO(&sho_const_one));

  ENDFUN;
}
InfoTab it_main5 =
  { .name               = "main5",
    .entryCode          = &main5,
    .objType            = THUNK,
    .fvCount            = 0,
  };
Obj sho_main5 =
  { .objType = THUNK,
    .infoPtr = &it_main5,
  };

// ****************************************************************
// mainfail = THUNK(mainfail) - this works but displaying the heap doesn't detect cycles
Obj sho_mainfail;

DEFUN1(mainfail, self) {
  stgThunk(self);
  stgCurVal = (PtrOrLiteral) { .argType = HEAPOBJ, .op = &sho_mainfail };
  STGRETURN0();
  ENDFUN;
}

InfoTab it_mainfail =
  { .name               = "mainfail",
    .entryCode          = &mainfail,
    .objType            = THUNK
  };
Obj sho_mainfail =
  { .objType = THUNK,
    .infoPtr = &it_mainfail,
  };

// ****************************************************************

void initPredefs() {
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_main_unit;
  stgStatObj[stgStatObjCount++] = &sho_main1;
  stgStatObj[stgStatObjCount++] = &sho_main3;
  stgStatObj[stgStatObjCount++] = &sho_main5;
  stgStatObj[stgStatObjCount++] = &sho_main4;
  stgStatObj[stgStatObjCount++] = &sho_id;
  stgStatObj[stgStatObjCount++] = &sho_constf;
  stgStatObj[stgStatObjCount++] = &sho_mainfail;
}

DEFUN0(start) {
  stgPushCont(sho_stgShowResultCont);
  stgCurVal = (PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main5};
  while (stgCurVal.argType == HEAPOBJ &&
	 stgCurVal.op->objType == THUNK) {
    stgPushCont((Obj){.infoPtr = &it_stgCallCont});
    STGCALL1(stgCurVal.op->infoPtr->entryCode, stgCurVal);
    stgPopCont();
  }
  STGRETURN0(); // return through stgShowResultCont
  ENDFUN;
}

/*
DEFUN2(primAdd, x, y) {

  ENDFUN;
}

// main3 = 
//   let { x = CON (I 3);
//         f = FUN y -> x }
//   in f x
InfoTab it_main3 =
  { .name               = "main3",
    .entryCode          = &main3,
    .objType            = THUNK
  };

Obj sho_main3 =
  { .objType = THUNK,
    .infoPtr = &it_main3,
  };

// main3 = 
//   let { xx = CON (I 3);
//         ff = FUN (y -> x) }
//   in ff xx

InfoTab it_ff =
  { .name               = "ff",
    .entryCode          = &ff,
    .objType            = FUN,
    .funFields.arity    = 1
  };

DEFUN0(ff) {
  ENDFUN;
}

DEFUN0(main3) {
  Obj *x = stgNewHeapObj();
  *x = (Obj) 
    { .objType = CON,
      .infoPtr = &it_I,
      .payload[0] = (PtrOrLiteral) {.argType = INT, .i = 3}
    };
  // in x
  stgCurVal = (PtrOrLiteral) { .argType = HEAPOBJ, .op = x };
  CmmFnPtr go = ((Obj *)stgSP)->infoPtr->entryCode;
  fprintf(stderr,"leaving main3\n");
  JUMP0(  ((CMMVal) { .valTag = CMMFP, .cmmFP = (FnPtr)go }) );
  ENDFUN;
}

*/
