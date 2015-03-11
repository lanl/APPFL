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
// each constructor has a single infotab entry
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

InfoTab it_y =
  { .name               = "y",
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
  // scrutinee is always stgCurVal
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
// mainfail = THUNK(mainfail)

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
  stgStatObj[stgStatObjCount++] = &sho_mainfail;
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
