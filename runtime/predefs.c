#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"
#include "stgutils.h"
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

// ****************************************************************
// constructors
// these could overlap--make distinct for debugging purposes now
//
// nullary constructors:  these could have SHO as an optimization
//   skip that for now?

typedef enum {
  TagUnit  = 0,
  TagFalse = 1,
  TagTrue  = 2,
  TagI     = 3,
  TagLeft  = 4,
  TagRight = 5,
  TagNil   = 6,
  TagCons  = 7,
} TagVal;

// Each constructor has a single infotab entry.  This works because
// constructors are always saturated and InfoTab.fvCount

// CON(Unit)
InfoTab it_Unit =
  { .name               = "Unit",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagUnit,
    .conFields.arity = 0,
  };

// CON(False)
InfoTab it_False =
  { .name               = "False",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagFalse,
    .conFields.arity = 0,
  };

// CON(True)
InfoTab it_True =
  { .name               = "True",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagTrue,
    .conFields.arity = 0,
  };

// CON(I _)
InfoTab it_I =
  { .name               = "I",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagI,
    .conFields.arity = 1,
  };

// CON(Left _)
InfoTab it_Left =
  { .name               = "Left",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagLeft,
    .conFields.arity = 1,
  };

// CON(Right _)
InfoTab it_Right =
  { .name               = "Left",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagRight,
    .conFields.arity = 1,
  };

// CON(Nil)
InfoTab it_Nil =
  { .name               = "Nil",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagNil,
    .conFields.arity = 0,
  };

// CON(Cons _ _)
InfoTab it_Cons =
  { .name               = "Cons",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = TagCons,
    .conFields.arity = 2,
  };

// ****************************************************************
// TLDs
//
// by definition TLDs have no free variables--heap offsets are static

// unit = CON(Unit)
Obj sho_unit =
  { .objType = CON,
    .infoPtr = &it_Unit,
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

// true = CON(True)
Obj sho_true = 
  { .objType = CON,
    .infoPtr = &it_True,
  };

// false = CON(False)
Obj sho_false = 
  { .objType = CON,
    .infoPtr = &it_False,
  };

// ****************************************************************
// main_thunk_unit = 
//  THUNK(unit)

DEFUN1(main_thunk_unit, self) {
  //THUNK
  stgThunk(self);
  // unit
  stgCurVal = HOTOPL(&sho_unit);
  STGRETURN0();
  ENDFUN;
}
InfoTab it_main_thunk_unit =
  { .name               = "main_thunk_unit",
    .entryCode          = &main_thunk_unit,
    .objType            = THUNK,
    .fvCount            = 0,
  };
Obj sho_main_thunk_unit =
  { .objType = THUNK,
    .infoPtr = &it_main_thunk_unit,
  };

// ****************************************************************

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
      .payload[0] = INTTOPL(3),
    };
  // in x
  stgCurVal = HOTOPL(STGHEAPAT(-1));
  STGRETURN0();
  ENDFUN;
}
InfoTab it_main1 =
  { .name               = "main1",
    .entryCode          = &main1,
    .objType            = THUNK,
    .fvCount = 0,
  };
Obj sho_main1 =
  { .objType = THUNK,
    .infoPtr = &it_main1,
  };

// ****************************************************************
// main2 = 
//   THUNK(let { x = CON (I 3);          x -3
//               y = THUNK( x );         y -2
//               z = THUNK( y ) }        z -1
//         in z)

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
  .objType            = THUNK,
  .fvCount            = 1,
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
    .objType            = THUNK,
    .fvCount            = 1,
  };

DEFUN1(main2, self) {
  stgThunk(self);
  // let { x = CON (I 3) }
  Obj *x = stgNewHeapObj();
  *x = (Obj) 
    { .objType = CON,
      .infoPtr = &it_I,
      .payload[0] = INTTOPL(3),
    };
  // y = THUNK( x );
  Obj *y = stgNewHeapObj();
  *y = (Obj) 
    { .objType = THUNK,
      .infoPtr = &it_y,
      .payload[0] = HOTOPL(STGHEAPAT(-2)),
    };
  // z = THUNK( y )
  Obj *z = stgNewHeapObj();
  *z = (Obj) 
    { .objType = THUNK,
      .infoPtr = &it_z,
      .payload[0] = HOTOPL(STGHEAPAT(-2)),
    };
  // in z
  stgCurVal = HOTOPL(STGHEAPAT(-1));

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
// gotta love the typing--hey, it's superpolymorphic!

DEFUN0(alts1) {
  // scrutinee is alway stgCurVal
  // chase down any indirection--lets require that eval was nice to us
  // derefStgCurVal();
  // winging it here, not exactly canonical
  // and let's require that this be part of the eval process
  /*
  if (stgCurVal.argType == HEAPOBJ && stgCurVal.op->objType == BLACKHOLE) {
    fprintf(stderr, "down a black hole!\n");
    exit(0);
  }
  */
  Cont cont = stgPopCont();
  PtrOrLiteral ctor = stgCurVal;
  if (stgCurVal.argType != HEAPOBJ ||
      stgCurVal.op->objType != CON ) goto casedefault;
  switch(ctor.op->infoPtr->conFields.tag) {
  case TagLeft:
    // variable saved in casecont
    stgCurVal = cont.payload[0];
    STGRETURN0();
  case TagRight:
    // from constructor
    stgCurVal = ctor.op->payload[0];
    STGRETURN0();
  default:
casedefault:
    stgCurVal = ctor;
    STGRETURN0();
  }
  ENDFUN;
}

InfoTab it_alts1 =
  { .name               = "alts1",
    .entryCode          = &alts1,
    .fvCount            = 1,
    .objType            = CASECONT,
  };

DEFUN1(main3, self) {
  stgThunk(self);
  // let { left = CON(Left one)
  Obj *left = stgNewHeapObj();
  *left = (Obj) 
    { .objType = CON,
      .infoPtr = &it_Left,
      .payload[0] = HOTOPL(&sho_one),
    };
  // right = CON(Right two)} in
  Obj *right = stgNewHeapObj();
  *right = (Obj) 
    { .objType = CON,
      .infoPtr = &it_Right,
      .payload[0] = HOTOPL(&sho_two),
    };
  // case 
  stgPushCont( (Cont)
	       { .objType = CASECONT,
		   .retAddr = &alts1,
		   .payload[0] = HOTOPL(STGHEAPAT(-1)) });     // stash right
  // left of
  // rule for var is deposit in stgCurVal
  stgCurVal = (PtrOrLiteral) {.argType = HEAPOBJ, .op = STGHEAPAT(-2) };
  // rule for case is to evaluate whatever expression e left behind
  STGEVAL(stgCurVal);
  STGRETURN0(); // return through casecont if didn't already jump
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
  STGJUMP2(stgApply1, HOTOPL(&sho_id), HOTOPL(&sho_unit));
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
  STGAPPLY1(HOTOPL(&sho_constf), HOTOPL(&sho_one));
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
  STGAPPLY1(HOTOPL(&sho_const_one), HOTOPL(&sho_unit));
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
// mainfail = THUNK(mainfail) - this works but displaying the heap doesn't 
// correctly detect cycles
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
  stgStatObj[stgStatObjCount++] = &sho_main_thunk_unit;
  stgStatObj[stgStatObjCount++] = &sho_main1;
  stgStatObj[stgStatObjCount++] = &sho_main3;
  stgStatObj[stgStatObjCount++] = &sho_main5;
  stgStatObj[stgStatObjCount++] = &sho_main4;
  stgStatObj[stgStatObjCount++] = &sho_id;
  stgStatObj[stgStatObjCount++] = &sho_constf;
  stgStatObj[stgStatObjCount++] = &sho_mainfail;
}

// Note this definition not only handles THUNKs returning THUNKs,
// but also the general case of e.g. main = CON(I 1)
DEFUN0(start) {
  initPredefs();

  stgPushCont(showResultCont);  // nothing to save or restore

  /*
  stgCurVal = (PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main5};
  while (stgCurVal.argType == HEAPOBJ &&
	 stgCurVal.op->objType == THUNK) {
    stgPushCont((Obj){.infoPtr = &it_stgCallCont,
	              .objType = CALLCONT,
	              .payload[0] = {0}});
    STGCALL1(stgCurVal.op->infoPtr->entryCode, stgCurVal);
    stgPopCont();
  }
  if (stgCurVal.argType == HEAPOBJ && 
      stgCurVal.op->objType == BLACKHOLE) {
    fprintf(stderr, "infinite loop detected in start!\n");
    exit(0);
  }
  */

  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main3}));

  STGRETURN0(); // return through stgShowResultCont
  ENDFUN;
}

