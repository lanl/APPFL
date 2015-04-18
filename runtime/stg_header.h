/*
File included at top of every program generated
by the stgc codegenerator. 

Maybe stgc.h would be a better name.
*/

#ifndef stg_header_h
#define stg_header_h

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"
#include "stgutils.h"
#include "gc.h"

// every manifest heap object is introduced by a "let" so has
// a name.  However, for a CON(C,...) we can use the same InfoTab
// entry for each manifest occurrence, so we'll use the constructor
// name rather than uniqued version of name of variable bound to
// Also true for known functions?

extern FnPtr start();

// CON(False)
InfoTab it_False =
  { .name               = "False",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = 0, // must match what is in ConMap2.hs!
    .conFields.arity = 0,
  };

// false = CON(False)
Obj sho_False = 
  { .objType = CON,
    .infoPtr = &it_False,
  };

// CON(True)
InfoTab it_True =
  { .name               = "True",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = 1, // must match what is in ConMap2.hs!
    .conFields.arity = 0,
  };

// true = CON(True)
Obj sho_True = 
  { .objType = CON,
    .infoPtr = &it_True,
  };

// stg_case_not_exhaustive = FUN( x ->  );
DEFUN2(stg_case_not_exhaustive, self, x) {
  fprintf(stderr, "stg_case_not_exhaustive!\n");
  // display x
  exit(0);
  ENDFUN;
}
InfoTab it_stg_case_not_exhaustive = {
  .name = "stg_case_not_exhaustive",
  .entryCode = &stg_case_not_exhaustive,
  .objType = FUN,
  .fvCount = 0,
  .funFields.arity = 1,
};
Obj sho_stg_case_not_exhaustive = {
  .objType = FUN,
  .infoPtr = &it_stg_case_not_exhaustive,
};

// BLACKHOLE = THUNK();
DEFUN1(bhl_error, self) {
  fprintf(stderr, "bhl_error!\n");
  exit(0);
  ENDFUN;
}
InfoTab it_bhl_error = {
  .name = "bhl_error",
  .entryCode = &bhl_error,
  .objType = THUNK,
  .fvCount = 0,
};
Obj sho_bhl_error = {
  .objType = THUNK,
  .infoPtr = &it_bhl_error,
};

#endif
