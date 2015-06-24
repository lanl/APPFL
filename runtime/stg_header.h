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

#include "cruntime.h"

// every manifest heap object is introduced by a "let" so has
// a name.  However, for a CON(C,...) we can use the same InfoTab
// entry for each manifest occurrence, so we'll use the constructor
// name rather than uniqued version of name of variable bound to
// Also true for known functions?

extern FnPtr start();

/*
// false = CON(False)
InfoTab it_false =
  { .name               = "false",
    .entryCode          = &stg_constructorcall,
    .objType            = CON,
    .conFields.tag      = 0, // must match what is in ConMap2.hs!
    .conFields.arity    = 0,
    .conFields.conName  = "False"
  };

// false = CON(False)
Obj sho_false = 
  { .objType = CON,
    .infoPtr = &it_false,
  };

// true = CON(True)
InfoTab it_true =
  { .name               = "true",
    .entryCode          = &stg_constructorcall,
    .objType            = CON,
    .conFields.tag      = 1, // must match what is in ConMap2.hs!
    .conFields.arity = 0,
    .conFields.conName  = "True"
  };

Obj sho_true = 
  { .objType = CON,
    .infoPtr = &it_true,
  };
*/
// stg_case_not_exhaustive = FUN( x ->  );
DEFUN2(stg_case_not_exhaustive, self, x) {
  fprintf(stderr, "stg_case_not_exhaustive: ");
  showStgVal(x);
  fprintf(stderr, "\n");
  showStgHeap();
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
  .ident = "stg_case_not_exhaustive",
};

// BLACKHOLE = THUNK();
DEFUN1(stg_error, self) {
  fprintf(stderr, "fun_error (BLACKHOLE)!\n");
  exit(0);
  ENDFUN;
}

// InfoTab it_error = {
//   .name = "stg_error",
//   .entryCode = &fun_error,
//   .objType = THUNK,
//   .fvCount = 0,
// };
// 
// Obj sho_error = {
//   .objType = THUNK,
//   .infoPtr = &it_error,
// };

#endif
