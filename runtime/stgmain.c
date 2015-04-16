#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"
#include "stgutils.h"
#include "stgmain.h"

// CON(False)
InfoTab it_False =
  { .name               = "False",
    .entryCode          = &whiteHole,
    .objType            = CON,
    .conFields.tag      = 100,
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
    .conFields.tag      = 101,
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

// user progream including registerSHOs() and sho_main()
#include "userprog.c"

DEFUN0(start) {
  registerSHOs();  // in userprog.c

  stgPushCont(showResultCont);  // nothing to save or restore

  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));

  STGRETURN0(); // return through stgShowResultCont
  ENDFUN;
}

