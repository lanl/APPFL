#include <stdlib.h>
#include <stdio.h>
#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"

extern void stgThunk(PtrOrLiteral self);

DEFUN0(stgUpdateCont) {
  Obj cont = stgPopCont();
  assert(cont.objType == UPDCONT && "I'm not an UPDCONT!");
  PtrOrLiteral p = cont.payload[0];
  assert(p.argType == HEAPOBJ && "not a HEAPOBJ!");
  assert(p.op->objType == BLACKHOLE && "not a BLACKHOLE!");
  fprintf(stderr, "stgUpdateCont updating\n  ");
  showStgObj(p.op);
  fprintf(stderr, "with\n  ");
  showStgObj(stgCurVal.op);
  p.op->objType = INDIRECT;
  p.op->payload[0] = stgCurVal;
  STGRETURN0();
  ENDFUN
}
InfoTab it_stgUpdateCont =
  { .name               = "stgUpdateCont",
    .entryCode          = &stgUpdateCont,
    .objType            = UPDCONT
  };
Obj sho_stgUpdateCont = {
  .infoPtr = &it_stgUpdateCont
};

DEFUN0(stgCallCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgCallCont returning\n");
  RETURN0();
  ENDFUN;
}

InfoTab it_stgCallCont =
  { .name               = "stgCallCont",
    .entryCode          = &stgCallCont,
    .objType            = CALLCONT
  };

