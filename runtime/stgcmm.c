#include <stdlib.h>
#include <stdio.h>
#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"

/*
inline DEFUN0(stgUpdateCont) {
  Cont cont = stgPopCont();
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
*/

DEFUN0(stgCallCont) {
  // stgPopCont();  user must do this
  fprintf(stderr,"stgCallCont returning\n");
  RETURN0();
  ENDFUN;
}

InfoTab it_stgCallCont =
  { .name = "stgCallCont",
    .fvCount = 0,
    .entryCode = &stgCallCont,
    .objType = CALLCONT,
  };

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

/*
inline void stgThunk(PtrOrLiteral self) {
  assert(self.argType == HEAPOBJ && "stgThunk:  not HEAPOJ\n");
  Cont ccont = {		
    .retAddr = &stgUpdateCont,	
    .objType = UPDCONT,
    .payload[0] = self		
  };
  // should make all .idents (char *)
  strcpy(ccont.ident, self.op->ident);

  stgPushCont(ccont);				
  self.op->objType = BLACKHOLE;	
}
*/

InfoTab it_stgUpdateCont =
  { .name = "stgUpdateCont",
    .fvCount = 1,  // self
    .entryCode = &stgUpdateCont,
    .objType = UPDCONT,
  };

void stgThunk(PtrOrLiteral self) {
  assert(self.argType == HEAPOBJ && "stgThunk:  not HEAPOJ\n");
  Obj ccont = {		
    .infoPtr = &it_stgUpdateCont,
    .objType = UPDCONT,
    // .ident = "stgUpdateCont", // inherit from self
    .payload[0] = self,
  };
  // should make all .idents (char *)
  strcpy(ccont.ident, self.op->ident);

  stgPushCont(ccont);				
  self.op->objType = BLACKHOLE;	
}

