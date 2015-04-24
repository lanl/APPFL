#ifndef stgcmm_h
#define stgcmm_h

#include "stg.h"
#include "cmm.h"
#include "string.h"

extern FnPtr stgUpdateCont();
extern FnPtr stgCallCont();

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

#endif

