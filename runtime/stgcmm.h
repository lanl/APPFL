#ifndef stgcmm_h
#define stgcmm_h

#include "stg.h"
#include "cmm.h"

extern FnPtr stgUpdateCont();
extern FnPtr stgCallCont();

inline void stgThunk(PtrOrLiteral self) {
  stgPushCont( (Cont) {		
    .retAddr = &stgUpdateCont,	
    .objType = UPDCONT,		
    .payload[0] = self		
  });				
  self.op->objType = BLACKHOLE;	
}

#endif

