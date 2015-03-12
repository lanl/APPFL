#ifndef stgcmm_h
#define stgcmm_h

#include "stg.h"
#include "cmm.h"

extern FnPtr stgUpdateCont();
extern InfoTab it_stgUpdateCont;
extern Obj sho_stgUpdateCont;

extern FnPtr stgCallCont();
extern InfoTab it_stgCallCont;

inline void stgThunk(PtrOrLiteral self) {
  stgPushCont( (Obj) {		
    .objType = UPDCONT,		
    .infoPtr = &it_stgUpdateCont,	
    .payload[0] = self		
  });				
  self.op->objType = BLACKHOLE;	
}

#endif
