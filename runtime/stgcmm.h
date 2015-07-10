#ifndef stgcmm_h
#define stgcmm_h

#include "stg.h"
#include "cmm.h"
#include "string.h"

extern void stgThunk(PtrOrLiteral self);

extern FnPtr stgCallCont();
extern InfoTab it_stgCallCont;

extern FnPtr stgUpdateCont();
extern InfoTab it_stgUpdateCont;

#endif

