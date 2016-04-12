#ifndef stack_h
#define stack_h

#include "stg.h"

static inline CInfoTab *getCInfoPtr(Cont *p)  { return p->cInfoPtr; }

Bitmap64 cLayoutInfoToBitmap64(CLayoutInfo *lip);

static inline ContType getContType(Cont *p) {
  return p->contType;
}



#endif // ifndef stack_h
