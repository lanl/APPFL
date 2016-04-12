#ifndef stack_h
#define stack_h

#include "stg.h"

static inline CInfoTab *getCInfoPtr(Cont *p)  { return p->cInfoPtr; }

Bitmap64 cLayoutInfoToBitmap64(CLayoutInfo *lip);

static inline ContType getContType(Cont *p) {
  return p->contType;
}

int  getContSize(Cont *);

void stgPopContIfPopMe();

// allocate Obj on continuation stack, returning pointer to new Obj
extern Cont *stgAllocCallOrStackCont(CInfoTab *it, int payloadSize);
extern Cont *stgAllocCont(CInfoTab *it);
// remove Obj from top of continuation stack, returning pointer to de-alloced Obj
void stgPopCont();
// get top of stack pointer, must be STACKCONT
Cont *stgGetStackArgp();
Cont *stgJumpAdjust();
Cont *stgAdjustTopContSize(Cont *cp, int delta);

#endif // ifndef stack_h
