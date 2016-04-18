#ifndef heap_h
#define heap_h

#include "stg.h"

// allocate Obj on heap, returning pointer to new Obj
Obj* stgNewHeapObj(InfoTab *itp);
Obj* stgNewHeapPAP(InfoTab *itp, int pargc, int nargc);
Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bitmap);

#endif // ifndef heap_h
