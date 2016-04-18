#ifndef heap_h
#define heap_h

#include "stg.h"

Obj* stgNewHeapObj(InfoTab *itp);
Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bm);

#endif
