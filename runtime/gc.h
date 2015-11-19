#ifndef gc_h
#define gc_h

#include "stg.h"
#include "args.h"

void initGc(void);

void gc(void);

#define GC() if(stgHP-stgHeap > GCThreshold*stgHeapSize) gc();

#endif
