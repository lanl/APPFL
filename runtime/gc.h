#ifndef gc_h
#define gc_h

#include "stg.h"

// fraction of total heap used before gc runs.
static const float gcThreshold=0.0;

void initGc(void);

void gc(void);

#define GC() if(stgHP-stgHeap > gcThreshold*stgHeapSize) gc();

#endif
