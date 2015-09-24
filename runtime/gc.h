#ifndef gc_h
#define gc_h

#include "stg.h"
#include "args.h"

static void *toPtr, *fromPtr;
static void *scanPtr, *freePtr;

static inline bool isFrom(void *p) {
  return (p >= fromPtr && (char *) p < (char *) fromPtr + stgHeapSize / 2);
}

static inline bool isTo(void *p) {
  return (p >= toPtr && (char *) p < (char *) toPtr + stgHeapSize / 2);
}

void initGc(void);

void gc(void);

#define GC() if(stgHP-stgHeap > GCThreshold*stgHeapSize) gc();

#endif
