#ifndef gc_h
#define gc_h

#include "stg.h"
#include "args.h"

void initGc(void);

/* moved to stg.h
void gc(void);

define GC() if(stgHP-stgHeap > GCThreshold*stgHeapSize) gc();
*/

#endif
