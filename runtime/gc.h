#ifndef gc_h
#define gc_h

#include "stg.h"
#include "args.h"
#include "log.h"

void initGc(void);

/* moved to stg.h
 void gc(void);
 
define GC() if(stgHP-stgHeap > GCThreshold*stgHeapSize) gc();
*/

#define EXTRASTART() LOG(LOG_SPEW, "EXTRA check file %s line %d\n", __FILE__, __LINE__)
#define EXTRAEND() LOG(LOG_SPEW, "EXTRA check succeeded %s %d\n", __FILE__, __LINE__)

#endif // ifndef gc_h
