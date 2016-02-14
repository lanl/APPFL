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

#define EXTRASTART() log(LOG_SPEW, "EXTRA check file %s line %d\n", __FILE__, __LINE__)
#define EXTRAEND() log(LOG_SPEW, "EXTRA check succeeded %s %d\n", __FILE__, __LINE__)

static inline __attribute__((always_inline))
void setArgType(PtrOrLiteral *f, ArgType type) {
#if USE_ARGTYPE
  f->argType = type;
#endif
}

#endif
