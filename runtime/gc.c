#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>

#include "gc.h"
#include "heap.h"
#include "stg.h"
#include "stgutils.h"
#include "obj.h"
#include "options.h"
#include "sanity.h"
#include "args.h"

static void *scanPtr, *freePtr;

__attribute__((always_inline)) inline void setHeapArgType(PtrOrLiteral *f) {
  #if USE_ARGTYPE
        f->argType = HEAPOBJ;
  #endif
}

void initGc(void) {
  assert(stgHeap && "gc: heap not defined");
  fromPtr = stgHeap;
  toPtr = (char *) stgHeap + stgHeapSize / 2;
  scanPtr = toPtr;
  freePtr = toPtr;
}

void swapPtrs(void) {
  assert(scanPtr == freePtr && "gc: not finished");
  size_t before = stgHP - stgHeap;
  stgHeap = toPtr;
  stgHP = freePtr;
  toPtr = fromPtr;
  fromPtr = stgHeap;
  freePtr = toPtr;
  scanPtr = toPtr;
  assert(stgHP - stgHeap <= before && "gc: increased heap size!\n");
}

Obj *deref(Obj *p) {
  while (!(isLSBset(p->_infoPtr))) {
    if (getObjType(p) != INDIRECT) {
      return p;
    }
    p = p->payload[0].op;
  }
  return p;
}

PtrOrLiteral updatePtrByValue (PtrOrLiteral f) {
  assert(mayBeBoxed(f) && "not a HEAPOBJ");
  if (f.op == NULL) return f;

  Obj *p = deref(f.op);
  if (isFrom(p)) {
    // from space
    if (isLSBset(p->_infoPtr)) {
      // from space && forwarding
      LOG(LOG_SPEW, "update forward %s\n", p->ident);
      f.op = (Obj *)getInfoPtr(p);
      assert(isTo(f.op));
      setHeapArgType(&f);
      return f;
    } else {
      // from space && !forwarding
      int size = getObjSize(p);
      LOG(LOG_SPEW, "copy %s %s from->to size=%d\n",
                    objTypeNames[getObjType(p)], p->ident, size);
      if (USE_PERFCOUNTERS && rtArg.perfCounters) perfCounter.heapBytesCopied += size;
      memcpy(freePtr, p, size);

	    assert(isLSBset((InfoTab *)freePtr) == 0 && "gc: bad alignment");

      p->_infoPtr = setLSB((InfoTab *)freePtr);
      f.op = (Obj *)freePtr;
      setHeapArgType(&f);
      freePtr = (char *)freePtr + size;
      return f;
    }
  } else if (isTo(p)) {
    // to space
    assert(!isLSBset(p->_infoPtr));
    f.op = p;
    setHeapArgType(&f);
    return f;
  } else if (isSHO(p)) {
    // SHO
    assert(!isLSBset(p->_infoPtr));
    f.op = p;
    setHeapArgType(&f);
    return f;
  } else {
    assert(false && "bad ptr");
    LOG(LOG_FATAL, "bad ptr");  // if asserts are off
    exit(1);
    return (PtrOrLiteral){.op = NULL};  // avoid dumb compiler warning/error
  }
}


__attribute__((always_inline)) inline void updatePtr(PtrOrLiteral *f) {
  *f = updatePtrByValue(*f);
}


void processObj(Obj *p) {
  if (p == NULL) return;
  size_t i;
  LOG(LOG_SPEW, "processObj %s %s\n", objTypeNames[getObjType(p)], p->ident);


  switch (getObjType(p)) {
  case FUN: {
    int start = startFUNFVsB(p);
    int end = endFUNFVsB(p);
    // process boxed freevars
    for (i = start; i < end; i++) {
      updatePtr(&p->payload[i]);
    }
    break;
  }

  case PAP: {
    if (endPAPFVsU(p)) {
      // boxed free vars
      int start = startPAPFVsB(p);
      int end = endPAPFVsB(p);
      LOG(LOG_SPEW, "  %d free variables\n", end - start);
      for (i = start; i < end; i++) {
        updatePtr(&p->payload[i]);
      }
    } else { LOG(LOG_SPEW, "  no free variables\n");}
    Bitmap64 bm = p->payload[endPAPFVsU(p)].b;
    uint64_t mask = bm.bitmap.mask;
    int i = endPAPFVsU(p) + 1;
    for (int size = bm.bitmap.size; size != 0; size--, i++, mask >>= 1) {
      if (mask & 0x1UL) {
      	LOG(LOG_SPEW, "  call updatePtr boxed payload[%d]\n", i);
      	updatePtr(&p->payload[i]);
      } else {
        LOG(LOG_SPEW, "  don't call updatePtr unboxed\n");
      }
    }
    break;
  }

  case CON: {
    // boxed args
    int start = startCONargsB(p);
    int end = endCONargsB(p);
    for (i = start; i < end; i++) {
      updatePtr(&p->payload[i]);
    }
    break;
  }

  case THUNK:
  case BLACKHOLE: {
    int start = startTHUNKFVsB(p);
    int end = endTHUNKFVsB(p);
    for (i = start; i < end; i++) {
      updatePtr(&p->payload[i]);
    }
    break;
  }

  case INDIRECT:
    updatePtr(&p->payload[0]);
    break;

  default:
    LOG(LOG_ERROR, "gc: bad obj. type %d %s", getObjType(p),
        objTypeNames[getObjType(p)]);
    assert(false);
  }
}

void processCont(Cont *p) {
  int contType = getContType(p);
  LOG(LOG_SPEW, "processCont %s %s\n", contTypeNames[contType], p->ident);
  Bitmap64 bm = p->layout;
  uint64_t mask = bm.bitmap.mask;
  int size = bm.bitmap.size;
  if (contType != LETCONT) {
    for (int i = 0; i != size; i++, mask >>= 1) {
      if (mask & 0x1UL) updatePtr(&p->payload[i]);
    }  // for
  } else { // LETCONT
    for (int i = 0; i != size; i++) {
      if (p->payload[i].op != NULL) {
	      updatePtr(&p->payload[i]);
      }
    }
  }
}

void gc(void) {

  size_t before = stgHP - stgHeap;

  if (USE_PERFCOUNTERS && rtArg.perfCounters > 1) {
    if (before > perfCounter.heapMaxSize) perfCounter.heapMaxSize = before;
  }
  if(LOG_LEVEL == LOG_SPEW) {
    if(rtArg.loggingLevel == LOG_SPEW) {
      LOG(LOG_SPEW, "start gc heap size %lx\n", before);
    }
  }

  if (rtArg.sanityChecker) {
    LOG(LOG_INFO, "before GC\n");
    heapCheck(true, LOG_INFO);
    stackCheck(true, LOG_INFO);
  }

  // check if GC should run at all
  if(stgHP-stgHeap <= rtArg.gcThreshold * stgHeapSize) return;

  clock_t start_t = 0;
  (void)start_t;
  if (USE_PERFCOUNTERS && rtArg.perfCounters) perfCounter.heapCollections++;
  if (USE_PERFCOUNTERS && rtArg.perfCounters > 2)  start_t = clock();

  // add stgCurVal
  if (stgCurVal.op != NULL)
    processObj(stgCurVal.op);

  // only THUNK SHOs are potential roots
  for (int i = 0; i < stgStatObjCount; i++) {
    ObjType ot = getObjType(stgStatObj[i]);
    if (// getObjType(stgStatObj[i]) == THUNK ||
	ot == INDIRECT ||
	ot == BLACKHOLE) processObj(stgStatObj[i]);
  }

  //Cont. stack
  for (Cont *p = (Cont *)stgSP;
       (char *)p < (char*) stgStack + stgStackSize;
       p = (Cont *)((char*)p + getContSize(p))) {
    processCont(p);
  }

  //all roots are now added.

  //update stgCurVal
  if (stgCurVal.op != NULL)
    stgCurVal = updatePtrByValue(stgCurVal);

  // process "to" space
  while (scanPtr < freePtr) {
    processObj(scanPtr);
    scanPtr = (char *) scanPtr + getObjSize(scanPtr);
  }

  swapPtrs();

  if (rtArg.sanityChecker) {
    LOG(LOG_INFO, "after GC\n");
    heapCheck(true, LOG_INFO);
    stackCheck(true, LOG_INFO);
  }

  if(LOG_LEVEL == LOG_SPEW) {
    if(rtArg.loggingLevel == LOG_SPEW) {
      size_t after = stgHP - stgHeap;
      LOG(LOG_SPEW, "end gc heap size %lx (change %lx)\n", after, before - after);
    }
  }

  if (USE_PERFCOUNTERS && rtArg.perfCounters > 2) {
    perfCounter.gcTime += (double)(clock() - start_t) / CLOCKS_PER_SEC;
  }
}
