#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"
#include "obj.h"
#include "options.h"
#include "sanity.h"

static void *scanPtr, *freePtr;

void *getToPtr() {return toPtr;}

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

Obj *deref1(Obj *op) {
  while (getObjType(op) == INDIRECT)
    op = op->payload[0].op;
  return op;
}

Obj *deref2(Obj *p) {
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
  
  Obj *p = deref2(f.op);
  if (isFrom(p)) {
    // from space
    if (isLSBset(p->_infoPtr)) {
      // from space && forwarding
      LOG(LOG_SPEW, "update forward %s\n", p->ident);
      f.op = (Obj *)getInfoPtr(p);
      assert(isTo(f.op));
#if USE_ARGTYPE
      f.argType = HEAPOBJ;
#endif
      return f;
    } else {
      // from space && !forwarding
      int size = getObjSize(p);
      LOG(LOG_SPEW, "copy %s %s from->to size=%d\n",
                    objTypeNames[getObjType(p)], p->ident, size);
      memcpy(freePtr, p, size);
      if (EXTRA_CHECKS_GC) {
	      assert(isLSBset((InfoTab *)freePtr) == 0 && "gc: bad alignment");
      }
      p->_infoPtr = setLSB((InfoTab *)freePtr);
      f.op = (Obj *)freePtr;
#if USE_ARGTYPE
      f.argType = HEAPOBJ;
#endif
      freePtr = (char *)freePtr + size;
      return f;
    }
  } else if (isTo(p)) {
    // to space
    assert(!isLSBset(p->_infoPtr));
    f.op = p;
#if USE_ARGTYPE
    f.argType = HEAPOBJ;
#endif
    return f;
  } else if (isSHO(p)) {
    // SHO
    assert(!isLSBset(p->_infoPtr));
    f.op = p;
#if USE_ARGTYPE
    f.argType = HEAPOBJ;
#endif
    return f;
  } else {
    assert(false && "bad ptr");
    LOG(LOG_FATAL, "bad ptr");  // if asserts are off
    exit(1);
    return (PtrOrLiteral){.op = NULL};  // avoid dumb compiler warning/error
  }
}

void updatePtr(PtrOrLiteral *f) {
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
  assert(contType > PHONYSTARTCONT &&
	 contType < PHONYENDCONT && "bad cont type");
  LOG(LOG_SPEW, "processCont %s %s\n", contTypeNames[contType], p->ident);
  Bitmap64 bm = p->layout;
  uint64_t mask = bm.bitmap.mask;
  int size = bm.bitmap.size;
  if (contType != LETCONT) {
    for (int i = 0; i != size; i++, mask >>= 1) {
      if (EXTRA_CHECKS_GC) {
    	  EXTRASTART();
    	  if (mask & 0x1UL) {
    	    if (!mayBeBoxed(p->payload[i])) {
            LOG(LOG_ERROR, "gc: unexpected unboxed arg in CONT index %d\n", i);
    	      assert(false);
    	    }
    	  } else {
          if (!mayBeUnboxed(p->payload[i])) {
            LOG(LOG_ERROR, "gc: unexpected boxed arg in CONT index %d\n", i);
    	      assert(false);
    	    }
    	  }
    	  EXTRAEND();
      }
      if (mask & 0x1UL) updatePtr(&p->payload[i]);
    }  // for
  } else { // LETCONT
    for (int i = 0; i != size; i++) {
      if (p->payload[i].op != NULL) {
	      if (EXTRA_CHECKS_GC) {
	        EXTRASTART();
	        if (!mayBeBoxed(p->payload[i])) {
	           LOG(LOG_ERROR, "gc: unexpected unboxed arg in LETCONT index %d\n", i);
	            assert(false);
	        }
	        EXTRAEND();
	      }
	      updatePtr(&p->payload[i]);
      }
    }
  }
}

void gc(void) {
  //LOG(LOG_INFO, "GARBAGE COLLECTION DISABLED in gc.c/gc(void)\n"); return;

  //heapCheck(); // disable untill it passes tests

  size_t before = stgHP - stgHeap;

  if (EXTRA_CHECKS_GC) {
    EXTRASTART();
    // checkStgHeap(); -- heap is fragmented
    EXTRAEND();
  }

  if(LOG_LEVEL == LOG_SPEW) {
    if(LoggingLevel == LOG_SPEW) {
      showStgHeap(LOG_SPEW);
      LOG(LOG_SPEW, "start gc heap size %lx\n", before);
    }
  }

  if(stgHP-stgHeap <= GCThreshold*stgHeapSize) return;

  // add stgCurVal
  if (EXTRA_CHECKS_GC) {
    assert(mayBeBoxed(stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
  }
  if (stgCurVal.op != NULL)
    processObj(stgCurVal.op);

  // all SHO's
  for (int i = 0; i < stgStatObjCount; i++) {
    processObj(stgStatObj[i]);
  }
  //Cont. stack
  for (Cont *p = (Cont *)stgSP;
       (char *)p < (char*) stgStack + stgStackSize;
       p = (Cont *)((char*)p + getContSize(p))) {
    processCont(p);
  }

  //all roots are now added.

  //update stgCurVal
  if (EXTRA_CHECKS_GC) {
    EXTRASTART();
    assert(mayBeBoxed(stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
    EXTRASTART();
  }
  if (stgCurVal.op != NULL)
    stgCurVal = updatePtrByValue(stgCurVal);

  // process "to" space
  while (scanPtr < freePtr) {
    processObj(scanPtr);
    scanPtr = (char *) scanPtr + getObjSize(scanPtr);
  }

  swapPtrs();

  if (EXTRA_CHECKS_GC) {
    EXTRASTART();
    checkStgHeap();
    EXTRAEND();
  }
  
  if(LOG_LEVEL == LOG_SPEW) {
    if(LoggingLevel == LOG_SPEW) {
      LOG(LOG_SPEW, "new heap\n");
      showStgHeap(LOG_SPEW);
      size_t after = stgHP - stgHeap;
      LOG(LOG_SPEW, "end gc heap size %lx (change %lx)\n", after, before - after);
    }
  }
}
