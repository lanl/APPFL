#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"
#include "obj.h"
#include "options.h"


#define EXTRASTART() PRINTF( "EXTRA check file %s line %d\n", __FILE__, __LINE__)
#define EXTRAEND() PRINTF( "EXTRA check succeeded %s %d\n", __FILE__, __LINE__)

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
  assert(isBoxed(f) && "not a HEAPOBJ");
  Obj *p = deref2(f.op);  
  if (isFrom(p)) {
    // from space
    if (isLSBset(p->_infoPtr)) {
      // from space && forwarding
      if (DEBUG_GC) PRINTF( "update forward %s\n", p->ident);
      f.op = (Obj *)getInfoPtr(p);
      assert(isTo(f.op));
#if USE_ARGTYPE
      f.argType = HEAPOBJ;
#endif
      return f;
    } else {
      // from space && !forwarding
      int size = getObjSize(p);
      if (DEBUG_GC) {
        PRINTF( "copy %s %s from->to size=%d\n", 
		objTypeNames[getObjType(p)], p->ident, size);
      }
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
    PRINTF( "bad ptr");  // if asserts are off
    exit(1);
    return (PtrOrLiteral){.op = NULL};  // avoid dumb compiler warning/error
  }
}

void updatePtr(PtrOrLiteral *f) {
  *f = updatePtrByValue(*f);
}

void processObj(Obj *p) {
  size_t i;
  if (DEBUG_GC) PRINTF( "processObj %s %s\n", objTypeNames[getObjType(p)], p->ident);

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
      for (i = start; i < end; i++) {
        updatePtr(&p->payload[i]);
      }
    }
    Bitmap64 bm = p->payload[endPAPFVsU(p)].b;
    uint64_t mask = bm.bitmap.mask;
    int i = endPAPFVsU(p) + 1;
    for (int size = bm.bitmap.size; size != 0; size--, i++, mask >>= 1) {
      if (mask & 0x1UL) updatePtr(&p->payload[i]);
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
    PRINTF( "gc: bad obj. type %d %s", getObjType(p),
        objTypeNames[getObjType(p)]);
    assert(false);
  }
}

void processCont(Cont *p) {
  int contType = getContType(p);
  assert(contType > PHONYSTARTCONT && 
	 contType < PHONYENDCONT && "bad cont type");
  if (DEBUG_GC) PRINTF("processCont %s %s\n", 
		       contTypeNames[contType], p->ident);
  Bitmap64 bm = p->layout;
  uint64_t mask = bm.bitmap.mask;
  int size = bm.bitmap.size;
  if (contType != LETCONT) {
    for (int i = 0; i != size; i++, mask >>= 1) {
      if (EXTRA_CHECKS_GC) {
    	EXTRASTART();
    	if (mask & 0x1UL) {
    	  if (!isBoxed(p->payload[i])) {
    	    PRINTF("gc: unexpected unboxed arg in CONT index %d\n", i);
    	    assert(false);
    	  }
    	} else {
    	  if (isBoxed(p->payload[i])) {
    	    PRINTF("gc: unexpected boxed arg in CONT index %d\n", i);
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
	  if (!isBoxed(p->payload[i])) {
	    PRINTF("gc: unexpected unboxed arg in LETCONT index %d\n", i);
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

  // PRINTF( "GARBAGE COLLECTION DISABLED in gc.c/gc(void)\n"); return;

  size_t before = stgHP - stgHeap;

  if (EXTRA_CHECKS_GC) {
    EXTRASTART();
    // checkStgHeap(); -- heap is fragmented
    EXTRAEND();
  }

  if (DEBUG_GC) {
    showStgHeap();
    PRINTF( "start gc heap size %lx\n", before);
  }

  // add stgCurVal
  if (EXTRA_CHECKS_GC) {
    assert(isBoxed(stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
  }
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
    assert(isBoxed(stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
    EXTRASTART();
  }
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
  if (DEBUG_GC) {
    PRINTF( "new heap\n");
    showStgHeap();
    size_t after = stgHP - stgHeap;
    PRINTF( "end gc heap size %lx (change %lx)\n", after, before - after);
    // suppress unused var warnings
    (void)after;
    (void)before;
  }
}

