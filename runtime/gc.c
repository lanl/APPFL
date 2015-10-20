#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"
#include "obj.h"

const bool DEBUG = false;
const bool EXTRA = true;  // run extra checks

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

void updatePtr(PtrOrLiteral *f) {
  Obj *p = derefPoL(*f);

  if (isFrom(p)) {
    if (isLSBset(p->infoPtr)) {
      if (DEBUG) fprintf(stderr, "update forward %s\n", p->ident);
      f->op = (Obj *) maskInfoPtr(p->infoPtr);
    } else {
      int size = getObjSize(p);
      if (DEBUG) {
        fprintf(stderr, "copy %s %s from->to size=%d\n", objTypeNames[p->objType], p->ident, size);
      }

      memcpy(freePtr, p, size);
      if (EXTRA) assert(isLSBset(freePtr) == 0 && "gc: bad alignment");

      p->infoPtr = (InfoTab *) setLSB(freePtr);
      f->op = (Obj *) freePtr;
      freePtr = (char *) freePtr + size;
    }
  } else if (isTo(p)) {
    // do nothing
  } else { // SHO
    if (isFrom(f->op) && f->op->objType == INDIRECT) {
        if (DEBUG) fprintf(stderr, "fix INDIRECT to sho %s\n", f->op->ident);
        f->op = p;
    }
  }
}

void processObj(Obj *p) {
  size_t i;
  if (DEBUG) fprintf(stderr, "processObj %s %s\n", objTypeNames[p->objType], p->ident);

  switch (p->objType) {
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

    // boxed args already applied
    int start = startPAPargsB(p);
    int end = endPAPargsB(p);
    for (i = start; i < end; i++) {
      updatePtr(&p->payload[i]);
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
    fprintf(stderr, "gc: bad obj. type %d %s", p->objType,
        objTypeNames[p->objType]);
    assert(false);
  }
}

void processCont(Obj *p) {
  size_t i;
  if (DEBUG) fprintf(stderr, "processCont %s %s\n", objTypeNames[p->objType], p->ident);
  switch (p->objType) {
  case UPDCONT:
    updatePtr(&p->payload[0]);
    break;
  case CASECONT: {
    if (EXTRA) {
      for (i = startCASEFVsU(p); i < endCASEFVsU(p); i++) {
        assert(isUnboxed(p->payload[i]) && "gc: unexpected boxed arg in CASE");
      }
    }

    int start = startCASEFVsB(p);
    int end = endCASEFVsB(p);
    for (i = start; i < end; i++) {
      if (EXTRA) assert(isBoxed(p->payload[i]) && "gc: unexpected unboxed arg in CASE");
      updatePtr(&p->payload[i]);
    }
    break;
  }
  case CALLCONT: {
    int start = startCALLFVsB(p);
    int end = endCALLFVsB(p);
    for (i = start; i < end; i++) {
      if (EXTRA) assert(isBoxed(p->payload[i]) && "gc: unexpected unboxed arg in CALL");
      updatePtr(&p->payload[i]);
    }
    break;
  }
  case FUNCONT:
    if (EXTRA) assert(isBoxed(p->payload[0]) && "gc: unexpected unboxed arg in FUN");
    updatePtr(&p->payload[0]);
    break;
  default:
    fprintf(stderr, "gc: bad cont. type %d %s\n", p->objType,
        objTypeNames[p->objType]);
    assert(false);
  }
}

void gc(void) {

  size_t before = stgHP - stgHeap;

  if (EXTRA) checkStgHeap();

  if (DEBUG) {
    fprintf(stderr, "old heap\n");
    showStgHeap();
    fprintf(stderr, "start gc heap size %lx\n", before);
  }

  // add stgCurVal
  if (EXTRA ) assert(isBoxed(stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
  processObj(stgCurVal.op);

  // all SHO's
  for (int i = 0; i < stgStatObjCount; i++) {
    processObj(stgStatObj[i]);
  }

  //Cont. stack
  for (char *p = (char*) stgSP; p < (char*) stgStack + stgStackSize; p +=
      getObjSize((Obj *) p)) {
    processCont((Obj *) p);
  }

  //all roots are now added.

  //update stgCurVal
  if (EXTRA ) assert(isBoxed(stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
  updatePtr(&stgCurVal);

  // process "to" space
  while (scanPtr < freePtr) {
    processObj(scanPtr);
    scanPtr = (char *) scanPtr + getObjSize(scanPtr);
  }

  swapPtrs();

  if (EXTRA) checkStgHeap();
  if (DEBUG) {
    fprintf(stderr, "new heap\n");
    showStgHeap();
    size_t after = stgHP - stgHeap;
    fprintf(stderr, "end gc heap size %lx (change %lx)\n", after, before - after);
  }
}

