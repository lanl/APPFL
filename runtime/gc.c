#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"

const bool DEBUG = false;
const bool EXTRA = true;  // run extra checks

void *toPtr=NULL, *fromPtr=NULL;
void *scanPtr=NULL, *freePtr=NULL;

// wrapper functions for possible interface changes
// see README-heapobj.txt
static inline size_t startFUNFVsB(Obj *p) { return 0; }
static inline size_t endFUNFVsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startFUNFVsU(Obj *p) { return endFUNFVsB(p); }
static inline size_t endFUNFVsU(Obj *p) { return startFUNFVsU(p) + p->infoPtr->layoutInfo.unboxedCount; }
static inline void checkFUNFVs(Obj *p) { assert (p->infoPtr->fvCount == endFUNFVsU(p) && "gc: FUN mismatch"); }

static inline size_t startPAPFVsB(Obj *p) { return 0; }
static inline size_t endPAPFVsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startPAPFVsU(Obj *p) { return endPAPFVsB(p); }
static inline size_t endPAPFVsU(Obj *p)   { return startPAPFVsU(p) +  p->infoPtr->layoutInfo.unboxedCount; }
static inline size_t startPAPargsB(Obj *p) { return endPAPFVsU(p) + 1; }
static inline size_t endPAPargsB(Obj *p) { return startPAPargsB(p) + PUNPACK(p->payload[endPAPFVsU(p)].i); }
static inline size_t startPAPargsU(Obj *p) { return endPAPargsB(p); }
static inline size_t endPAPargsU(Obj *p) { return startPAPargsU(p) + NUNPACK(p->payload[endPAPFVsU(p)].i); }
static inline void checkPAPFVs(Obj *p) {
  // fvCount is going away at some point
  assert (p->infoPtr->fvCount == endPAPFVsU(p) && "gc: PAP FV mismatch");
}
static inline void checkPAPargs(Obj *p) {
  // argCount is going away at some point
  assert (p->argCount == endPAPargsU(p) - startPAPargsB(p) && "gc: PAP arg mismatch");
}

static inline size_t startCONargsB(Obj *p) { return 0; }
static inline size_t endCONargsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startCONargsU(Obj *p) { return endCONargsB(p); }
static inline size_t endCONargsU(Obj *p) { return startCONargsU(p) + p->infoPtr->layoutInfo.unboxedCount; }
static inline void checkCONargs(Obj *p) {
  assert (p->infoPtr->conFields.arity == endCONargsU(p) && "gc: CON args mismatch");
}

// payload[0] of thunk is result field
static inline size_t startTHUNKFVsB(Obj *p) { return 1; }
static inline size_t endTHUNKFVsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount + 1; }
static inline size_t startTHUNKFVsU(Obj *p) { return endCONargsB(p); }
static inline size_t endTHUNKFVsU(Obj *p) { return startCONargsU(p) + p->infoPtr->layoutInfo.unboxedCount; }
static inline void checkTHUNKFVs(Obj *p) {
  // fvCount is going away at some point
  assert (p->infoPtr->fvCount == endTHUNKFVsU(p) && "gc: THUNK FV mismatch");
}

static inline size_t startCALLFVsB(Obj *p) { return 1; }
static inline size_t endCALLFVsB(Obj *p) { return p->payload[0].i + 1; }

static inline size_t startCASEFVsB(Obj *p) { return 0; }
static inline size_t endCASEFVsB(Obj *p) { return  p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startCASEFVsU(Obj *p) { return endCASEFVsB(p); }
static inline size_t endCASEFVsU(Obj *p) { return startCASEFVsU(p) + p->infoPtr->layoutInfo.unboxedCount; }
static inline void checkCASEFVs(Obj *p) {
  // fvCount is going away at some point
  assert (p->infoPtr->fvCount == endCASEFVsU(p) && "gc: CASE FV mismatch");
}

static inline bool isFrom(void *p) {
  return (p >= fromPtr && (char *)p < (char *)fromPtr + stgHeapSize/2);
}

static inline bool isTo(void *p) {
  return (p >= toPtr && (char *)p < (char *)toPtr + stgHeapSize/2);
}

static inline bool isBoxed(PtrOrLiteral *f) { return (f->argType == HEAPOBJ); }

// use LSB to say it is a FORWARD
static inline uintptr_t setLSB(void *ptr) { return (uintptr_t)ptr | 1; }
static inline uintptr_t unsetLSB(void *ptr) { return (uintptr_t)ptr & ~1; }
static inline uintptr_t isLSBset(void *ptr) { return (uintptr_t)ptr & 1; }

// end of wrappers

void initGc(void) {
  assert(stgHeap && "gc: heap not defined");
  fromPtr = stgHeap;
  toPtr = (char *)stgHeap + stgHeapSize/2;
  scanPtr = toPtr;
  freePtr = toPtr;
}

void swapPtrs(void) { 
  assert( scanPtr == freePtr && "gc: not finished");
  size_t before = stgHP-stgHeap;
  stgHeap = toPtr;
  stgHP = freePtr;
  toPtr = fromPtr;
  fromPtr = stgHeap;
  freePtr = toPtr;
  scanPtr = toPtr;
  assert (stgHP-stgHeap <= before && "gc: increased heap size!\n");
}


void updatePtr(PtrOrLiteral *f) {
  Obj *p = derefPoL(*f);

  if (isFrom(p)) {
    if (isLSBset(p->infoPtr)) {
      if (DEBUG) fprintf(stderr,"update forward %s\n",p->ident);
      f->op = (Obj *)unsetLSB(p->infoPtr);
    } else {
      int size = getObjSize(p);
      if (DEBUG) {
        fprintf(stderr, "copy %s %s from->to size=%d\n", objTypeNames[p->objType], p->ident, size);
      }

      memcpy(freePtr, p, size);

      p->infoPtr = (InfoTab *)setLSB(freePtr);

      f->op = (Obj *)freePtr;

      freePtr = (char *)freePtr + size;
    }
  } else if (isTo(p)) {
    // do nothing
  } else { // SHO
    if(f->op->objType == INDIRECT) {
      if(isFrom(f->op)) {
        if (DEBUG) fprintf(stderr,"fix INDIRECT to sho %s\n",f->op->ident);
        f->op = p;
      }
    }
  }
}


void processObj(Obj *p) {
  size_t i;
  if (DEBUG) fprintf(stderr,"processObj %s %s\n",objTypeNames[p->objType], p->ident);
  switch(p->objType) {
  case FUN: {
    int FVCount = endFUNFVsU(p);
    if(FVCount) {
      if (EXTRA) checkFUNFVs(p);
      // process boxed freevars
      for (i = startFUNFVsB(p); i < endFUNFVsB(p); i++) {
        if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed FV in FUN");
        updatePtr(&p->payload[i]);
      }
      // double check that unboxed FVs really are unboxed
      if (EXTRA) {
        for (i = startFUNFVsU(p); i < FVCount; i++) {
          assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed FV in FUN");
        }
      }
    }
    break;
  }
  case PAP: {
    int  FVCount = endPAPFVsU(p);
    if(FVCount) {
      if (EXTRA) checkPAPFVs(p);
      // boxed free vars
      int startB = startPAPFVsB(p);
      int endB = endPAPFVsB(p);
      for (i = startB; i < endB; i++) {
        if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed FV in PAP");
        updatePtr(&p->payload[i]);
      }

      // double check that unboxed FVs really are unboxed
      if (EXTRA) {
        int startU = startPAPFVsU(p);
        for (i = startU; i < FVCount; i++) {
          assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed FV in PAP");
        }
      }
    }

    if (EXTRA) checkPAPargs(p);
    // boxed args already applied
    for (i = startPAPargsB(p); i < endPAPargsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in PAP");
      updatePtr(&p->payload[i]);
    }
    // double check that unboxed args really are unboxed
    if (EXTRA) {
      for (i = startPAPargsU(p); i < endPAPargsU(p); i++) {
        assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed arg in PAP");
      }
    }
    break;
  }
  case CON:
    if (EXTRA) checkCONargs(p);

    // boxed args
    for(i = startCONargsB(p); i < endCONargsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in CON");
      updatePtr(&p->payload[i]);
    }
    // double check that unboxed args really are unboxed
    if (EXTRA) {
      for (i = startCONargsU(p); i < endCONargsU(p); i++) {
        assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed arg in CON");
      }
    }
    break;
  case THUNK:
  case BLACKHOLE:
    if (EXTRA) checkTHUNKFVs(p);

    for(i = startTHUNKFVsB(p); i < endTHUNKFVsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in THUNK");
      updatePtr(&p->payload[i]);
    }
    // double check that unboxed args really are unboxed
    if (EXTRA) {
      for (i = startTHUNKFVsU(p); i < endTHUNKFVsU(p); i++) {
        assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed arg in THUNK");
      }
    }
    break;
  case INDIRECT:
    if (EXTRA) assert(isBoxed(&p->payload[0]) && "gc: unexpected unboxed arg in INDIRECT");
    updatePtr(&p->payload[0]);
    break;
  default:
    fprintf(stderr, "gc: bad obj. type %d %s", p->objType, objTypeNames[p->objType]);
    assert (false);
  }
}

void processCont(Obj *p) {
  size_t i;
  if (DEBUG) fprintf(stderr,"processCont %s %s\n",objTypeNames[p->objType], p->ident);
  switch(p->objType) {
  case UPDCONT:
    updatePtr(&p->payload[0]);
    break;
  case CASECONT:
    if (EXTRA) checkCASEFVs(p);

    for(i = startCASEFVsB(p); i < endCASEFVsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in CASE");
      updatePtr(&p->payload[i]);
    }
    if (EXTRA) {
      for (i = startCASEFVsU(p); i < endCASEFVsU(p); i++) {
        assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed arg in CASE");
      }
    }
    break;
  case CALLCONT:
    for(i = startCALLFVsB(p); i < endCALLFVsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in CALL");
      updatePtr(&p->payload[i]);
    }
    break;
  case FUNCONT:
    if (EXTRA) assert(isBoxed(&p->payload[0]) && "gc: unexpected unboxed arg in FUN");
    updatePtr(&p->payload[0]);
    break;
  default:
    fprintf(stderr, "gc: bad cont. type %d %s\n",p->objType, objTypeNames[p->objType]);
    assert (false);
  }
}

void gc(void) {

  size_t before = stgHP-stgHeap;

  if (DEBUG) {
    fprintf(stderr, "old heap\n");
    showStgHeap();
    fprintf(stderr,"start gc heap size %lx\n", before);
  }

  // add stgCurVal
  if (stgCurVal.argType == HEAPOBJ) {
    processObj(stgCurVal.op);
  }

  // all SHO's
  for(int i=0; i<stgStatObjCount; i++) {
    processObj(stgStatObj[i]);
  }

  //Cont. stack
  for (char *p = (char*)stgSP;
      p < (char*)stgStack + stgStackSize;
      p += getObjSize((Obj *)p)) {
    processCont((Obj *)p);
  }

  //all roots are now added.

  //update stgCurVal
  if (EXTRA) assert(isBoxed(&stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
  updatePtr(&stgCurVal);


  // process "to" space
  while(scanPtr < freePtr) {
    processObj(scanPtr);
    scanPtr = (char *)scanPtr + getObjSize(scanPtr);
  }

  swapPtrs();

  if (DEBUG) {
    fprintf(stderr, "new heap\n");
    showStgHeap();
    size_t after = stgHP-stgHeap;
    fprintf(stderr,"end gc heap size %lx (change %lx)\n",
        after, before-after );
  }
}
