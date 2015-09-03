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

static inline size_t startFUNFvsB(Obj *p) { return 0; }
static inline size_t endFUNFvsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startFUNFvsU(Obj *p) { return endFUNFvsB(p); }
static inline size_t endFUNFvsU(Obj *p) { return startFUNFvsU(p) + p->infoPtr->layoutInfo.unboxedCount; }
static inline void checkFUNFvs(Obj *p) { assert (p->infoPtr->fvCount == endFUNFvsU(p) && "FUN mismatch"); }

static inline size_t startPAPFVsB(Obj *p) { return 0; }
static inline size_t endPAPFVsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startPAPFVsU(Obj *p) { return endPAPFVsB(p); }
static inline size_t endPAPFVsU(Obj *p)   { return startPAPFVsU(p) +  p->infoPtr->layoutInfo.unboxedCount; }
static inline size_t startPAPargsB(Obj *p) { return endPAPFVsU(p) + 1; }
static inline size_t endPAPargsB(Obj *p) { return  startPAPargsB(p) +  PUNPACK(p->payload[endPAPFVsU(p)].i); }
static inline size_t startPAPargsU(Obj *p) { return  endPAPargsB(p);}
static inline size_t endPAPargsU(Obj *p) {  return startPAPargsU(p) + NUNPACK(p->payload[endPAPFVsU(p)].i); }
static inline void checkPAPFVs(Obj *p) {
  assert (p->infoPtr->fvCount == endPAPFVsU(p) && "PAP FV mismatch");
}
static inline void checkPAPargs(Obj *p) {
  assert (p->argCount == endPAPargsU(p) - startPAPargsB(p) && "PAP arg mismatch");
}

static inline size_t countCONargs(Obj *p) { return p->infoPtr->conFields.arity; }
static inline size_t startCONargsB(Obj *p) { return 0; }
static inline size_t endCONargsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startCONargsU(Obj *p) { return endCONargsB(p); }
static inline size_t endCONargsU(Obj *p) { return startCONargsU(p) + p->infoPtr->layoutInfo.unboxedCount; }

// payload[0] of thunk is result field
static inline size_t startTHUNKargsB(Obj *p) { return 1; }
static inline size_t endTHUNKargsB(Obj *p) { return p->infoPtr->layoutInfo.boxedCount + 1; }
static inline size_t startTHUNKargsU(Obj *p) { return endCONargsB(p); }
static inline size_t endTHUNKargsU(Obj *p) { return startCONargsU(p) + p->infoPtr->layoutInfo.unboxedCount; }

static inline size_t startCALLargsB(Obj *p) { return 1; }
static inline size_t endCALLargsB(Obj *p) { return p->payload[0].i + 1; }

static inline size_t startCASEargsB(Obj *p) { return 0; }
static inline size_t endCASEargsB(Obj *p) { return  p->infoPtr->layoutInfo.boxedCount; }
static inline size_t startCASEargsU(Obj *p) { return endCASEargsB(p); }
static inline size_t endCASEargsU(Obj *p) { return startCASEargsU(p) + p->infoPtr->layoutInfo.unboxedCount; }

static inline bool isFrom(void *p) {
  return (p >= fromPtr && (char *)p < (char *)fromPtr + stgHeapSize/2);
}

static inline bool isTo(void *p) {
  return (p >= toPtr && (char *)p < (char *)toPtr + stgHeapSize/2);
}

static inline bool isBoxed(PtrOrLiteral *f) { return (f->argType == HEAPOBJ); }

// end of wrappers

void initGc(void) {
  assert(stgHeap && "heap not defined"); 
  fromPtr = stgHeap;
  toPtr = (char *)stgHeap + stgHeapSize/2;
  scanPtr = toPtr;
  freePtr = toPtr;
}

void swapPtrs(void) { 
  assert( scanPtr == freePtr && "gc not finished");
  size_t before = stgHP-stgHeap;
  stgHeap = toPtr;
  stgHP = freePtr;
  toPtr = fromPtr;
  fromPtr = stgHeap;
  freePtr = toPtr;
  scanPtr = toPtr;
  assert (stgHP-stgHeap <= before && "gc increased heap size!\n"); 
}


void updatePtr(PtrOrLiteral *f) {
  Obj *p = derefPoL(*f);

  if (isFrom(p)) {
    if(p->objType == FORWARD) {
      if (DEBUG) fprintf(stderr,"update forward %s\n",p->ident);
      f->op = p->payload[0].op;
    } else {
      int size = getObjSize(p);
      if (DEBUG) {
        fprintf(stderr, "copy %s %s from->to size=%d\n", objTypeNames[p->objType], p->ident, size);
      }

      memcpy(freePtr, p, size);

      assert(size > sizeof(Obj) && "no space for FORWARD");

      p->objType = FORWARD;
      p->payload[0].op = freePtr;

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
    int FVCount = endFUNFvsU(p);
    if(FVCount) {
      if (EXTRA) checkFUNFvs(p);
      // process boxed freevars
      for (i = startFUNFvsB(p); i < endFUNFvsB(p); i++) {
        if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed FV in FUN");
        updatePtr(&p->payload[i]);
      }
      // double check that unboxed FVs really are unboxed
      if (EXTRA) {
        for (i = startFUNFvsU(p); i < FVCount; i++) {
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
    if (EXTRA) assert (endCONargsU(p) == p->infoPtr->conFields.arity && "gc: arity mismatch in CON");

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
    for(i = startTHUNKargsB(p); i < endTHUNKargsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in THUNK");
      updatePtr(&p->payload[i]);
    }
    // double check that unboxed args really are unboxed
    if (EXTRA) {
      for (i = startTHUNKargsU(p); i < endTHUNKargsU(p); i++) {
        assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed arg in THUNK");
      }
    }
    break;
  case INDIRECT:
    if (EXTRA) assert(isBoxed(&p->payload[0]) && "gc: unexpected unboxed arg in INDIRECT");
    updatePtr(&p->payload[0]);
    break;
  default:
    fprintf(stderr, "bad obj. type %d %s", p->objType, objTypeNames[p->objType]);
    assert (false && "bad obj type");
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
    for(i = startCASEargsB(p); i < endCASEargsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in CASE");
      updatePtr(&p->payload[i]);
    }
    if (EXTRA) {
      for (i = startCASEargsU(p); i < endCASEargsU(p); i++) {
        assert(!isBoxed(&p->payload[i]) && "gc: unexpected boxed arg in CASE");
      }
    }
    break;
  case CALLCONT:
    for(i = startCALLargsB(p); i < endCALLargsB(p); i++) {
      if (EXTRA) assert(isBoxed(&p->payload[i]) && "gc: unexpected unboxed arg in CALL");
      updatePtr(&p->payload[i]);
    }
    break;
  case FUNCONT:
    if (EXTRA) assert(isBoxed(&p->payload[0]) && "gc: unexpected unboxed arg in FUN");
    updatePtr(&p->payload[0]);
    break;
  default:
    fprintf(stderr, "bad cont. type %d %s\n",p->objType, objTypeNames[p->objType]);
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
  if (stgCurVal.argType == HEAPOBJ) {
    if (EXTRA) assert(isBoxed(&stgCurVal) && "gc: unexpected unboxed arg in stgCurVal");
    updatePtr(&stgCurVal);
  }

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
