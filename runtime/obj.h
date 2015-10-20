#ifndef obj_h
#define obj_h

#include "options.h"
#include "stg.h"

// wrapper functions for possible interface changes
// see README-heapobj.txt
static inline size_t startFUNFVsB(Obj *p) { return 0; }
static inline size_t endFUNFVsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount; }
static inline size_t startFUNFVsU(Obj *p) { return endFUNFVsB(p); }
static inline size_t endFUNFVsU(Obj *p) { return startFUNFVsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }

static inline size_t startPAPFVsB(Obj *p) { return 0; }
static inline size_t endPAPFVsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount; }
static inline size_t startPAPFVsU(Obj *p) { return endPAPFVsB(p); }
static inline size_t endPAPFVsU(Obj *p) { return startPAPFVsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }
static inline size_t startPAPargsB(Obj *p) { return endPAPFVsU(p) + 1; }
static inline size_t endPAPargsB(Obj *p) { return startPAPargsB(p) + PUNPACK(p->payload[endPAPFVsU(p)].i); }
static inline size_t startPAPargsU(Obj *p) { return endPAPargsB(p); }
static inline size_t endPAPargsU(Obj *p) { return startPAPargsU(p) + NUNPACK(p->payload[endPAPFVsU(p)].i); }

static inline size_t startCONargsB(Obj *p) { return 0; }
static inline size_t endCONargsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount; }
static inline size_t startCONargsU(Obj *p) { return endCONargsB(p); }
static inline size_t endCONargsU(Obj *p) { return startCONargsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }
static inline void checkCONargs(Obj *p) {
  assert(
      !((uintptr_t) p->infoPtr & (uintptr_t) 1)
          && "...odd infoPtr checkCONargs");
  assert(
      p->infoPtr->conFields.arity >= 0 && getInfoPtr(p)->conFields.arity <= 10
          && "gc:  unlikely number of constructor arguments");
  assert(
      getInfoPtr(p)->conFields.arity == endCONargsU(p) && "gc: CON args mismatch");
}

// payload[0] of thunk is result field
static inline size_t startTHUNKFVsB(Obj *p) { return 1; }
static inline size_t endTHUNKFVsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount + 1; }
static inline size_t startTHUNKFVsU(Obj *p) { return endCONargsB(p); }
static inline size_t endTHUNKFVsU(Obj *p) { return startCONargsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }

static inline size_t startCALLFVsB(Obj *p) { return 1; }
static inline size_t endCALLFVsB(Obj *p) { return p->payload[0].i + 1; }

static inline size_t startCASEFVsB(Obj *p) { return 0; }
static inline size_t endCASEFVsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount; }
static inline size_t startCASEFVsU(Obj *p) { return endCASEFVsB(p); }
static inline size_t endCASEFVsU(Obj *p) { return startCASEFVsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }


static inline bool isBoxed(PtrOrLiteral f) {
#if USE_ARGTYPE
  return (f.argType == HEAPOBJ);
#else
  return true;
#endif
}

static inline bool isUnboxed(PtrOrLiteral f) {
#if USE_ARGTYPE
  return (f.argType != HEAPOBJ);
#else
  return true;
#endif
}

// use LSB to say it is a FORWARD
static inline uintptr_t setLSB(void *ptr) { return (uintptr_t) ptr | 1; }
static inline uintptr_t unsetLSB(void *ptr) { return (uintptr_t) ptr & ~1; }
static inline uintptr_t isLSBset(void *ptr) { return (uintptr_t) ptr & 1; }


// end of wrappers

#endif
