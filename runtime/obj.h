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
//static inline size_t startPAPargsB(Obj *p) { return endPAPFVsU(p) + 1; }
//static inline size_t endPAPargsB(Obj *p) { return startPAPargsB(p) + PUNPACK(p->payload[endPAPFVsU(p)].i); }
//static inline size_t startPAPargsU(Obj *p) { return endPAPargsB(p); }
//static inline size_t endPAPargsU(Obj *p) { return startPAPargsU(p) + NUNPACK(p->payload[endPAPFVsU(p)].i); }

static inline size_t startCONargsB(Obj *p) { return 0; }
static inline size_t endCONargsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount; }
static inline size_t startCONargsU(Obj *p) { return endCONargsB(p); }
static inline size_t endCONargsU(Obj *p) { return startCONargsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }
static inline void checkCONargs(Obj *p) {
  assert(
      !((uintptr_t) p->_infoPtr & (uintptr_t) 1)
          && "...odd infoPtr checkCONargs");
  assert(
      getInfoPtr(p)->conFields.arity >= 0 && getInfoPtr(p)->conFields.arity <= 10
          && "gc:  unlikely number of constructor arguments");
  assert(
      getInfoPtr(p)->conFields.arity == endCONargsU(p) && "gc: CON args mismatch");
}

// payload[0] of thunk is result field
static inline size_t startTHUNKFVsB(Obj *p) { return 1; }
static inline size_t endTHUNKFVsB(Obj *p) { return getInfoPtr(p)->layoutInfo.boxedCount + 1; }
static inline size_t startTHUNKFVsU(Obj *p) { return endTHUNKFVsB(p); }
static inline size_t endTHUNKFVsU(Obj *p) { return startTHUNKFVsU(p) + getInfoPtr(p)->layoutInfo.unboxedCount; }

// static inline size_t startCALLFVsB(Cont *p) { return 1; }
// static inline size_t endCALLFVsB(Cont *p) { return p->payload[0].i + 1; }

static inline size_t startCASEFVsB(Cont *p) { return 0; }
static inline size_t endCASEFVsB(Cont *p) { return getCInfoPtr(p)->layoutInfo.boxedCount; }
static inline size_t startCASEFVsU(Cont *p) { return endCASEFVsB(p); }
static inline size_t endCASEFVsU(Cont *p) { return startCASEFVsU(p) + getCInfoPtr(p)->layoutInfo.unboxedCount; }


// use LSB to say it is a FORWARD
static inline InfoTab *setLSB(InfoTab *ptr) { 
  return (InfoTab *)((uintptr_t)ptr | 1); 
}
static inline InfoTab *unsetLSB(InfoTab *ptr) { 
  return (InfoTab *)((uintptr_t)ptr & ~1); 
}
static inline bool isLSBset(InfoTab *ptr) { 
  return (bool)((uintptr_t)ptr & 1); 
}

// end of wrappers

#endif
