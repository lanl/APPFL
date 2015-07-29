#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"

const bool DEBUG = true;

// fraction of total heap used before gc runs.
const float gcThreshold=0.0;

void *toPtr=NULL, *fromPtr=NULL;
void *scanPtr=NULL, *freePtr=NULL;

// wrapper functions for possible interface changes
static inline size_t countFVs(Obj *p) { return p->infoPtr->fvCount; }
static inline size_t startPAPFVs(Obj *p) { return 1; }
static inline size_t endPAPFVs(Obj *p) { return countFVs(p) + 1; }
static inline size_t startPAPargs(Obj *p) { return countFVs(p) + 1; }
static inline size_t endPAPargs(Obj *p) { return p->payload[0].i + 1; }
static inline size_t countCONargs(Obj *p) { return p->infoPtr->conFields.arity; }
static inline size_t startCallargs(Obj *p) { return 1; }
static inline size_t endCallargs(Obj *p) { return p->payload[0].i + 1; }
static inline size_t startCaseargs(Obj *p) { return 0; }
static inline size_t endCaseargs(Obj *p) { return p->infoPtr->fvCount; }
static inline bool isHeap(Obj *p, size_t index) {
  return (p->payload[index].argType == HEAPOBJ);
}
// end of wrappers

void initGc(void) {
  assert(stgHeap && "heap not defined"); 
  fromPtr = stgHeap;
  toPtr = (char *)stgHeap + stgHeapSize/2;
  scanPtr = toPtr;
  freePtr = toPtr;
}

void swapPtrs(void) {
  assert( scanPtr == freePtr && "swapPtrs called when gc not finished");
  stgHeap = toPtr;
  stgHP = freePtr;
  toPtr = fromPtr;
  fromPtr = stgHP;
}

static inline bool isFrom(void *p) {
  return (p >= fromPtr && (char *)p < (char *)fromPtr + stgHeapSize/2); 
}

void updatePtr(PtrOrLiteral f) {
  Obj *p = derefPoL(f);
  if (isFrom(p)) {
    if(p->objType == FORWARD) {
      fprintf(stderr, "already copied\n");
      p = p->payload[0].op;
    } else {
      if (DEBUG) fprintf(stderr, "copy a %s from->to\n", objTypeNames[p->objType]);
      memcpy(freePtr, p, getObjSize(p));
      p->objType = FORWARD;
      p->payload[0].op = freePtr;
      p = freePtr;
      freePtr = (char *)freePtr + getObjSize(p);
    }
  }
}

void processObj(Obj *p) {
  size_t i;
  switch(p->objType) {
  case FUN:
    // freevars
    for(i = 0; i < countFVs(p); i++) {
      updatePtr(p->payload[i]);
    }    
    break;
  case PAP:
    // free vars
    for(i = startPAPFVs(p); i < endPAPFVs(p) + 1; i++) {
      updatePtr(p->payload[i]);
    }
    // args already applied    
    for(i = startPAPargs(p); i < endPAPargs(p); i++) {
      if (isHeap(p, i)) {
         updatePtr(p->payload[i]);
      }
    }
    break;
  case CON:
    for(i = 0; i < countCONargs(p); i++) {
      if (isHeap(p, i)) {
        updatePtr(p->payload[i]);
      }
    }
    break;
  case THUNK:
  case BLACKHOLE:
    for(i = 0; i < countFVs(p); i++) {
      updatePtr(p->payload[i]);
    }
    break;
  case INDIRECT:
    updatePtr(p->payload[0]);
    break;
  default:
    assert (false && "bad obj type");
  }
}

void processCont(Obj *p) {
  size_t i;
  switch(p->objType) {
  case UPDCONT:
    updatePtr(p->payload[0]);
    break;
  case CASECONT:
    for(i = startCaseargs(p); i< endCaseargs(p); i++) {
      updatePtr(p->payload[i]);
    }
    break;
  case CALLCONT:
    for(i = startCallargs(p); i< endCallargs(p); i++) {
      updatePtr(p->payload[i]);
    }
    break;
  case FUNCONT:
    updatePtr(p->payload[0]);
    break;
  default:
    fprintf(stderr, "bad cont. type %d %s\n",p->objType, objTypeNames[p->objType]);
    assert (false);
  }
}

void gc(void) {

  if((char*)stgHP - (char *)stgHeap < gcThreshold*stgHeapSize) {
    return;
  }

  if (DEBUG) fprintf(stderr,"start gc\n");

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

  // process "to" space
   while(scanPtr < freePtr) {
     processObj(scanPtr);
     scanPtr = (char *)scanPtr + getObjSize(scanPtr);
   }

   if (DEBUG) {
     fprintf(stderr, "old heap\n");
     showStgHeap();
   }
   swapPtrs();
   if (DEBUG) {
     fprintf(stderr, "new heap\n");
     showStgHeap();
     fprintf(stderr,"end gc\n");
   }
}
