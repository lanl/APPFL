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
  size_t before = stgHP-stgHeap;
  stgHeap = toPtr;
  stgHP = freePtr;
  toPtr = fromPtr;
  fromPtr = stgHeap;
  freePtr = toPtr;
  scanPtr = toPtr;
  assert (stgHP-stgHeap <= before && "gc increased heap size!\n"); 
}

static inline bool isFrom(void *p) {
  return (p >= fromPtr && (char *)p < (char *)fromPtr + stgHeapSize/2); 
}

static inline bool isTo(void *p) {
  return (p >= toPtr && (char *)p < (char *)toPtr + stgHeapSize/2);
}


void updatePtr(PtrOrLiteral *f) {
  if(f->argType == HEAPOBJ) {

	Obj *p = derefPoL(*f);


	if(f->op->objType == INDIRECT) {
		if(!isFrom(p) && !isTo(p)) {  // INDIRECT to SHO
	      if(isFrom(f->op)) {
	    	  fprintf(stderr,"fix INDIRECT to sho %s\n",f->op->ident);
	    	  f->op = p;
#if 0
	    	if(f->op->objType == FORWARD) {
	    		//if (DEBUG) fprintf(stderr,"update indirect forward %s\n",p->ident);
	    		//f->op = p->payload[0].op;
	    	} else {
				fprintf(stderr,"copy INDIRECT to sho %s\n",f->op->ident);
				fprintf(stderr,"f->op\n");
				showStgObj(f->op);
				fprintf(stderr,"p\n");
				showStgObj(p);

				int size = getObjSize(f->op);
				memcpy(freePtr, f->op, getObjSize(f->op));
				f->op->objType = FORWARD;
				f->op->payload[0].op = freePtr;
				//f->op = (Obj *)freePtr;
				freePtr = (char *)freePtr + size;

	    	}
	    	return;
#endif
	      }
		}
	}



    if (isFrom(p)) {
      if(p->objType == FORWARD) {
        if (DEBUG) fprintf(stderr,"update forward %s\n",p->ident);
        f->op = p->payload[0].op;
      } else {
        int size = getObjSize(p);
        if (DEBUG) {
          fprintf(stderr, "copy %s %s from->to size=%d\n", objTypeNames[p->objType], p->ident, size);
          fprintf(stderr, "before\n");
          showStgObj(p);
        }

        memcpy(freePtr, p, size);
        if (DEBUG) {
          fprintf(stderr, "after to\n");
          showStgObj((Obj *)freePtr);
        }
        assert(size > sizeof(Obj) && "no space for FORWARD");
        p->objType = FORWARD;
        p->payload[0].op = freePtr;
        if (DEBUG) {
          fprintf(stderr, "after from\n");
          showStgObj(p);
        }

        f->op = (Obj *)freePtr;
        if (DEBUG) {
          fprintf(stderr, "after from2\n");
          showStgObj(f->op);
          fprintf(stderr, "after from3\n");
          showStgObj(p);
        }

        freePtr = (char *)freePtr + size;
      }
    } else if (isTo(p)) {

    } else {
      // SHO
      fprintf(stderr,"it is a sho\n");
      showStgObj(p);
    }
  }
}


void processObj(Obj *p) {
  size_t i;
  if (DEBUG) fprintf(stderr,"processObj %s %s\n",objTypeNames[p->objType], p->ident);
  switch(p->objType) {
  case FUN:
    // freevars
    for(i = 0; i < countFVs(p); i++) {
      updatePtr(&p->payload[i]);
    }    
    break;
  case PAP:
    // free vars
    for(i = startPAPFVs(p); i < endPAPFVs(p); i++) {
      updatePtr(&p->payload[i]);
    }
    // args already applied    
    for(i = startPAPargs(p); i < endPAPargs(p); i++) {
         updatePtr(&p->payload[i]);
    }
    break;
  case CON:
	  if (DEBUG) fprintf(stderr,"CON args %zu\n",countCONargs(p));
    for(i = 0; i < countCONargs(p); i++) {
        updatePtr(&p->payload[i]);
    }
    break;
  case THUNK:
  case BLACKHOLE:
    for(i = 0; i < countFVs(p); i++) {
      updatePtr(&p->payload[i]);
    }
    break;
  case INDIRECT:
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
    for(i = startCaseargs(p); i < endCaseargs(p); i++) {
      updatePtr(&p->payload[i]);
    }
    break;
  case CALLCONT:
    for(i = startCallargs(p); i < endCallargs(p); i++) {
      updatePtr(&p->payload[i]);
    }
    break;
  case FUNCONT:
    updatePtr(&p->payload[0]);
    break;
  default:
    fprintf(stderr, "bad cont. type %d %s\n",p->objType, objTypeNames[p->objType]);
    assert (false);
  }
}

void gc(void) {

  size_t before = stgHP-stgHeap;
  if(before <= gcThreshold*stgHeapSize) {
    return;
  }

  if (DEBUG) {
    fprintf(stderr, "old heap\n");
    showStgHeap();
    fprintf(stderr,"start gc heap size %lx\n", before);
  }

  // add stgCurVal
  fprintf(stderr,"start add stgCurVal\n");
  if (stgCurVal.argType == HEAPOBJ) {
	  processObj(stgCurVal.op);
  }
  fprintf(stderr,"end add stgCurVal\n");

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
  fprintf(stderr,"start process to\n");
   while(scanPtr < freePtr) {
     processObj(scanPtr);
     scanPtr = (char *)scanPtr + getObjSize(scanPtr);
   }
   fprintf(stderr,"end process to\n");

   assert( scanPtr == freePtr && "gc not finished");

   //update stgCurVal
   fprintf(stderr,"start process stgCurVal\n");
   if (stgCurVal.argType == HEAPOBJ) {
	  updatePtr(&stgCurVal);
   }
   fprintf(stderr,"end process stgCurVal\n");

   swapPtrs();

   if (DEBUG) {
     fprintf(stderr, "new heap\n");
     showStgHeap();
     size_t after = stgHP-stgHeap;
     fprintf(stderr,"end gc heap size %lx (change %lx)\n", 
       after, before-after );
   }
}
