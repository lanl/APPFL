#include <stdio.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h> // for  mmap()
#include <fcntl.h>
#include <unistd.h> // for 
#include <stdio.h>
#include <errno.h>
#include <string.h>  // for memcpy()
#include <stdlib.h>  // for exit()
#include <malloc.h>  // for memalign()

#include "stg.h"
#include "cmm.h"

void *stgHeap = NULL;
void *stgHP = NULL;
void *stgStack = NULL;
void *stgSP = NULL;
PtrOrLiteral stgCurVal;  // current value STG register

const char *objTypeNames[] = {
  "FUN", 
  "PAP", 
  "CON",
  "THUNK",
  "BLACKHOLE",
  "INDIRECT",
  "UPDCONT", 
  "CASECONT", 
  "CALLCONT", 
  "FUNCONT",
  "FORWARD",
};

Obj *stgAllocCont(InfoTab *itp) {
  assert( itp->objType != CALLCONT &&
          "stgAllocCont: itp->objType == CALLCONT" );
  int payloadSize = itp->layoutInfo.payloadSize;
  size_t objSize = sizeof(Obj) + payloadSize * sizeof(PtrOrLiteral);
  fprintf(stderr, "allocating continuation with payloadSize %d\n", payloadSize);
  showIT(itp);
  stgSP = (char *)stgSP - objSize;
  assert(stgSP >= stgStack);
  Obj *objp = (Obj *)stgSP;
  objp->infoPtr = itp;
  objp->objType = itp->objType;
  objp->argCount = 0;  // for PAP, this will go
  strcpy(objp->ident, itp->name);  // may be overwritten
  return objp;
}

// this is still a hack:  we need the whole family of stgApply functions
// and an analogous set of infoTabs with correct layout info for
// CALLCONTs; alternatively, CALLCONTs could be more self-describing like PAPs
// for now payload[0].i == argc, the number of subsequent args
Obj *stgAllocCallCont2(InfoTab *itp, int argc) {
  assert(itp->objType == CALLCONT && 
	 "stgAllocCallCont: itp->objType != CALLCONT");
  size_t objSize = sizeof(Obj) + (argc + 1) * sizeof(PtrOrLiteral);
  fprintf(stderr, "allocating call continuation with argc %d\n", argc);
  showIT(itp);
  stgSP = (char *)stgSP - objSize;
  assert(stgSP >= stgStack);
  Obj *objp = (Obj *)stgSP;
  objp->infoPtr = itp;
  objp->objType = itp->objType;
  objp->argCount = 0;  // for PAP, this will go
  objp->payload[0] = (PtrOrLiteral) {.argType = INT, .i = argc};
  strcpy(objp->ident, itp->name);  // may be overwritten
  return objp;
}

// TODO: 64 bit align
// return pointer to popped cont
// NOTE:  stgAllocCont with invalidate this pointer!
// CALLCONT is special
Obj *stgPopCont() {
  Obj *retVal = (Obj *)stgSP;
  // this isn't robust for multiple reasons
  assert( retVal->infoPtr->objType >= UPDCONT &&
          retVal->infoPtr->objType <= FUNCONT);
  int payloadSize;
  if (retVal->infoPtr->objType == CALLCONT) {
    payloadSize = retVal->payload[0].i + 1;
    fprintf(stderr, "popping call continuation with argc %d\n", payloadSize-1);
  } else {
    payloadSize = retVal->infoPtr->layoutInfo.payloadSize;
    fprintf(stderr, "popping continuation with payloadSize %d\n", payloadSize);
  }
  size_t objSize = sizeof(Obj) + payloadSize * sizeof(PtrOrLiteral);
  showIT(retVal->infoPtr);
  assert((char *)retVal + objSize <= (char *)stgStack + stgStackSize);
  stgSP = (char *)retVal + objSize;
  return retVal;
}

Obj* stgNewHeapObj(InfoTab *itp) {
  fprintf(stderr, "stgNewHeapObj: "); showIT(itp);
  int payloadSize = itp->layoutInfo.payloadSize;
  size_t objSize = sizeof(Obj) + payloadSize * sizeof(PtrOrLiteral);
  Obj *objp = (Obj *)stgHP;
  stgHP = (char *)stgHP + objSize;
  objp->infoPtr = itp;
  objp->objType = itp->objType;
  objp->argCount = 0;  // for PAP, this will go
  strcpy(objp->ident, itp->name);  // may be overwritten
  fprintf(stderr, "stgNewHeapObj: "); showStgObj(objp);
  return objp;
}

// PAP is special
Obj* stgNewHeapPAP(InfoTab *itp, int argc) {
  assert(itp->objType == FUN && "stgNewHeapPap:  itp->objType != FUN" );
  fprintf(stderr, "stgNewHeapPap: "); showIT(itp);
  size_t objSize = sizeof(Obj) + (argc + 1) * sizeof(PtrOrLiteral);
  Obj *objp = (Obj *)stgHP;
  stgHP = (char *)stgHP + objSize;
  objp->infoPtr = itp;
  objp->objType = PAP;
  objp->argCount = argc;  // for PAP, this will go
  objp->payload[0] = (PtrOrLiteral) {.argType = INT, .i = argc};
  strcpy(objp->ident, itp->name);  // may be overwritten
  fprintf(stderr, "stgNewHeapPAP: "); showStgObj(objp);
  return objp;
}

void showStgObjPretty(Obj *p);
void showStgObjDebug(Obj *p);

void showStgObj(Obj *p) {
  showStgObjPretty(p);
  // showStgObjDebug(Obj *p)
}
void showStgValDebug(PtrOrLiteral v);
void showStgValPretty(PtrOrLiteral v);

void showStgVal(PtrOrLiteral v) {
  showStgValPretty(v);
  // showStgValDebug(v);
}

void showIT(InfoTab *itp) {
  fprintf(stderr, "showIT: %s %s, fvCount %d", 
	  objTypeNames[itp->objType], 
	  itp->name, 
	  itp->fvCount);
  if (itp->objType != CALLCONT)
    fprintf(stderr, ", layoutInfo.payloadSize %d", itp->layoutInfo.payloadSize);
  fprintf(stderr, "\n");
}  

// ****************************************************************

const int showDepthLimit = 1000;
int depth;
Obj *stack[1000];
int stackp;


void showStgObjRecDebug(Obj *p);
void showStgObjDebug(Obj *p) {
  depth = 0;
  showStgObjRecDebug(p);
}

void showStgObjRecPretty(Obj *p);
void showStgObjPretty(Obj *p) {
  depth = 0;
  showStgObjRecPretty(p);
  fprintf(stderr,"\n");
}

void showStgCont(Obj *c) {
  switch (c->objType) {
  case UPDCONT:
    fprintf(stderr,"UPDCONT  %s\n", c->ident);
    return;

  case CASECONT:
    fprintf(stderr,"CASECONT %s\n", c->ident);
    return;

  case CALLCONT:
    fprintf(stderr,"CALLCONT %s\n", c->ident);
    return;

  case FUNCONT:
    fprintf(stderr,"FUNCONT  %s\n", c->ident);
    return;

  default:
    fprintf(stderr,"showStgCont default case! %d %s\n", c->objType, objTypeNames[c->objType]);
    exit(0);
  }
}

void showStgObjRecPretty(Obj *p) {

  // depth check first
  if (depth+1 >= showDepthLimit) {
    fprintf(stderr, "******showStgObjRec depth exceeded\n");
    return;
  }

  InfoTab it = *(p->infoPtr);

  for (int i=0; i != depth; i++) {
    if (p == stack[i]) {
      fprintf(stderr, "((%s))", it.name);
      return;
    }
  }
  stack[depth++] = p;

  if (p->objType != BLACKHOLE &&
      p->objType != INDIRECT &&
      p->objType != FORWARD &&
      p->objType != it.objType) {
	    if(!(p->objType == PAP && it.objType == FUN)) {
          fprintf(stderr, "mismatch in infotab and object type! %d != %d\n",
        		p->objType, it.objType);
          exit(0);
	    }
  }
  if (strcmp(it.name, p->ident)) {
    fprintf(stderr, "mismatch in infotab and object names \"%s\" != \"%s\"\n",
	    it.name, p->ident);
    exit(0);
  }

  switch (p->objType) {
  case FUN:
  case PAP:
  case THUNK:
  case BLACKHOLE:
    fprintf(stderr, "%s = <%s>", p->ident, objTypeNames[p->objType]);
    break;

  case CON:
    fprintf(stderr, "%s = %s", p->ident, it.conFields.conName );
    int arity = it.conFields.arity;
    if (arity > 0) {
      if (arity > 1) fprintf(stderr, "(");
      else fprintf(stderr, " ");
      showStgValPretty(p->payload[0]);
      for (int i = 1; i < arity; i++) {
	    fprintf(stderr, ", ");
	    showStgValPretty(p->payload[i]);
      }
      if (arity > 1) fprintf(stderr, ")");
    }
    break;

  case INDIRECT:
    fprintf(stderr, "%s --> ", p->ident );
    showStgObjRecPretty(p->payload[0].op);
    break;

  case FORWARD:
    fprintf(stderr, "FORWARD" );
    break;

  default:
    fprintf(stderr,"********* default in showStgObj!\n");
    // exit(0);
  }
  depth--;
}

void showStgValPretty(PtrOrLiteral v) {
  switch (v.argType) {
  case INT:
    fprintf(stderr,"%d", v.i);
    break;
  case DOUBLE:
    fprintf(stderr,"%f", v.d);
    break;
    //  case FLOAT:
    //    fprintf(stderr,"%f", v.f);
    //    break;
  case HEAPOBJ:
    showStgObjRecPretty(v.op);
    break;
  default:
    fprintf(stderr,"undefined PtrOrLiteral.tag! tag=%d\n", v.argType);
    exit(1);
  }
}


void showStgObjRecDebug(Obj *p) {

  // depth check first
  if (depth+1 >= showDepthLimit) {
    fprintf(stderr, "******showStgObjRec depth exceeded\n");
    return;
  }
  for (int i=0; i != depth; i++) {
    if (p == stack[i]) {
      fprintf(stderr, "   ***cycle\n");
      return;
    }
  }
  stack[depth++] = p;



  InfoTab it = *(p->infoPtr);
  fprintf(stderr, "%s %s %s ", objTypeNames[p->objType],
	  objTypeNames[it.objType], it.name);
  switch (p->objType) {
  case FUN:
    fprintf(stderr,"\n");
    break;

  case PAP:
    fprintf(stderr,"\n");
    break;

  case CON:
    fprintf(stderr,"tag %d arity %d\n", it.conFields.tag, it.conFields.arity );
    for (int i = 0; i != it.conFields.arity; i++)
      showStgValDebug(p->payload[i]);
    break;

  case THUNK:
    fprintf(stderr,"\n");
    break;

  case BLACKHOLE:
    fprintf(stderr,"\n");
    break;

  case INDIRECT:
    fprintf(stderr,"INDIRECT to\n");
    showStgObjRecDebug(p->payload[0].op);
    break;

  case FORWARD:
    fprintf(stderr,"\n");
    break;

  default:
    fprintf(stderr,"default in showStgObj!\n");
    exit(1);
  }
  depth--;
}

void showStgValDebug(PtrOrLiteral v) {
  switch (v.argType) {
  case INT:
    fprintf(stderr,"INT %d\n", v.i);
    break;
  case DOUBLE:
    fprintf(stderr,"DOUBLE %f\n", v.d);
    break;
  case HEAPOBJ:
    fprintf(stderr,"HEAPOBJ %p ", v.op);
    showStgObjRecDebug(v.op);
    break;
  default:
    fprintf(stderr,"undefined PtrOrLiteral.tag!\n");
    exit(0);
  }
}


size_t stgStatObjCount;
Obj * stgStatObj[100];

int getObjSize(Obj *o) {
  if (o->objType == CALLCONT || o->objType == PAP) {
    return  sizeof(Obj) + (o->payload[0].i+1) * sizeof(PtrOrLiteral);
  } else {
    return  sizeof(Obj) + o->infoPtr->layoutInfo.payloadSize * sizeof(PtrOrLiteral);;
  }
}

void showStgStack() {
  fprintf(stderr,"\nSTG Stack:\n\n");
  for (char *p = (char*)stgSP;
       p < (char*)stgStack + stgStackSize;
       p += getObjSize((Obj *)p)) {
     showStgCont((Obj *)p);
   }
}

void showStgHeap() {
  fprintf(stderr,"\nSTG static objects: \n\n");
  for (int i = 0; i != stgStatObjCount; i++) {
    showStgObj(stgStatObj[i]);
    fprintf(stderr,"\n");
  }

  fprintf(stderr,"\nSTG heap:\n\n");
  for (char *p = (char*)stgHeap;
      p < (char*)stgHP;
      p += getObjSize((Obj *)p)) {
    showStgObj((Obj *)p);
  }

  showStgStack();
}

const size_t stgHeapSize  = (size_t)4*(size_t)(1024*1024*1024);
const size_t stgStackSize  = (size_t)4*(size_t)(1024*1024*1024);

void initStg() {
  stgHeap =
    mmap( NULL,                   // void *address, NULL => no preference
	  stgHeapSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON, // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgHeap == MAP_FAILED) {
    fprintf(stderr,"mmap stg heap failed!!\n"); 
    exit(1);
  }

  stgHP = stgHeap; // first free address

  stgStack = 
    mmap( NULL,                   // void *address, NULL => no preference
	  stgStackSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON,  // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgStack == MAP_FAILED) {
    fprintf(stderr,"mmap stg stack failed!!\n"); 
    exit(1);
  }
  
  stgSP = (char *)stgStack + stgStackSize;

  fprintf(stderr,"Stg stack at %p and heap at %p\n", stgStack, stgHP);

  stgStatObjCount = 0;

}
