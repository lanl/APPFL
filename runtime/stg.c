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
extern void stgPushCont(Obj c);
extern Obj stgPopCont();

void *stgHeap = NULL;
void *stgHP = NULL;
void *stgStack = NULL;
void *stgSP = NULL;
PtrOrLiteral stgCurVal;  // current value STG register

Obj* stgNewHeapObj() {
  Obj *curp = (Obj *)stgHP;
  stgHP = (Obj *)stgHP + 1;
  return curp;
}

void showStgObj(Obj *p) {
  Obj o = *p;
  InfoTab it = *o.infoPtr;
  fprintf(stderr, "name %s\n", it.name);
  switch (o.objType) {
  case FUN:
    fprintf(stderr,"FUN\n");
    break;

  case PAP:
    fprintf(stderr,"PAP\n");
    break;

  case CON:
    fprintf(stderr,"CON %d\n", it.conFields.tag );
    break;

  case THUNK:
    fprintf(stderr,"THUNK\n");
    break;

  case BLACKHOLE:
    fprintf(stderr,"BLACKHOLE\n");
    break;

  case UPDCONT:
    fprintf(stderr,"UPDCONT\n");
    break;

  case INDIRECT:
    fprintf(stderr,"INDIRECT to\n");
    showStgObj(o.payload[0].op);
    break;

  case CASECONT:
    fprintf(stderr,"CASECONT\n");
    break;

  case CALLCONT:
    fprintf(stderr,"CALLCONT\n");
    break;

  case FUNCONT:
    fprintf(stderr,"FUNCONT\n");
    break;

  default:
    fprintf(stderr,"default in showStgObj!\n");
    exit(1);
  }
}

void showStgVal(PtrOrLiteral v) {
  switch (v.argType) {
  case INT:
    fprintf(stderr,"INT %d\n", v.i);
    break;
  case DOUBLE:
    fprintf(stderr,"DOUBLE %f\n", v.d);
    break;
  case HEAPOBJ:
    fprintf(stderr,"HEAPOBJ %p\n", v.op);
    showStgObj(v.op);
    break;
  default:
    fprintf(stderr,"undefined PtrOrLiteral.tag!\n");
    exit(0);
  }
}


size_t stgStatObjCount;
Obj * stgStatObj[100];

void showStgHeap() {
  fprintf(stderr,"\nSTG static objects:\n\n");
  for (int i = 0; i != stgStatObjCount; i++) {
    showStgObj(stgStatObj[i]);
    fprintf(stderr,"\n");
  }
  fprintf(stderr,"\nSTG heap:\n\n");
  for (Obj *p = ((Obj *) stgHP) - 1;
       p >= (Obj *)stgHeap;
       p--) {showStgObj(p); fprintf(stderr,"\n");}
}

const size_t stgHeapSize  = (size_t)4*(size_t)(1024*1024*1024);
const size_t stgStackSize  = (size_t)4*(size_t)(1024*1024*1024);

void showStgStack() {
  fprintf(stderr,"\nSTG stack:\n\n");
  for (Obj *p = ((Obj *) stgSP);
       p < (Obj *)((char *)stgStack + stgStackSize);
       p++) showStgObj(p);
}

void initStg() {
  stgHeap = 
    mmap( NULL,                   // void *address, NULL => no preference
	  stgHeapSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANONYMOUS, // int flags
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
	  MAP_PRIVATE | MAP_ANONYMOUS,  // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgStack == MAP_FAILED) {
    fprintf(stderr,"mmap stg stack failed!!\n"); 
    exit(1);
  }
  
  stgSP = (char *)stgStack + stgStackSize;

  fprintf(stderr,"Stg stack at %p and heap at %p\n", stgStack, stgHP);

  stgStatObjCount = 0;

  /*
  int i;
  int *p = (int *)stgStack + stgStackSize/sizeof(int);
  int *q = (int *)stgHeap;
  for (i = 0; i != stgStackSize/sizeof(int); i++) {
    *--p = i;
    *q++ = i;
  }
  */
}
