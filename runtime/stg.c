#include <stdio.h>
#include <stdbool.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h> // for  mmap()
#include <fcntl.h>
#include <unistd.h> // for
#include <stdio.h>
#include <errno.h>
#include <string.h>  // for memcpy()
#include <stdlib.h>  // for exit()
#ifndef __APPLE__
#include <malloc.h>  // for memalign()
#endif

#include "stg.h"
#include "cmm.h"
#include "obj.h"
#include "log.h"
#include "heap.h"

void *stgHeap = NULL;
void *stgHP = NULL;
void *toPtr = NULL;
void *fromPtr = NULL;

void *stgStack = NULL;
void *stgSP = NULL;

#if !defined(__clang__) && !USE_ARGTYPE
// register PtrOrLiteral stgCurVal asm("%r15");  // current value STG register
#else
PtrOrLiteral stgCurVal;  // current/return value
PtrOrLiteral stgCurValU;  // current/return value
#endif

const char *objTypeNames[] = {
  "PHONYSTARTOBJ",
  "FUN",
  "PAP",
  "CON",
  "THUNK",
  "BLACKHOLE",
  "INDIRECT"
};

const char *contTypeNames[] = {
  "BADCONTTYPE0",
  "BADCONTTYPE1",
  "BADCONTTYPE2",
  "BADCONTTYPE3",
  "BADCONTTYPE4",
  "BADCONTTYPE5",
  "PHONYSTARTCONT",
  "UPDCONT",
  "CASECONT",
  "CALLCONT",
  "STACKCONT",
  "POPMECONT",
  "LETCONT",
};

void startCheck() {
  if (sizeof(Obj) % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "sizeof(Obj) is %lu not multiple of %d\n",
                    sizeof(Obj), OBJ_ALIGN);
    exit(1);
  }
}

void showIT(InfoTab *itp) {
    LOG(LOG_DEBUG, "showIT: %s %s, bc %d ubc %d layoutInfo.payloadSize %d\n",
	  objTypeNames[itp->objType],
	  itp->name,
	  itp->layoutInfo.boxedCount,
          itp->layoutInfo.unboxedCount,
	  itp->layoutInfo.payloadSize);
}

const size_t stgHeapSize  = (size_t)(1024*1024*1024);
const size_t stgStackSize  = (size_t)(1024*1024*1024);

void initStg() {
  stgHeap =
    mmap( NULL,                   // void *address, NULL => no preference
	  stgHeapSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON, // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgHeap == MAP_FAILED) {
    LOG(LOG_FATAL, "mmap stg heap failed!!\n");
    exit(1);
  }

  if ((uintptr_t)stgHeap % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "stgHeap not OBJ_ALIGNed\n");
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
    LOG(LOG_FATAL, "mmap stg stack failed!!\n");
    exit(1);
  }
  
  if ((uintptr_t)stgStack % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "stgStack not OBJ_ALIGNed\n");
    exit(1);
  }
  stgSP = (char *)stgStack + stgStackSize;
  if ((uintptr_t)stgSP % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "stgSP not OBJ_ALIGNed\n");
    exit(1);
  }

  LOG(LOG_INFO, "Stg stack at %p and heap at %p\n", stgStack, stgHP);

  stgStatObjCount = 0;

}
