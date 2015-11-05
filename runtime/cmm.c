#include "assert.h"
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

#include "cmm.h"

const size_t cmmStackSize = (size_t)4*(size_t)(1024*1024*1024);

// extern PtrOrLiteral _POP();

extern void _PUSH(PtrOrLiteral V);

void *cmmStack = NULL;
void *cmmSP = NULL;

void initCmm() {
  cmmStack = 
    mmap( NULL,                   // void *address, NULL => no preference
	  cmmStackSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON, // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (cmmStack == MAP_FAILED) {
    fprintf(stderr,"mmap cmm stack failed!!\n"); 
    exit(1);
  }
  cmmSP = ((char *)cmmStack) + cmmStackSize;
  fprintf(stderr,"Cmm stack %p allocated, initial cmmSP %p\n", cmmStack, cmmSP);

  /* sanity test
  PtrOrLiteral v;
  for (int i = 0; i != cmmStackSize/sizeof(PtrOrLiteral); i++) _PUSH(v);
  for (int i = 0; i != cmmStackSize/sizeof(PtrOrLiteral); i++) _POP();
  if (cmmSP != ((char *)cmmStack) + cmmStackSize)
    fprintf(stderr,"cmm stack failed test!\n");
  */
    
  
}
