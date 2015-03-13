#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

#include "gc.h"
#include "stg.h"

void *toPtr=NULL, *fromPtr=NULL;
void *scanPtr=NULL, *freePtr=NULL;

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

bool isFrom(void *p) {
  return (p >= fromPtr && (char *)p < (char *)fromPtr + stgHeapSize/2); 
}


