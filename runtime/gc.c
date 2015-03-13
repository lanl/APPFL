#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

#include "gc.h"
#include "stg.h"

void *to=NULL, *from=NULL;
void *scan=NULL, *free=NULL;

void initGc(void) {
  assert(stgHeap && "heap not defined"); 
  from = stgHeap;
  to = (char *)stgHeap + stgHeapSize/2;
  scan = to;
  free = to;
}

void swapPtrs(void) {
  assert( scan == free && "swapPtrs called when gc not finished");
  stgHeap = to;
  stgHP = free;
  to = from;
  from = stgHP;
}

bool isFrom(void *p) {
  return (p >= from && (char *)p < (char *)from + stgHeapSize/2); 
}


