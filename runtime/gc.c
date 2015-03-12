#include "gc.h"
#include "stg.h"

bool HeapSpace = false;

// what half is pointer in (-1 for not in heap)
int stgHeapHalf(void *ptr) {
  if (ptr < stgHeap || (char *)ptr >= (char *)stgHeap + stgHeapSize) return -1;
  return ((char *)ptr >=  (char *)stgHeap + stgHeapSize/2 ) ? 1 : 0;
}

