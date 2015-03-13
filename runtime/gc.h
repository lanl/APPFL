#ifndef gc_h
#define gc_h

#include <stdbool.h>

extern bool HeapSpace;

extern void *stgHeap0, *stgHeap1, *stgHP;

int stgHeapHalf(void *ptr);

 
#endif
