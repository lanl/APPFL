// node pool implemented by lock-free stack

#ifndef nodepool_h
#define nodepool_h

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include "cascpp.h"

struct _Node;
typedef struct _Node Node;

// payload type, any size
typedef uintptr_t T;

// Pointer is a CAS-able value
// here 128 bit
typedef struct {
  Node *ptr;
  uint64_t count;
} Pointer __attribute__ ((aligned (16)));  // else CAS will segfault?

// Node is a Pointer and a payload
typedef struct {
  Pointer next;
  T value;
} Node __attribute__ ((aligned (16)));  // is aligned transitive?

void NP_init(size_t _size);  // initial size

Node *take();

void release(Node *p);

#endif // ifndef nodepool_h
