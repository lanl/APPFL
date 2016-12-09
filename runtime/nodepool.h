// node pool implemented by lock-free stack

#ifndef nodepool_h
#define nodepool_h

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#include "cas.h"

struct _Node;
typedef struct _Node Node;

// payload type, any size
typedef uintptr_t T;

// Pointer is a CAS-able value
// here 128 bit
typedef union {
  struct {
    Node *ptr;
    uint64_t count;
  };
  __int128 bits;
} __attribute__ ((aligned (16))) Pointer;

// Node is a Pointer and a payload
struct __attribute__ ((aligned (16))) _Node {
  Pointer next;
  T value;
};

void NP_init(size_t _size);  // initial size

Node *NP_take();

void NP_release(Node *p);

#endif // ifndef nodepool_h
