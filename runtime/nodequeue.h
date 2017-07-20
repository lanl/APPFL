// non-locking node queue

#ifndef NODEQUEUE_H
#define NODEQUEUE_H

#include "nodepool.h"
#include <stdint.h>
#include <stdbool.h>

// lock free queue is broken use lock...
// use lock on queue w/ clang for now
#if defined(__clang__)
#define USE_LOCK 0 
#else
#define USE_LOCK 0
#endif

void NQ_init();
void NQ_enqueue(T value);
bool NQ_dequeue(T *value);

#endif // NODEQUEUE_H
