// non-locking node queue

#ifndef NODEQUEUE_H
#define NODEQUEUE_H

#include "nodepool.h"
#include <stdint.h>

void NQ_init();
void NQ_enqueue(T value);
bool NQ_dequeue(T *value);

#endif // NODEQUEUE_H
