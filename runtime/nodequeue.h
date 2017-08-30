// non-locking node queue

#ifndef QUEUE_H
#define QUEUE_H

#include <stdint.h>
#include <stdbool.h>

typedef uintptr_t T;

void NQ_init();
void NQ_enqueue(T value);
bool NQ_dequeue(T *value);

#endif // QUEUE_H
