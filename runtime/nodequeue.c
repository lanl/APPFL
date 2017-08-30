#include "nodequeue.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include "lockfree_queue_c.h"

lockfree_queue lfq;

void NQ_init() {
  lfq = lockfree_queue_create(0);
}

void NQ_enqueue(T value) {
  lockfree_queue_push(lfq, value);
}

bool NQ_dequeue(T *value) {
  return lockfree_queue_pop(lfq, value);
}

