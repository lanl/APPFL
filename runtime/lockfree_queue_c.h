#ifndef _LOCKFREE_QUEUE_C_H_
#define _LOCKFREE_QUEUE_C_H_

typedef void* lockfree_queue;
typedef uintptr_t item;

lockfree_queue lockfree_queue_create(unsigned long capacity);

void lockfree_queue_destroy(lockfree_queue q);

int lockfree_queue_empty(lockfree_queue q);

int lockfree_queue_push(lockfree_queue q, item i);

int lockfree_queue_pop(lockfree_queue q, item* i);

#endif // _LOCKFREE_QUEUE_C_H_
