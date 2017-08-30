#include "lockfree_queue.h"

namespace{

  using item = void*;

  using lockfree_queue = lockfree::queue<item>;

} // namespace

extern "C"{

  lockfree_queue* lockfree_queue_create(unsigned long capacity){
    return new lockfree_queue(capacity);
  }

  void lockfree_queue_destroy(lockfree_queue* q){
    delete q;
  }

  int lockfree_queue_empty(lockfree_queue* q){
    return q->empty();
  }

  int lockfree_queue_push(lockfree_queue* q, item i){
    return q->push(i);
  }

  int lockfree_queue_pop(lockfree_queue* q, item* i){
    return q->pop(*i);
  }

} // extern "C"
