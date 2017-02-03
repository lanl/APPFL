// Michael and Scott, 
//   "Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
//   Queue Algorithms"
// see also
// http://www.drdobbs.com/lock-free-data-structures-with-hazard-po/184401890
// https://www.research.ibm.com/people/m/michael/ieeetpds-2004.pdf
// http://www.cs.technion.ac.il/~mad/publications/ppopp2013-x86queues.pdf

#if 1 

#include "nodequeue.h"
#include "nodepool.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>

Pointer head, tail;

void NQ_init() {
  Node *np = NP_take();
  np->next.ptr = NULL;
  head.ptr = tail.ptr = np;
}

void NQ_enqueue(T value) {
  Pointer tailtmp;
  Node *nodep = NP_take();
  nodep->value = value;
  nodep->next.ptr = NULL;
  while (1) {
    tailtmp = tail;
    Pointer next = tailtmp.ptr->next;
    if (tailtmp.bits == tail.bits) {
      if (next.ptr == NULL) {
	    if (cas128((__int128 *)&tailtmp.ptr->next,
		           next.bits, 
                   ((Pointer){nodep, next.count+1}).bits))

	     break;
      } else {
	    cas128((__int128 *)&tail, 
	           tailtmp.bits, 
               ((Pointer){next.ptr, tailtmp.count+1}).bits);
      }
    }
  }
  cas128((__int128 *)&tail, 
         tailtmp.bits, 
         ((Pointer){nodep, tailtmp.count+1}).bits);
}

bool NQ_dequeue(T *value) {
  Pointer headtmp;
  while(1) {
    headtmp = head;
    Pointer tailtmp = tail;
    Pointer next = headtmp.ptr->next;
    if (headtmp.bits == head.bits) {
      if (headtmp.ptr == tailtmp.ptr) {
	    if (next.ptr == NULL)
	      return false;
	    cas128((__int128 *)&tail, 
	           tailtmp.bits, 
               ((Pointer){next.ptr, tailtmp.count+1}).bits);
      } else {
	    *value = next.ptr->value;
	    if (cas128((__int128 *)&head, 
		    headtmp.bits,
		    ((Pointer){next.ptr, headtmp.count+1}).bits))
	        break;
      }
    }
  }
  NP_release(headtmp.ptr);
  return true;
}
#else

#include "lfq.h"
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

typedef uintptr_t T;

Queue *queue;

void NQ_init() {
  queue = q_initialize();
}

void NQ_enqueue(T value) {
  fprintf(stderr,"enqueue %ld\n",value);
  qpush(queue, value);
}

bool NQ_dequeue(T *value) {
  unsigned int* v = NULL;
  v = qpop(queue,0);
  if (v != NULL) {
    *value = (unsigned long *)v;
    fprintf(stderr,"dequeue %ld\n", *value);
    return true;
  } else {
    return false;
  }
}



#endif



