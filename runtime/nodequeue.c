// Michael and Scott, 
//   "Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
//   Queue Algorithms"
// see also
// http://www.drdobbs.com/lock-free-data-structures-with-hazard-po/184401890
// https://www.research.ibm.com/people/m/michael/ieeetpds-2004.pdf
// http://www.cs.technion.ac.il/~mad/publications/ppopp2013-x86queues.pdf

#include "nodequeue.h"
#include "nodepool.h"
#include <bool.h>
#include <stdio.h>
#include <stdint.h>

Pointer head, tail;
NodePool nodePool;

void NQ_init() {
  Node *np = NP_take();
  np->next.ptr = NULL;
  head.ptr = tail.ptr = np;
}

void NQ_enqueue(T value) {
  Pointer tailtmp;
  Node *nodep = nodePool.take();
  nodep->value = value;
  nodep->next.ptr = NULL;
  while (1) {
    tailtmp = tail;
    Pointer next = tailtmp.ptr->next;
    if (tailtmp == tail) {
      if (next.ptr == NULL) {
	if (cas128(&tailtmp.ptr->next,
		   next,
		   (Pointer){nodep, next.count+1}))
	  break;
      } else {
	cas128(&tail, 
	       tailtmp, 
	       (Pointer){next.ptr, tailtmp.count+1});
      }
    }
  }
  cas128(&tail, tailtmp, (Pointer){nodep, tailtmp.count+1});
}

bool NQ_dequeue(T &value) {
  Pointer headtmp;
  while(1) {
    headtmp = head;
    Pointer tailtmp = tail;
    Pointer next = headtmp.ptr->next;
    if (headtmp == head) {
      if (headtmp.ptr == tailtmp.ptr) {
	if (next.ptr == NULL)
	  return false;
	cas128(&tail, 
	       tailtmp, 
	       (Pointer){next.ptr, tailtmp.count+1});
      } else {
	value = next.ptr->value;
	if (cas(&head, 
		headtmp,
		(Pointer){next.ptr, headtmp.count+1}))
	  break;
      }
    }
  }
  nodePool.release(headtmp.ptr);
  return true;
}

