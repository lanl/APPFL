// Michael and Scott, 
//   "Simple, Fast, and Practical Non-Blocking and Blocking Concurrent
//   Queue Algorithms"
// see also
// http://www.drdobbs.com/lock-free-data-structures-with-hazard-po/184401890
// https://www.research.ibm.com/people/m/michael/ieeetpds-2004.pdf
// http://www.cs.technion.ac.il/~mad/publications/ppopp2013-x86queues.pdf

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
	    if (__sync_bool_compare_and_swap((__int128 *)&tailtmp.ptr->next,
		           next.bits, 
                   ((Pointer){nodep, next.count+1}).bits))

	     break;
      } else {
	    __sync_bool_compare_and_swap((__int128 *)&tail, 
	           tailtmp.bits, 
               ((Pointer){next.ptr, tailtmp.count+1}).bits);
      }
    }
  }
  __sync_bool_compare_and_swap((__int128 *)&tail, 
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
	    __sync_bool_compare_and_swap((__int128 *)&tail, 
	           tailtmp.bits, 
               ((Pointer){next.ptr, tailtmp.count+1}).bits);
      } else {
	    *value = next.ptr->value;
	    if (__sync_bool_compare_and_swap((__int128 *)&head, 
		    headtmp.bits,
		    ((Pointer){next.ptr, headtmp.count+1}).bits))
	        break;
      }
    }
  }
  NP_release(headtmp.ptr);
  return true;
}

