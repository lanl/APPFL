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

#if USE_LOCK
#include <pthread.h>
#endif 

Pointer head, tail;

#if USE_LOCK
pthread_mutex_t qlock = PTHREAD_MUTEX_INITIALIZER;
#endif

void NQ_init() {
  Node *np = NP_take();
  np->next.ptr = NULL;
  head.ptr = tail.ptr = np;
#if USE_LOCK
  pthread_mutex_init(&qlock, NULL);
#endif
}

void NQ_enqueue(T value) {
#if USE_LOCK
  pthread_mutex_lock(&qlock);
#endif
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
#if USE_LOCK
  pthread_mutex_unlock(&qlock);
#endif
}

bool NQ_dequeue(T *value) {
#if USE_LOCK
  pthread_mutex_lock(&qlock);
#endif
  Pointer headtmp;
  while(1) {
    headtmp = head;
    Pointer tailtmp = tail;
    Pointer next = headtmp.ptr->next;
    if (headtmp.bits == head.bits) {
      if (headtmp.ptr == tailtmp.ptr) {
	    if (next.ptr == NULL) {
#if USE_LOCK
          pthread_mutex_unlock(&qlock); 
#endif
	      return false;
        }
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
#if USE_LOCK
  pthread_mutex_unlock(&qlock);
#endif
  return true;
}

