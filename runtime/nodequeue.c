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


#if TWOLOCKQUEUE

#include <abt.h>
ABT_mutex hlock;
ABT_mutex tlock;

Node *head, *tail;

void NQ_init() {
  Node *np = malloc(sizeof(Node));
  np->next = NULL;
  head = tail = np;
  ABT_mutex_create(&hlock);
  ABT_mutex_create(&tlock);
}

void NQ_enqueue(T value) {
  Node *nodep = malloc(sizeof(Node));
  nodep->value = value;
  nodep->next = NULL;
  ABT_mutex_lock(tlock);
  tail->next = nodep;
  tail = nodep;
  ABT_mutex_unlock(tlock);
}

bool NQ_dequeue(T *value) {
  ABT_mutex_lock(hlock);
  Node *nodep = head;
  Node *newhead = nodep->next;
  if (newhead == NULL) {
     ABT_mutex_unlock(hlock);
     return false;
  }
  *value = newhead->value;
  head = newhead;
  ABT_mutex_unlock(hlock);
  free(nodep);
  return true;
}

#elif LOCKFREE_QUEUE

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

#else 

#if USE_LOCK
#include <abt.h>
#endif 

Pointer head, tail;

#if USE_LOCK
ABT_mutex hlock;
ABT_mutex tlock;
#endif

void NQ_init() {
  Node *np = malloc(sizeof(Node));
  np->next.ptr = NULL;
  head.ptr = tail.ptr = np;
#if USE_LOCK
  ABT_mutex_create(&hlock);
  ABT_mutex_create(&tlock);
#endif
}

void NQ_enqueue(T value) {
#if USE_LOCK
  ABT_mutex_lock(tlock);
#endif
  Pointer tailtmp;
  Node *nodep = malloc(sizeof(Node));
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
  ABT_mutex_unlock(tlock);
#endif
}

bool NQ_dequeue(T *value) {
#if USE_LOCK
  ABT_mutex_lock(tlock);
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
          ABT_mutex_unlock(tlock); 
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
  free(headtmp.ptr);
#if USE_LOCK
  ABT_mutex_unlock(tlock);
#endif
  return true;
}

#endif
