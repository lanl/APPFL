/*
Copyright (c) 2015, Mike Taghavi (mitghi) <mitghi[at]me.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <stdio.h>
#include <stdlib.h>

#include "lfq.h"

#define MPANIC(x) ;if(x == NULL) { perror("Malloc failed."); exit(1); }
#define RETDISCARD /* VALUE IS DISCARDED */

struct _node{
  Value *value;
  Pointer *next;
};

struct _pointer{
  int count;
  Node *nptr;
};

struct _queue{
  Pointer *head;
  Pointer *tail;
};

Queue *
q_initialize() {

  Queue *queue = (Queue *) malloc(sizeof(Queue)) MPANIC(queue);
  Node *node = (Node *) malloc(sizeof(Node)) MPANIC(node);
  Pointer *nodeptr = (Pointer *) malloc(sizeof(Pointer)) MPANIC(nodeptr);

  node->next = NULL;
  node->value = NULL;
  nodeptr->count = 1;
  nodeptr->nptr = node;
  queue->head = queue->tail = nodeptr;

 /**************************************
 * All nodes are accessed through  
 *  entry pointer.
 *
 * head ..,    .----.   .----.
 *        |->  |nptr|-->|node|->[ptr]NULL
 * tail ..'    `----'   `----'
 ***************************************/

  __sync_synchronize();
  return queue;
}

void
qpush(Queue *queue,void *value) {

  Pointer *head;
  Pointer *tail;
  Pointer *next;

  Node *node = (Node *) malloc(sizeof(Node)) MPANIC(node);

  Pointer * nodeptr = (Pointer *) malloc(sizeof(Pointer)) MPANIC(nodeptr);

  node->value = value;
  node->next = NULL;
  nodeptr->count = 1;
  nodeptr->nptr = node;

  while(1){
    head = queue->head;
    tail = queue->tail;
    //FIX THIS
    next = tail->nptr->next;

    if (tail == queue->tail) {
      if(next == NULL){
	__sync_fetch_and_add(&(nodeptr->count),1);
	if (__sync_bool_compare_and_swap(&(tail->nptr->next),(long long unsigned int)next,nodeptr)){
	  if (__sync_bool_compare_and_swap(&(queue->tail),(long long unsigned int)tail,nodeptr)){
	    break;
	  }
	}
      } else {
	//FIX THIS
	__sync_fetch_and_add(&(next->count),1);
	if (__sync_bool_compare_and_swap(&(queue->tail),(long long unsigned int)tail,next->nptr)){
	}
      }
    }
 }
}

Value *
qpop(Queue *queue,int thrd){

  Pointer *head = NULL;
  Pointer *tail = NULL;
  Pointer *_next = NULL;
  Pointer *tmp = NULL;
  Value *val = NULL;

  while (1){
    head = queue->head;
    tail = queue->tail;

    if(head == tail) return NULL;

    if (queue->head->count == 0) {
      if (__sync_bool_compare_and_swap(&(queue->head->nptr),_next,_next)) return NULL;
      
      Node *_nptr = (Node *) malloc(sizeof(Node)) MPANIC(_nptr);
      _nptr->next = NULL;
      _nptr->value = NULL;
	
      __sync_bool_compare_and_swap(&(queue->tail->count),queue->tail->count,0);
      while(1){ if(__sync_bool_compare_and_swap(&(queue->tail->nptr),queue->tail->nptr,_nptr)) break; }
      while(1){ if(__sync_bool_compare_and_swap(&(queue->head),queue->head,queue->tail)) break; }
    }

    tmp = (Pointer *)queue->head->nptr->next;
    _next = queue->head->nptr->next;
    int count = head->count;

    if (_next == NULL) return NULL;

    if (head == queue->head){
      if (head->nptr == tail->nptr){
	if (_next == NULL) return NULL;
	//FIX THIS
	__sync_fetch_and_add(&(_next->count),1);
	if (!__sync_bool_compare_and_swap(&(queue->tail),(long long unsigned int)tail,_next)) continue;
      } else {
	val = _next->nptr->value;
	//FIX THIS
	__sync_synchronize();

	if (_next->nptr->next == NULL) {
	  free(queue->head->nptr);
	  if (__sync_bool_compare_and_swap(&(queue->head->nptr),(long long unsigned int)queue->head->nptr,_next->nptr)) {
	    __sync_bool_compare_and_swap(&(queue->tail),queue->tail,queue->head);	    
	    free(_next);
	    __sync_synchronize();
	    break;
	  }
	  break;
	}
	__sync_fetch_and_add(&(_next->count),1);
	if (__sync_bool_compare_and_swap(&(queue->head->nptr->next),(long long unsigned int)tmp,_next->nptr->next)) {
	  __sync_synchronize();
	  free(_next->nptr);
	  free(_next);
	  break;
	}
      }
    }
  }

  return val;
}

void
queue_free(Queue *queue){  
  /*TODO:
   * Free remaining nodes if queue->head != queue->tail 
   */

  if (queue->head == queue->tail){
    free(queue->head->nptr);
    free(queue->head);
    free(queue);
  } else if(queue->head != NULL && queue->tail != NULL){
    free(queue->head->nptr);
    free(queue->head);
    free(queue->tail->nptr);
    free(queue->tail);
    free(queue);
  }
}
