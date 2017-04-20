#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>
#include "nodepool.h"
#include "args.h"
#include "log.h"

Pointer tosp;
size_t size;

void NP_init(size_t _size) {
  tosp.ptr = NULL; 
  tosp.count = 0;
  size = _size;
  LOG(LOG_DEBUG, "NodePool() &tosp = %p tosp.ptr = %p, tosp.count = %" PRIu64 "\n", 
	  &tosp,
	  tosp.ptr, 
	  tosp.count);
  for (size_t i = 0; i != size; i++) {
    Node *newNodep = malloc(sizeof(Node));
    if (newNodep == NULL) {
      LOG(LOG_ERROR, "NP_take:  out of memory\n");
      exit(1);
    }
    newNodep->next.ptr = tosp.ptr;
    newNodep->next.count = tosp.count;
    tosp.ptr = newNodep;
    tosp.count++;
  }
}


Node *NP_take() {
  Pointer current = tosp;
  while (current.ptr) {
    if ( __sync_bool_compare_and_swap((__int128 *)&tosp, 
		        current.bits, 
                ((Pointer){current.ptr->next.ptr, current.count+1}).bits) )
      return current.ptr;
  }
  // no nodes in pool, create new one, Pointer value doesn't matter
  Node *newNode = malloc(sizeof(Node));
  if (newNode == NULL) {
    LOG(LOG_ERROR, "NP_take:  out of memory\n");
    exit(1);
  }
  return newNode;
}


void NP_release(Node *nodep) {
  do {
    LOG(LOG_DEBUG, "release(%p) tosp.ptr = %p, tosp.count = %" PRIu64 "\n", 
	    nodep, tosp.ptr, tosp.count);    
    nodep->next = tosp;
  } while( !__sync_bool_compare_and_swap(
	          (__int128 *)&tosp,
		  nodep->next.bits,
		  ((Pointer){nodep, nodep->next.count+1}).bits));

  //  This must be broken, but in such a way as to no more than create a space leak?
  //  } while( !__sync_bool_compare_and_swap(
  //	          (__int128 *)&tosp,
  //		  nodep->next.bits,
  //		  ((Pointer){nodep->next.ptr, nodep->next.count+1}).bits));
}




