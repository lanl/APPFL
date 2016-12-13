#include "nodepool.h"
#include <stdbool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <inttypes.h>

Pointer tosp;
size_t size;

void NP_init(size_t _size) {
  tosp.ptr = NULL; 
  tosp.count = 0;
  size = _size;
  fprintf(stderr, "NodePool() &tosp = %p tosp.ptr = %p, tosp.count = %" PRIu64 "\n", 
	  &tosp,
	  tosp.ptr, 
	  tosp.count);
}


Node *NP_take() {
  Pointer current = tosp;
  while (current.ptr) {
    if ( cas128((__int128 *)&tosp, 
		        current.bits, 
                ((Pointer){current.ptr->next.ptr, current.count+1}).bits) )
      return current.ptr;
  }
  // no nodes in pool, create new one, Pointer value doesn't matter
  Node *newNode = malloc(sizeof(Node));
  if (newNode == NULL) {
    fprintf(stderr, "NP_take:  out of memory\n");
    exit(1);
  }
  return newNode;
}


void NP_release(Node *node) {
  do {
    fprintf(stderr, "release(%p) tosp.ptr = %p, tosp.count = %" PRIu64 "\n", 
	    node, tosp.ptr, tosp.count);    
    node->next = tosp;
  } while( !cas128( (__int128 *)&tosp,
                       node->next.bits,
                       ((Pointer){node->next.ptr, node->next.count+1}).bits));
}




