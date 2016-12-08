#include "nodepool.h"
#include <bool.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

Pointer tosp;
size_t size;

void NP_init(size_t _size) {
  tosp.ptr = NULL; 
  tosp.count = 0;
  size = _size;
  fprintf(stderr, "NodePool() &tosp = %p tosp.ptr = %p, tosp.count = %lu\n", 
	  &tosp,
	  tosp.ptr, 
	  tosp.count);
}


Node *NP_take() {
  Pointer current = tosp;
  while (current.ptr) {
    if ( cas128 >(&tosp, 
		  current, 
		  (Pointer){current.ptr->next.ptr, 
			    current.count+1} ) )
      return current.ptr;
  }
  // no nodes in pool, create new one, Pointer value doesn't matter
  Node *newNode = malloc(sizeOf(Node));
  if (newNode == NULL) {
    fprintf(stderr, "NP_take:  out of memory\n");
    exit(1);
  }
  return newNode();
}


void release(Node *node) {
  do {
    fprintf(stderr, "release(%p) tosp.ptr = %p, tosp.count = %lu\n", 
	    node, tosp.ptr, tosp.count);    
    node->next = tosp;
    bool success = !cas128( (__int128 *)&tosp, 
			    node->next, 
			    (Pointer){node->next.ptr, 
				node->next.count+1});
  } while( !success );
}




