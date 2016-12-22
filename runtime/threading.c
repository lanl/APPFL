#include "threading.h"
#include <stdio.h>
#include <stdlib.h>

#if USE_ARGOBOTS

static ABT_xstream *xstreams;
static ABT_pool *pools;
static intptr_t nextID;

#include "abt.h"
void threadingInit(int argc, char *argv[]) {
  // can only have one instance 
  assert(ABT_initialized() == ABT_ERR_UNINITIALIZED);

  assert(ABT_init(argc, argv) == ABT_SUCCESS);

  xstreams = 
      (ABT_xstream *)malloc(sizeof(ABT_xstream) * rtArg.nThreads);

  // we are the main ES and ULT, stream already exists
  assert(ABT_xstream_self(&xstreams[0]) == ABT_SUCCESS);

  for (int i = 1; i != rtArg.nThreads; i++) {
    assert(ABT_xstream_create(ABT_SCHED_NULL, // round robin?
                &xstreams[i]) == ABT_SUCCESS);
  }

  // populate pools, one per total number of streams
  pools = (ABT_pool *)malloc(sizeof(ABT_pool) * rtArg.nThreads);

  // get the pools in which to allocate threads
  for (int i = 0; i != rtArg.nThreads; i++) {
    assert(ABT_xstream_get_main_pools(xstreams[i], 
                    1, // just the first such pool
                    &pools[i]) 
       == ABT_SUCCESS);
  }

  nextID = 0;
  assert(ABT_self_set_arg((void *)nextID) == ABT_SUCCESS);
  nextID = 1;
}

void threadingFinalize() {
  assert(ABT_finalize() == ABT_SUCCESS);
}

void threadingPush(FuncPtr func) {
   assert(ABT_thread_create(pools[nextID],
                 func,
                 (void *)nextID,
                 ABT_THREAD_ATTR_NULL,
                 NULL)
       == ABT_SUCCESS);
    nextID = (nextID + 1) % (rtArg.nThreads + 1);
}

void threadingYield() {
  assert(ABT_thread_yield() == ABT_SUCCESS);
}

#elif USE_PTHREADS
#error "USE_PTHREADS not implemented"
#else
void threadingInit(int argc, char *argv[]) {}
#endif // use threading library
  
