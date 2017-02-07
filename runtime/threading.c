#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "threading.h"
#include "nodequeue.h"
#include "stg.h"
#include "log.h"

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
      (ABT_xstream *)malloc(sizeof(ABT_xstream) * (rtArg.nThreads+1));

  // we are the main ES and ULT, stream already exists
  assert(ABT_xstream_self(&xstreams[0]) == ABT_SUCCESS);

  for (int i = 1; i != rtArg.nThreads+1; i++) {
    assert(ABT_xstream_create(ABT_SCHED_NULL, // round robin?
                &xstreams[i]) == ABT_SUCCESS);
  }

  // populate pools, one per total number of streams
  pools = (ABT_pool *)malloc(sizeof(ABT_pool) * (rtArg.nThreads+1));

  // get the pools in which to allocate threads
  for (int i = 0; i != rtArg.nThreads+1; i++) {
    assert(ABT_xstream_get_main_pools(xstreams[i], 
                    1, // just the first such pool
                    &pools[i]) 
       == ABT_SUCCESS);
  }

  nextID = 0;
  assert(ABT_self_set_arg((void *)nextID) == ABT_SUCCESS);
  nextID = 1;

  NP_init(1000);
  NQ_init();

  LOG(LOG_DEBUG, "threadingInit start service thread\n");
  ABT_thread thread;
  ABT_thread_create(pools[nextID], serviceQueue, (void *)nextID, ABT_THREAD_ATTR_NULL, &thread);
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

// really dumb service queue thread
void serviceQueue(void *p) {
  struct timespec polling;
  polling.tv_sec = 0;
  polling.tv_nsec = 1000;
 
  LOG(LOG_DEBUG, "in serviceQueue ID=%ld\n", nextID);

  while(1) {
    unsigned long f;
    if(NQ_dequeue(&f)) {
      LOG(LOG_DEBUG,"dequeue %lu\n",f);
      //don't actually call function yet 
      //(((CmmFnPtr)f)());
    } 
    nanosleep(&polling, NULL);
  }
}


#elif USE_PTHREADS
#error "USE_PTHREADS not implemented"
#else
void threadingInit(int argc, char *argv[]) {}
#endif // use threading library
  
