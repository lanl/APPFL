#ifndef threading_h
#define threading_h

#include "options.h"
#include "args.h"
#include "assert.h"

void threadingInit(int argc, char *argv[]);
void threadingFinalize();

// the preprocessor nests are expected to grow as needed
// OS/compiler/thread library

#if USE_ARGOBOTS
#include "abt.h"
static inline int myThreadID() {
  // need indexes [0..Nthreads), make ULT arg its thread ID
  void *ret;
  assert (ABT_self_get_arg(&ret) == ABT_SUCCESS);
  int tid = (intptr_t)ret;
  assert (tid >= 0 && tid < rtArg.nThreads);
  return tid;
}
#elif USE_PTHREADS
static inline int myThreadID() {
  // need indexes [0..NTHREADS), use TLS
#error "thread library not implemented"
}
#endif

#endif // threading_h
