#include "threading.h"
#include <stdio.h>
#include <stdlib.h>

#if USE_ARGOBOTS
#include "abt.h"
void threadingInit(int argc, char *argv[]) {
  assert(ABT_init(argc, argv) == ABT_SUCCESS);

  intptr_t x = 0;
  assert(ABT_self_set_arg((void *)x) == ABT_SUCCESS);
}

void threadingFinalize() {
  assert(ABT_finalize() == ABT_SUCCESS);
}
#elif USE_PTHREADS
#error "USE_PTHREADS not implemented"
#else
void threadingInit(int argc, char *argv[]) {}
#endif // use threading library
  
