#ifndef args_h
#define args_h

#include <stdbool.h>

extern float GCThreshold; // fraction of total heap used before gc runs.

typedef enum {
  LAZY, // default
  STRICT1,
} EvalStrategy;

extern int evalStrategy;

void parseArgs (int argc, char **argv);

#endif
