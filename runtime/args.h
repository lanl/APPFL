#ifndef args_h
#define args_h

#include <stdbool.h>
#include "log.h"

extern float GCThreshold; // fraction of total heap used before gc runs.

typedef enum {
  LAZY,    // default
  STRICT1, // just function's arity args
  STRICT2, // all args in application
} EvalStrategy;

typedef struct Args {
  float gcThreshold;
  int evalStrategy;
  int constStrict;
  bool sanityChecker;
  int perfCounters;
  LogLevel loggingLevel;
} Args;

extern Args rtArg;

void parseArgs (int argc, char **argv);

#endif
