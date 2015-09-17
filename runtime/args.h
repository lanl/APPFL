#ifndef args_h
#define args_h

#include <stdbool.h>

extern float GCThreshold; // fraction of total heap used before gc runs.

void parseArgs (int argc, char **argv);

#endif
