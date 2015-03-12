#ifndef predefs_h
#define predefs_h

#include "stg.h"


// every manifest heap object is introduced by a "let" so has
// a name.  However, for a CON(C,...) we can use the same InfoTab
// entry for each manifest occurrence, so we'll use the constructor
// name rather than uniqued version of name of variable bound to
// Also true for known functions?

extern void initPredefs();
extern FnPtr start();

#endif
