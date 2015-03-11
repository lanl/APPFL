#ifndef predefs_h
#define predefs_h

#include "stg.h"

// these could overlap--make distinct for debugging purposes now

typedef enum {
  TagUnit  = 0,
  TagFalse = 1,
  TagTrue  = 2,
  TagI     = 3,
  TagNil   = 4,
  TagCons  = 5,
  TagLeft  = 6,
  TagRight = 7
} TagVal;


// every manifest heap object is introduced by a "let" so has
// a name.  However, for a CON(C,...) we can use the same InfoTab
// entry for each manifest occurrence, so we'll use the constructor
// name rather than uniqued version of name of variable bound to
// Also true for known functions?

extern void initPredefs();
extern FnPtr start();
extern void initInfoTabs();

#endif
