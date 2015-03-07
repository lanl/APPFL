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

extern void initPredefs();

// every manifest heap object is introduced by a "let" so has
// a name.  However, for a CON(C,...) we can use the same InfoTab
// entry for each manifest occurrence, so we'll use the constructor
// name rather than uniqued version of name of variable bound to
// Also true for known functions?

extern FnPtr stgShowResultCont();
extern Obj sho_stgShowResultCont;

//extern FnPtr stgCallCont();
//extern Obj sho_stgCallCont;

extern Obj sho_main_unit;
extern Obj sho_main1;
extern Obj sho_main2;
extern Obj sho_mainfail;
extern Obj sho_main3;

extern FnPtr program();

extern void initInfoTabs();

#endif
