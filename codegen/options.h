/* options used by both C and haskell side for conditional compilation */
#ifndef options_h
#define options_h

/* If 1 use argType enum field in PtrOrLiteral */
#define USE_ARGTYPE 1

/* If 1 use objType enum field in Obj */
#define USE_OBJTYPE 1

/* If 1 generate C AST rather than text */
#define USE_CAST 0 

/* If 1 verbose debug info from stgApply functions */
#define DEBUGSTGAPPLY 1

/* If 1 use .pi = PI header in InfoTab */
#define DEBUG_INFOTAB 1

#endif
