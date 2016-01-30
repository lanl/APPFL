/* options used by both C and haskell side for conditional compilation */
#ifndef options_h
#define options_h

#define OBJ_ALIGN 8
#define OBJ_ALIGNM1 7

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

/* if 1 verbose debug info from gc() */
#define DEBUG_GC 1

/* if 1 extra gc() checks */
#define EXTRA_CHECKS_GC 1 

/* if 1 initiate GC with every object allocation */
#define ALLOC_GC 0

/* ghc cpp doesn't like varargs */
/* define PRINTF(...) */
/* define PRINTF(...) fprintf (stderr, __VA_ARGS__) */
#if __STDC__
#define PRINTF(...) fprintf (stderr, __VA_ARGS__)
#endif

#endif
