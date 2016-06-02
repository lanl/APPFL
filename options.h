#ifndef options_h
#define options_h

/* options used by both C and haskell side for conditional compilation */

/* 0/1, If 1 use argType enum field in PtrOrLiteral */
#define USE_ARGTYPE 1

/* 0/1, If 1 use objType enum field in Obj */
#define USE_OBJTYPE 1

/* If 1 use .pi = PI header in InfoTab */
#define DEBUG_INFOTAB 0


/* C side options */

#define OBJ_ALIGN 8
#define OBJ_ALIGNM1 7

/* log level defined in log.h */
#define LOG_LEVEL 7

/* if 1 extra gc() checks */
#define EXTRA_CHECKS_GC 1

/* if 1 initiate GC with every object allocation */
#define ALLOC_GC 1

/* number of threads, temporary solution */

#define NTHREADS 2

/* if 1, sanity checker enabled */
#define HEAP_SANITY_CHECK 0

#endif
