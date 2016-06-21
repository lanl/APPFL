#ifndef options_h
#define options_h

/* options used by both C and haskell side for conditional compilation */

/* Controls syntax of case expressions and allows main in ConvertSTG
   to convert old STG files to new syntax */
#ifndef REWRITE_STG
#define REWRITE_STG 0
#endif


/* 0/1, If 1 use argType enum field in PtrOrLiteral */
#ifndef DEF_ARGTYPE  /* used by make multi */
#define USE_ARGTYPE 1
#endif

/* 0/1, If 1 use objType enum field in Obj */
#ifndef DEF_OBJTYPE  /* used by make multi */
#define USE_OBJTYPE 1
#endif

/* If 1 use .ident in Obj/Cont */
#define USE_IDENT 1 

/* If 1 use .name in Infotab */
#define USE_INFOTAB_NAME 1 

/* If 1 use .pi = PI header in InfoTab */
#define DEBUG_INFOTAB 0

/* align for objects/infotabs */
#define OBJ_ALIGN 8

/* C side options */

#define OBJ_ALIGNM1 7

#if DEBUG_INFOTAB
#define PI() (3.14159265358979323846)
#endif

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
