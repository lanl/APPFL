/*
  Pass function pointers around as proper CMMVal types

  using the C call stack for calls (because there's no way to return to
  the calling point otherwise).  

  This is done by calling a proxy cmmCall, which makes the actual C call.
  Each Cmm function must return its return "kind" to cmmCall, i.e.
  whether it's a jump or return.  If a return, then cmmCall just returns.
  If a jump, cmmCall makes the call.  This allows the interleaving of
  returns and jumps to work properly.

*/

#ifndef cmm_h
#define cmm_h

#include <stdio.h>
#include <stdlib.h>

#define TRAMPOLINE 0

#if TRAMPOLINE
//------ fake typedef fp (*fp)() -- no recursive types in C
typedef void (*vvfp)();
typedef vvfp (*FnPtr)();
typedef FnPtr (*CmmFnPtr)();

#define CALL0_0(f)				\
  do {						\
    for (CmmFnPtr _f = (CmmFnPtr)f; _f; _f = (CmmFnPtr)_f());	\
  } while (0)

#define RETURN0()				\
  do {						\
    return NULL;				\
  } while (0)

#define JUMP0(f)				\
  do {						\
    return ((FnPtr)f);				\
  } while (0)

#else  // not trampoline

typedef void FnPtr;
typedef FnPtr (*CmmFnPtr)();

#define CALL0_0(f)				\
  do {						\
    (((CmmFnPtr)f)());				\
  } while (0)

#define JUMP0(f)				\
  do {						\
    (((CmmFnPtr)f)());				\
    return;					\
  } while (0)

#define RETURN0() return

#endif // if TRAMPOLINE



#endif //ifdef cmm_h
 
