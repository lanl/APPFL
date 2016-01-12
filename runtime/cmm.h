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

//------ fake typedef fp (*fp)()
// how to roll these into one?
// seems like the only way to get a recursive type is to use a struct
typedef void (*vvfp)();
typedef vvfp (*FnPtr)();
typedef FnPtr (*CmmFnPtr)();

#define CALL0_0(F)				\
  do {						\
    _CALL(F);					\
  } while (0)

#define RETURN0()				\
  do {						\
    _RETURN();					\
  } while (0)

#define JUMP0(F)				\
  do {						\
    _JUMP(F);					\
  } while (0)

/* ********** NON-USER STUFF! ********* */

// dispatch loop
extern void _cmmCall(CmmFnPtr f);

#define _RETURN() return NULL

#define _JUMP(f)  return ((FnPtr)f)

// this works so there's hope for tail calls
// define _JUMP(f)  return ((FnPtr)(f()))

#define _CALL(f)  for (CmmFnPtr _f = (CmmFnPtr)f; _f; _f = (CmmFnPtr)_f())

#endif //ifdef cmm_h
 
