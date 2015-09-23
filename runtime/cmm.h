/*
  Pass function pointers around as proper CMMVal types

  using the C call stack for calls (because there's no way to return to
  the calling point otherwise).  

  This is done by calling a proxy cmmCall, which makes the actual C call.
  Each Cmm function must return its return "kind" to cmmCall, i.e.
  whether it's a jump or return.  If a return, then cmmCall just returns.
  If a jump, cmmCall makes the call.  This allows the interleaving of
  returns and jumps to work properly.

  using explicit Cmm stack for arg and return values.

  calling convention:
    push args, right to left
    do C call (via cmmCall)
    pop results

  return:
    pop args
    push results, right to left
    return NULL to cmmCall

  jump:
    pop args
    push args, right to left
    return destfp to cmmCall
*/

#ifndef cmm_h
#define cmm_h

#include <stdio.h>
#include <stdlib.h>
#include "stg.h"

extern void *cmmStack, *cmmSP;
extern void initCmm();

// CMM types

// CMM stack item tag -- for debugging purposes only

// N*M of these

#define CALL0_0(F)				\
  do {						\
    _PUSHVALS0();				\
    _CALL(F);					\
    _POPVALS0();				\
  } while (0)

#define CALL1_0(F,P1)				\
  do {						\
    _PUSHVALS1(P1);				\
    _CALL(F);					\
    _POPVALS0();				\
  } while (0)

#define CALL0_1(R1,F)				\
  do {						\
    _PUSHVALS0();				\
    _CALL(F);					\
    _POPVALS1(R1);				\
  } while (0)

#define CALL2_0(F,P1,P2)			\
  do {						\
    _PUSHVALS2(P1,P2);				\
    _CALL(F);					\
    _POPVALS0();				\
  } while (0)


#define CALL3_0(F,P1,P2,P3)			\
  do {						\
    _PUSHVALS3(P1,P2,P3);			\
    _CALL(F);					\
    _POPVALS0();				\
  } while (0)


#define CALL4_0(F,P1,P2,P3,P4)			\
  do {						\
    _PUSHVALS4(P1,P2,P3,P4);			\
    _CALL(F);					\
    _POPVALS0();				\
  } while (0)


#define RETURN0()				\
  do {						\
    _RETURN();					\
  } while (0)

#define RETURN1(R1)				\
  do {						\
    stgCurVal = R1;				\
    _RETURN();					\
  } while (0)

#define JUMP0(F)				\
  do {						\
    _PUSHVALS0();				\
    _JUMP(F);					\
  } while (0)

#define JUMP1(F,V1)				\
  do {						\
    _PUSHVALS1(V1);				\
    _JUMP(F);					\
  } while (0)

#define JUMP2(F,V1,V2)				\
  do {						\
    _PUSHVALS2(V1,V2);				\
    _JUMP(F);					\
  } while (0)

#define JUMP3(F,V1,V2,V3)			\
  do {						\
    _PUSHVALS3(V1,V2,V3);			\
    _JUMP(F);					\
  } while (0)

#define JUMP4(F,V1,V2,V3,V4)			\
  do {						\
    _PUSHVALS4(V1,V2,V3,V4);			\
    _JUMP(F);					\
  } while (0)

#define JUMP5(F,V1,V2,V3,V4,V5)			\
  do {						\
    _PUSHVALS5(V1,V2,V3,V4,V5);			\
    _JUMP(F);					\
  } while (0)

#define JUMP6(F,V1,V2,V3,V4,V5,V6)		\
  do {						\
    _PUSHVALS6(V1,V2,V3,V4,V5,V6);		\
    _JUMP(F);					\
  } while (0)

#define JUMP7(F,V1,V2,V3,V4,V5,V6,V7)		\
  do {						\
    _PUSHVALS7(V1,V2,V3,V4,V5,V6,V7);		\
    _JUMP(F);					\
  } while (0)

#define JUMP8(F,V1,V2,V3,V4,V5,V6,V7,V8)	\
  do {						\
    _PUSHVALS8(V1,V2,V3,V4,V5,V6,V7,V8);	\
    _JUMP(F);					\
  } while (0)

// DEFUN#args(argnames)

#define DEFUN0(F)				\
  FnPtr F() {					\
  _POPVALS0();					

#define DEFUN1(F,P1)				\
  FnPtr F() {					\
  PtrOrLiteral P1;				\
  _POPVALS1(P1);

#define DEFUN2(F,P1,P2)				\
  FnPtr F() {					\
  PtrOrLiteral P1, P2;				\
  _POPVALS2(P1,P2);					

#define DEFUN3(F,P1,P2,P3)			\
  FnPtr F() {					\
  PtrOrLiteral P1, P2, P3;			\
  _POPVALS3(P1,P2,P3);					

#define DEFUN4(F,P1,P2,P3,P4)			\
  FnPtr F() {					\
  PtrOrLiteral P1, P2, P3, P4;			\
  _POPVALS4(P1,P2,P3,P4);					

#define DEFUN5(F,P1,P2,P3,P4,P5)		\
  FnPtr F() {					\
  PtrOrLiteral P1, P2, P3, P4, P5;		\
  _POPVALS5(P1,P2,P3,P4,P5);					

#define ENDFUN return NULL;}

/* ********** NON-USER STUFF! ********* */
// 
// these need to be auto-generated!
// another approach would be to generate MN CALL macros (or functions)
// M results, N args
// CALL32(r1,r2,f,p1,p2,p3)
//
// This all isn't the most efficient, since accessing args via the stack
// would be faster.  For hand-coding experiments, however, this will be
// less error prone.

// these _MACRO are for use by other macros, not generated code


// dispatch loop
extern void _cmmCall(CmmFnPtr f);

extern const size_t cmmStackSize;

inline void _PUSH(PtrOrLiteral V) {
  fprintf(stderr, "_PUSH() ");			
  showStgVal(V);   fprintf(stderr, "\n");
  cmmSP = ((char *)cmmSP) - sizeof(PtrOrLiteral);
  assert(cmmSP >= cmmStack);
  *((PtrOrLiteral *)cmmSP) = V;
}

/*
inline PtrOrLiteral _POP() {
  assert((char *)cmmSP + sizeof(PtrOrLiteral) <= (char *)cmmStack + cmmStackSize);
  PtrOrLiteral v = *((PtrOrLiteral *)cmmSP);
  cmmSP = (char *)cmmSP + sizeof(PtrOrLiteral);
  return v;
}
*/

#define _POP(V)								\
  do {									\
    fprintf(stderr, "_POP() ");						\
    assert((char *)cmmSP + sizeof(PtrOrLiteral) <=			\
	   (char *)cmmStack + cmmStackSize);				\
    PtrOrLiteral v = *((PtrOrLiteral *)cmmSP);				\
    showStgVal(v); fprintf(stderr, "\n");				\
    cmmSP = (char *)cmmSP + sizeof(PtrOrLiteral);			\
    V = v;								\
  } while (0)

#define _RETURN() return NULL

#define _JUMP(f)  return ((FnPtr)f)

// this works so there's hope for tail calls
// define _JUMP(f)  return ((FnPtr)(f()))

#define _CALL(f)  for (CmmFnPtr _f = (CmmFnPtr)f; _f; _f = (CmmFnPtr)_f())

// POPVALSN(P1,...,PN), N >= 0

#define _POPVALS0()				\
  do {						\
  } while (0)

#define _POPVALS1(L1)				\
  do {						\
    _POP(L1);					\
  } while (0)

#define _POPVALS2(L1,L2)			\
  do {						\
    _POP(L1);				\
    _POP(L2);				\
  } while (0)

#define _POPVALS3(L1,L2,L3)			\
  do {						\
    _POP(L1);					\
    _POP(L2);					\
    _POP(L3);				\
  } while (0)

#define _POPVALS4(L1,L2,L3,L4)			\
  do {						\
    _POP(L1);				\
    _POP(L2);				\
    _POP(L3);				\
    _POP(L4);				\
  } while (0)

#define _POPVALS5(L1,L2,L3,L4,L5)	\
  do {					\
    _POP(L1);				\
    _POP(L2);				\
    _POP(L3);				\
    _POP(L4);				\
    _POP(L5);				\
  } while (0)

// PUSHVALSN(P1,...,PN), N >= 0

#define _PUSHVALS0()				\
  do {						\
  } while (0)

#define _PUSHVALS1(L1)				\
  do {						\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS2(L1,L2)			\
  do {						\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS3(L1,L2,L3)			\
  do {						\
      _PUSH(L3);				\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS4(L1,L2,L3,L4)			\
  do {						\
      _PUSH(L4);				\
      _PUSH(L3);				\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS5(L1,L2,L3,L4,L5)		\
  do {						\
      _PUSH(L5);				\
      _PUSH(L4);				\
      _PUSH(L3);				\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS6(L1,L2,L3,L4,L5,L6)		\
  do {						\
      _PUSH(L6);				\
      _PUSH(L5);				\
      _PUSH(L4);				\
      _PUSH(L3);				\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS7(L1,L2,L3,L4,L5,L6,L7)	\
  do {						\
      _PUSH(L7);				\
      _PUSH(L6);				\
      _PUSH(L5);				\
      _PUSH(L4);				\
      _PUSH(L3);				\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#define _PUSHVALS8(L1,L2,L3,L4,L5,L6,L7,L8)	\
  do {						\
      _PUSH(L8);				\
      _PUSH(L7);				\
      _PUSH(L6);				\
      _PUSH(L5);				\
      _PUSH(L4);				\
      _PUSH(L3);				\
      _PUSH(L2);				\
      _PUSH(L1);				\
  } while (0)

#endif //ifdef cmm_h
