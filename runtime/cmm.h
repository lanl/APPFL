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
    _CALL(F);					\
  } while (0)

#define CALL1_0(F,P1)				\
  do {						\
    _PUSHVALS1(P1);				\
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

#define JUMP1(F,V1)				\
  do {						\
    _PUSHVALS1(V1);				\
    _JUMP(F);					\
  } while (0)

// objects take self through stgCurVal

#define DEFOBJworks(F,P1)				\
  FnPtr F() {					\
  PtrOrLiteral P1 = stgCurVal;

#define DEFOBJ(F)				\
  FnPtr F() {

// DEFUN#args(argnames)

#define DEFUN0(F)				\
  FnPtr F() {

#define DEFUN1works(F,P1)				\
  FnPtr F() {					\
  PtrOrLiteral P1;				\
  _POPVALS1(P1);

#define DEFUN1(F,P1)				\
  FnPtr F() {					\
  PtrOrLiteral P1 = stgCurVal;

#define DEFUN2(F,P1,P2)				\
  FnPtr F() {					\
  PtrOrLiteral P1, P2;				\
  _POPVALS2(P1,P2);					

// new STACKCONT, for actual functions, not objects

#define DEFUNS0(F)				\
  FnPtr F() {					\
  Cont *stg_cp = stgPopCont();

#define DEFUNS1(F,P1)				\
  FnPtr F() {					\
  Cont *stg_cp = stgPopCont();			\
  PtrOrLiteral P1;				\
  P1 = stg_cp->payload[0];

#define DEFUNS2(F,P1,P2)			\
  FnPtr F() {					\
  Cont *stg_cp = stgPopCont();			\
  PtrOrLiteral P1, P2;				\
  P1 = stg_cp->payload[0];			\
  P2 = stg_cp->payload[1];


#define DEFUNS3(F,P1,P2,P3)			\
  FnPtr F() {					\
  Cont *stg_cp = stgPopCont();			\
  PtrOrLiteral P1, P2, P3;			\
  P1 = stg_cp->payload[0];			\
  P2 = stg_cp->payload[1];			\
  P3 = stg_cp->payload[2];


#define DEFUNS4(F,P1,P2,P3,P4)			\
  FnPtr F() {					\
  Cont *stg_cp = stgPopCont();			\
  PtrOrLiteral P1, P2, P3, P4;			\
  P1 = stg_cp->payload[0];			\
  P2 = stg_cp->payload[1];			\
  P3 = stg_cp->payload[2];			\
  P4 = stg_cp->payload[3];


#define DEFUNS5(F,P1,P2,P3,P4,P5)		\
  FnPtr F() {					\
  Cont *stg_cp = stgPopCont();			\
  PtrOrLiteral P1, P2, P3, P4, P5;		\
  P1 = stg_cp->payload[0];			\
  P2 = stg_cp->payload[1];			\
  P3 = stg_cp->payload[2];			\
  P4 = stg_cp->payload[3];			\
  P5 = stg_cp->payload[4];


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
/*
inline void _PUSH(PtrOrLiteral V) {
  fprintf(stderr, "_PUSH() ");			
  showStgVal(V);   fprintf(stderr, "\n");
  cmmSP = ((char *)cmmSP) - sizeof(PtrOrLiteral);
  assert(cmmSP >= cmmStack);
  *((PtrOrLiteral *)cmmSP) = V;
}
*/
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

#define _POPVALS1(L1)				\
  do {						\
    _POP(L1);					\
  } while (0)

#define _POPVALS2(L1,L2)			\
  do {						\
    _POP(L1);				\
    _POP(L2);				\
  } while (0)

// PUSHVALSN(P1,...,PN), N >= 0

#define _PUSHVALS1(L1)				\
  do {						\
      _PUSH(L1);				\
  } while (0)

#endif //ifdef cmm_h
