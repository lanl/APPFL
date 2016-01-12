#ifndef stg_h
#define stg_h
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "options.h"
#include "cmm.h"

struct _Obj;
typedef struct _Obj Obj;
struct _InfoTab;
typedef struct _InfoTab InfoTab;
struct _Cont;
typedef struct _Cont Cont;
struct _CInfoTab;
typedef struct _CInfoTab CInfoTab;

FnPtr stgCallCont();
extern CInfoTab it_stgCallCont;

FnPtr stgStackCont();
extern CInfoTab it_stgStackCont;

FnPtr stgUpdateCont();
extern CInfoTab it_stgUpdateCont;

FnPtr fun_stgShowResultCont();
extern CInfoTab it_stgShowResultCont;

// bitmap for specifying boxed/unboxed values
// assume 64 bits, high six bits for length,
// low bit is index 0,
// 0 => unboxed, 1 => boxed

typedef struct Bitmap64proto {
  uintptr_t mask : 58;
  unsigned int size : 6;
} Bitmap64proto;

typedef union Bitmap64 {
  uintptr_t bits;
  Bitmap64proto bitmap;
} Bitmap64;


#define BMSIZE(bm) (bm.bitmap.size)
#define BMMAP(bm) (bm.bitmap.mask)

//------ stack and heap objects

#if USE_ARGTYPE
typedef enum {          // superfluous, for sanity checking
  INT, 
  LONG,
  ULONG,
  FLOAT,
  DOUBLE,
  BITMAP,
  HEAPOBJ 
} ArgType;
#endif

// heap objects
typedef enum {
  OBJTYPE0BAD,
  FUN, 
  PAP, 
  CON,
  THUNK,
  BLACKHOLE,
  INDIRECT,
  PHONYENDOBJ
} ObjType;
const char *objTypeNames[PHONYENDOBJ];

  // stack continuations--change the names for compiler help finding them
typedef enum {
  BADCONTTYPE0,
  BADCONTTYPE1,
  BADCONTTYPE2,
  BADCONTTYPE3,
  BADCONTTYPE4,
  BADCONTTYPE5,
  BADCONTTYPE6,
  UPDCONT, 
  CASECONT, 
  CALLCONT,
  STACKCONT,
  FUNCONT,        // for strict evaluation
  PHONYENDCONT
} ContType;
const char *contTypeNames[PHONYENDCONT];

// PtrOrLiteral -- literal value or pointer to heap object 
typedef struct {
#if USE_ARGTYPE
  ArgType argType;        // superfluous, for sanity checking
#endif
  union {
    int64_t i;
    int64_t l;
    uint64_t u;
    float f;
    double d;
    Bitmap64 b;
    Obj *op;
  };
} PtrOrLiteral;

// STG registers
// %rbx, %rbp, %r10, %r13, %r14, %r15 callee saved
// TODO:  make heap, stack pointers registers, test performance
// TODO:  distinguish stgCurVal as stgCurPtr and stgCurUbx
#ifndef __clang__
register PtrOrLiteral stgCurVal asm("%r14");  // current/return value
#else
extern PtrOrLiteral stgCurVal;  // current/return value
#endif
/*
  payload -- see README
*/

struct _Obj {
  InfoTab *_infoPtr;         // canonical location of infoPtr--first word
#if USE_OBJTYPE
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
#endif
  char ident[32];           // temporary, just for tracing
  PtrOrLiteral payload[];
};

struct _Cont {
  CInfoTab *cInfoPtr;     // *going away* 
  CmmFnPtr entryCode;    // new
  ContType contType;
  Bitmap64 layout;        // new
  int _contSize;          // for debugging, should go away
  char ident[32];         // temporary, just for tracing
  PtrOrLiteral payload[];
};

// see README
typedef struct _LayoutInfo {
  int payloadSize;
  int boxedCount;
  int unboxedCount;
  char permString[64];  // this is just for e.g. displaying the heap
} LayoutInfo;

typedef struct {
  int arity;
  // curry paper suggests that we need type info
  CmmFnPtr trueEntryCode;
} FUNfields;

typedef struct {
  CmmFnPtr trueEntryCode;
} PAPfields;

typedef struct {
  int tag;
  int arity;
  char conName[32];
} CONfields;

typedef struct {
  //
} THUNKfields;

typedef struct {
  //
} UPDCONTfields;

typedef struct {
  //
} CASECONTfields;

typedef struct {
  //
} CALLCONTfields;

typedef struct {
  //
} FUNCONTfields;

#if DEBUG_INFOTAB
#define PI() (3.14159265358979323846)
#endif

// InfoTab
struct _InfoTab {
#if DEBUG_INFOTAB
  double pi;
#endif
  CmmFnPtr entryCode; 
  char name[32];  // for debugging
  ObjType objType; // kind of object, tag for union
  LayoutInfo layoutInfo;
  union {
    FUNfields funFields;
    PAPfields papFields;
    CONfields conFields;
    THUNKfields thunkFields;
  };
};

// CInfoTab
struct _CInfoTab {
  CmmFnPtr entryCode; 
  char name[32];  // for debugging
  ContType contType; // kind of continuation, tag for union
  LayoutInfo layoutInfo;
  union {
    UPDCONTfields updcontFields;
    CASECONTfields casecontFields;
    CALLCONTfields callcontFields;
    FUNCONTfields funcontFields;
  };
};

extern void *stgHeap, *stgHP;
extern void *toPtr, *fromPtr;
extern const size_t stgHeapSize;
extern const size_t stgStackSize;

extern void *stgStack, *stgSP;

extern size_t stgStatObjCount;
extern Obj *stgStatObj[];
extern void initStg();
extern void showStgObj(Obj *);
extern void showStgHeap();
extern void showStgStack();
extern void showStgVal(PtrOrLiteral);
extern void checkStgHeap();
extern void showIT(InfoTab *);
extern void showCIT(CInfoTab *);
extern int  getObjSize(Obj *);
extern int  getContSize(Cont *);

extern bool isSHO();
extern bool isHeap(Obj *p);
extern bool isFrom(void *p);
extern bool isTo(void *p);

extern bool isBoxed(PtrOrLiteral f);

extern bool isUnboxed(PtrOrLiteral f);

// use LSB to say it is a FORWARD
static inline InfoTab *setLSB(InfoTab *ptr) { 
  return (InfoTab *)((uintptr_t)ptr | 1); 
}
static inline InfoTab *unsetLSB(InfoTab *ptr) { 
  return (InfoTab *)((uintptr_t)ptr & ~1); 
}
static inline bool isLSBset(InfoTab *ptr) { 
  return (bool)((uintptr_t)ptr & 1); 
}

static inline InfoTab *setLSB2(InfoTab *ptr) { 
  return (InfoTab*)((uintptr_t)ptr | 2);
}

static inline bool isLSB2set(InfoTab *ptr) { 
  return (bool)((uintptr_t)ptr & 2); 
}

// for indirect
//static inline InfoTab *setLSB3(InfoTab *ptr) { 
//  return (InfoTab *)((uintptr_t)ptr | 4);
//}
//static inline bool isLSB3set(InfoTab *ptr) { return (bool)((uintptr_t)ptr & 4); }

static inline InfoTab *getInfoPtr(Obj *p)  { 
  InfoTab *itp = (InfoTab *)((((uintptr_t)(p->_infoPtr)) >> 3) << 3);
#if DEBUG_INFOTAB
  if (!isLSBset(p->_infoPtr))  // not forwarding
    assert(itp->pi == PI());
#endif
  return itp; 
}

static inline CInfoTab *getCInfoPtr(Cont *p)  { return p->cInfoPtr; }

static inline ObjType getObjType(Obj *p) {
  assert(!isLSBset(p->_infoPtr) && "getObjType on forwarding node");

  InfoTab *ip = getInfoPtr(p);

  ObjType iobjType; 
  switch (ip->objType) {
  case FUN:
    iobjType = isLSB2set(p->_infoPtr) ? PAP : FUN;
    break;
  case PAP:
    iobjType = PAP;
    break;
  case CON:
    iobjType = CON;
    break;
  case THUNK:
    iobjType = isLSB2set(p->_infoPtr) ? BLACKHOLE : THUNK;
    break;
  case BLACKHOLE:
    iobjType = BLACKHOLE;
    break;
  case INDIRECT:
    iobjType = INDIRECT;
    break;
  default:
    assert(false && "bad objType");
    break;
  }

#if USE_OBJTYPE
  ObjType objType = p->objType;
  if (objType != iobjType) {
    fprintf(stderr, "getting ObjType of %s aka %s, p->objType = %d, getInfoPtr(p)->objType = %d\n",
	    p->ident, ip->name, 
	    objType,
	    iobjType);
    assert(false);
  }
#endif

  return iobjType;
}

static inline ContType getContType(Cont *p) {
  return p->contType;
}

// allocate Obj on heap, returning pointer to new Obj
extern Obj* stgNewHeapObj(InfoTab *itp);
extern Obj* stgNewHeapPAP(InfoTab *itp, int pargc, int nargc);
extern Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bitmap);
// allocate Obj on continuation stack, returning pointer to new Obj
extern Cont *stgAllocStackCont(CInfoTab *it, int payloadSize);
extern Cont *stgAllocCallCont(CInfoTab *it, int payloadSize);
extern Cont *stgAllocCont(CInfoTab *it);
// remove Obj from top of continuation stack, returning pointer to de-alloced Obj
Cont *stgPopCont();
// get top of stack pointer, must be STACKCONT
Cont *stgGetStackArgp();
Cont *stgJumpAdjust();

extern void showStgObjPretty(Obj *p);
extern void showStgObjDebug(Obj *p);
extern void showStgValDebug(PtrOrLiteral v);
extern void showStgValPretty(PtrOrLiteral v);



#define STGCALL0(f)				\
  CALL0_0(f)

#define STGJUMP0(f)				\
  JUMP0(f)

// return through continuation stack

#define STGRETURN0()			\
  STGJUMP0(((Cont *)stgSP)->entryCode)

// objects take self through stgCurVal

#define DEFOBJworks(F,P1)				\
  FnPtr F() {					\
  PtrOrLiteral P1 = stgCurVal;

#define DEFOBJ(F)				\
  FnPtr F() {

// DEFUN#args(argnames)

#define DEFUN0(F)				\
  FnPtr F() {

#define DEFUN1(F,P1)				\
  FnPtr F() {					\
  PtrOrLiteral P1 = stgCurVal;

// new STACKCONT, for actual functions, not objects

#define DEFUNS0(F)				\
  FnPtr F() {					\
  stgPopCont();

#define DEFUNS1(F,P1)				\
  FnPtr F() {					\
  Cont *stg_sc = stgGetStackArgp();		\
  PtrOrLiteral P1;				\
  P1 = stg_sc->payload[0];				\
  stgPopCont();

#define DEFUNS2(F,P1,P2)			\
  FnPtr F() {					\
  Cont *stg_sc = stgGetStackArgp();		\
  PtrOrLiteral P1, P2;				\
  P1 = stg_sc->payload[0];				\
  P2 = stg_sc->payload[1];				\
  stgPopCont();


#define DEFUNS3(F,P1,P2,P3)			\
  FnPtr F() {					\
  Cont *stg_sc = stgGetStackArgp();		\
  PtrOrLiteral P1, P2, P3;			\
  P1 = stg_sc->payload[0];				\
  P2 = stg_sc->payload[1];				\
  P3 = stg_sc->payload[2];				\
  stgPopCont();


#define DEFUNS4(F,P1,P2,P3,P4)			\
  FnPtr F() {					\
  Cont *stg_sc = stgGetStackArgp();		\
  PtrOrLiteral P1, P2, P3, P4;			\
  P1 = stg_sc->payload[0];				\
  P2 = stg_sc->payload[1];				\
  P3 = stg_sc->payload[2];				\
  P4 = stg_sc->payload[3];				\
  stgPopCont();


#define DEFUNS5(F,P1,P2,P3,P4,P5)		\
  FnPtr F() {					\
  Cont *stg_sc = stgGetStackArgp();		\
  PtrOrLiteral P1, P2, P3, P4, P5;		\
  P1 = stg_sc->payload[0];				\
  P2 = stg_sc->payload[1];				\
  P3 = stg_sc->payload[2];				\
  P4 = stg_sc->payload[3];				\
  P5 = stg_sc->payload[4];				\
  stgPopCont();


#define ENDFUN						\
  fprintf(stderr, "ENDFUN should not be reached\n");	\
  assert(false);}

#endif  //ifdef stg_h
