#ifndef stg_h
#define stg_h
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include "options.h"

//------ fake typedef fp (*fp)()
// how to roll these into one?
// seems like the only way to get a recursive type is to use a struct
typedef void (*vvfp)();
typedef vvfp (*FnPtr)();
typedef FnPtr (*CmmFnPtr)();

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


// InfoTab
struct _InfoTab {
  char name[32];  // for debugging
  CmmFnPtr entryCode; 
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
  char name[32];  // for debugging
  CmmFnPtr entryCode; 
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
extern Obj * stgStatObj[];
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


static inline InfoTab *getInfoPtr(Obj *p)  { 
  return (InfoTab *)((((uintptr_t)(p->_infoPtr)) >> 3) << 3); 
}

static inline CInfoTab *getCInfoPtr(Cont *p)  { return p->cInfoPtr; }
  /*
static inline InfoTab *setLSB2(InfoTab *ptr) { 
  return (InfoTab*)((uintptr_t)ptr | 2);
}

static inline bool isLSB2set(InfoTab *ptr) { 
  return (bool)((uintptr_t)ptr & 2); 
}
  */
// for indirect
//static inline InfoTab *setLSB3(InfoTab *ptr) { 
//  return (InfoTab *)((uintptr_t)ptr | 4);
//}
//static inline bool isLSB3set(InfoTab *ptr) { return (bool)((uintptr_t)ptr & 4); }


static inline ObjType getObjType(Obj *p) {
  ObjType objType, iobjType;
  InfoTab *ip = getInfoPtr(p);
  bool okay;
  iobjType = ip->objType;
  objType = p->objType;
  switch(objType) {
  case FUN:
    okay = (iobjType == FUN);
    break;
  case PAP:
    okay = (iobjType == FUN);
    break;
  case CON:
    okay = (iobjType == CON);
    break;
  case THUNK:
    okay = (iobjType == THUNK);
    break;
  case BLACKHOLE:
    okay = (iobjType == THUNK);
    break;
  case INDIRECT:
    okay = (iobjType == INDIRECT);
    break;
  default:
    assert(false && "bad objType");
    break;
  }
  if (!okay) {
    fprintf(stderr, "getting ObjType of %s aka %s, p->objType = %s, getInfoPtr(p)->objType = %s\n",
	    p->ident, ip->name, 
	    objTypeNames[objType],
	    objTypeNames[iobjType]);
    //     assert(false);
  }

  /*
  switch(ip->objType) {
  case FUN:
    objType = (isLSB2set(p->infoPtr) ? PAP : FUN);
    break;
  case THUNK:
    objType = (isLSB2set(p->infoPtr) ? BLACKHOLE : THUNK);
    break;
  default:
    objType = ip->objType;
  }
#if USE_OBJTYPE
  if (objType != p->objType) {
    fprintf(stderr, "computed objType = %s but p->objType = %s for %s aka %s\n",
	    objTypeNames[objType], objTypeNames[p->objType], p->ident, ip->name);
    assert(objType == p->objType);
  }
#endif
  */
#if USE_OBJTYPE
  objType = p->objType;
#else
  assert(false);
#endif

  return objType;

}

static inline ContType getContType(Cont *p) {
  return p->contType;
}

/*
static inline ContType getContType(Cont *p) {
#if USE_OBJTYPE
  return p->contType;
#else
  CInfoTab *cInfoPtr = getCInfoPtr(p);
  return cInfoPtr->contType;
#endif
}
*/

// allocate Obj on heap, returning pointer to new Obj
extern Obj* stgNewHeapObj(InfoTab *itp);
extern Obj* stgNewHeapPAP(InfoTab *itp, int pargc, int nargc);
extern Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bitmap);
// allocate Obj on continuation stack, returning pointer to new Obj
extern Cont *stgAllocCallCont(CInfoTab *it, int payloadSize);
extern Cont *stgAllocCont(CInfoTab *it);
// remove Obj from top of continuation stack, returning pointer to de-alloced Obj
Cont *stgPopCont();

extern void showStgObjPretty(Obj *p);
extern void showStgObjDebug(Obj *p);
extern void showStgValDebug(PtrOrLiteral v);
extern void showStgValPretty(PtrOrLiteral v);



# define STGCALL0(f)				\
  CALL0_0(f)

# define STGCALL1(f,v1)				\
  CALL1_0(f,v1)

#define STGJUMP0(f)				\
  JUMP0(f)

#define STGJUMP1(f,v1)				\
  JUMP1(f,v1)

#define STGJUMP2(f,v1,v2)			\
  JUMP2(f,v1,v2)

#define STGJUMP3(f,v1,v2,v3)			\
  JUMP3(f,v1,v2,v3)

#define STGJUMP4(f,v1,v2,v3,v4)			\
  JUMP4(f,v1,v2,v3,v4)

#define STGJUMP5(f,v1,v2,v3,v4,v5)		\
  JUMP5(f,v1,v2,v3,v4,v5)

#define STGJUMP6(f,v1,v2,v3,v4,v5,v6)		\
  JUMP6(f,v1,v2,v3,v4,v5,v6)

#define STGJUMP7(f,v1,v2,v3,v4,v5,v6,v7)	\
  JUMP7(f,v1,v2,v3,v4,v5,v6,v7)

#define STGJUMP8(f,v1,v2,v3,v4,v5,v6,v7,v8)	\
  JUMP8(f,v1,v2,v3,v4,v5,v6,v7,v8)


// return through continuation stack

#define STGRETURN0()			\
  STGJUMP0(((Cont *)stgSP)->entryCode)


// no explicit return value stack
#define STGRETURN1(r)				\
  do {						\
    stgCurVal = r;				\
    STGRETURN0();				\
  } while(0)

#endif  //ifdef stg_h
