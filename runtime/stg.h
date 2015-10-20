#ifndef stg_h
#define stg_h
#include <stdint.h>
#include <stddef.h>
#include <assert.h>
#include <stdbool.h>
#include <stdint.h>
#include "options.h"

#define DEBUGSTGAPPLY 1

//------ stack and heap objects

typedef enum {          // superfluous, for sanity checking
  INT, 
  DOUBLE,
  BOOL,
  FLOAT,
  CHAR, 
  HEAPOBJ 
} ArgType;

typedef enum {
  // heap and stack objects
  OBJTYPE0BAD,
  FUN, 
  PAP, 
  CON,
  THUNK,
  BLACKHOLE,
  INDIRECT,
  // stack objects
  UPDCONT, 
  CASECONT, 
  CALLCONT, 
  FUNCONT,        // for strict evaluation
} ObjType;
const char *objTypeNames[FUNCONT+1];

struct _Obj;
struct _InfoTab;
typedef struct _Obj Obj;
typedef struct _InfoTab InfoTab;

//------ fake typedef fp (*fp)()
// how to roll these into one?
// seems like the only way to get a recursive type is to use a struct
typedef void (*vvfp)();
typedef vvfp (*FnPtr)();
typedef FnPtr (*CmmFnPtr)();

// PtrOrLiteral -- literal value or pointer to heap object 
typedef struct {
#if USE_ARGTYPE
  ArgType argType;        // superfluous, for sanity checking
#endif
  union {
    int64_t i;
    double d;
    float f;
    bool b;
    char c;
    Obj *op;
  };
} PtrOrLiteral;

// STG registers
extern PtrOrLiteral stgCurVal;  // current/return value

/*
  payload -- see README
*/

// stack or heap object
// in a proper implementation there will be no such struct declaration--
// they'll be variably-sized and self-describing


struct _Obj {
  uintptr_t infoPtr;         // canonical location of ObjType field
  int _objSize;              // for debugging
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  char ident[32];           // temporary, just for tracing
  PtrOrLiteral payload[];
};

// see README
typedef struct {
  int payloadSize;
  int boxedCount;
  int unboxedCount;
  char permString[64];  // this is just for e.g. displaying the heap
} LayoutInfo;

typedef struct {
  int arity;
  // curry paper suggests that we need type info
} FUNfields;

typedef struct {
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
  //  int fvCount;    // lexically determined, should be in layout
  CmmFnPtr entryCode; 
  ObjType objType; // kind of object, tag for union
  LayoutInfo layoutInfo;
  union {
    FUNfields funFields;
    PAPfields papFields;
    CONfields conFields;
    THUNKfields thunkFields;
    UPDCONTfields updcontFields;
    CASECONTfields casecontFields;
    CALLCONTfields callcontFields;
    FUNCONTfields funcontFields;
  };
};

extern void *stgHeap, *stgHP;
extern void *stgStack, *stgSP;
extern const size_t stgHeapSize;
extern const size_t stgStackSize;
extern size_t stgStatObjCount;
extern Obj * stgStatObj[];
extern void initStg();
extern void showStgObj(Obj *);
extern void showStgHeap();
extern void showStgStack();
extern void showStgVal(PtrOrLiteral);
extern void checkStgHeap();
extern void showIT(InfoTab *);
extern int getObjSize(Obj *);
extern bool isSHO();
extern bool isHeap(Obj *p);

#define PACKBITS (sizeof(uintptr_t)/2 * 8)
#define hibits (~0L << PACKBITS)
#define lobits (~0L & ~hibits)

#define PNPACK(pargc,nargc) ((pargc) | (((uintptr_t)(nargc)) << PACKBITS))

#define PUNPACK(n) (((uintptr_t) (n)) & lobits)
#define NUNPACK(n) (((uintptr_t) (n)) >> PACKBITS)

#define PNSIZE(n) (PUNPACK(n)+NUNPACK(n))

#define PNUNPACK(n,pargc,nargc) \
  do { \
    pargc = PUNPACK(n);	\
    nargc = NUNPACK(n);	\
  } while (0)

// bitmap for specifying boxed/unboxed values
// assume 64 bits, high six bits for length,
// low bit is index 0,
// 0 => unboxed, 1 => boxed
typedef uintptr_t Bitmap64;
#define BMSIZE(bm) ((bm>>58)&0x3F)
#define BMMAP(bm) (bm & 0x03FFFFFFFFFFFFFFLL) 

static inline InfoTab *getInfoPtr(Obj *p)  { return (InfoTab *)((p->infoPtr >> 3) << 3); }

// allocate Obj on heap, returning pointer to new Obj
extern Obj* stgNewHeapObj(InfoTab *itp);
extern Obj* stgNewHeapPAP(InfoTab *itp, int pargc, int nargc);
// allocate Obj on continuation stack, returning pointer to new Obj
extern Obj *stgAllocCallCont2(InfoTab *it, int payloadSize);
extern Obj *stgAllocCont(InfoTab *it);
// remove Obj from top of continuation stack, returning pointer to de-alloced Obj
Obj *stgPopCont();

# define STGCALL0(f)				\
  CALL0_0(f)

# define STGCALL1(f,v1)				\
  CALL1_0(f,v1)

# define STGCALL2(f,v1,v2)			\
  CALL2_0(f,v1,v2)

# define STGCALL3(f,v1,v2,v3)			\
  CALL3_0(f,v1,v2,v3)

# define STGCALL4(f,v1,v2,v3,v4)		\
  CALL4_0(f,v1,v2,v3,v4)

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

#define STGRETURN0()				\
  STGJUMP0(getInfoPtr((Obj *)stgSP)->entryCode)


// no explicit return value stack
#define STGRETURN1(r)				\
  do {						\
    stgCurVal = r;				\
    STGRETURN0();				\
  } while(0)

#define STGAPPLY1(f,v1)				\
  do {						\
    assert(getInfoPtr(f.op)->objType == FUN);	\
    assert(getInfoPtr(f.op)->funFields.arity == 1);\
    STGJUMP2(f.op->infoPtr->entryCode, f, v1);	\
  } while(0)

#define STGAPPLY2(f,v1,v2)				\
  do {							\
    assert(getInfoPtr(f.op)->objType == FUN);		\
    assert(getInfoPtr(f.op)->funFields.arity == 2);	\
    STGJUMP3(getInfoPtr(f.op)->entryCode, f, v1, v2);	\
  } while(0)

#define STGAPPLY3(f,v1,v2,v3)				\
  do {							\
    assert(getInfoPtr(f.op)->objType == FUN);		\
    assert(getInfoPtr(f.op)->funFields.arity == 3);	\
    STGJUMP4(getInfoPtr(f.op)->entryCode, f, v1, v2, v3);	\
  } while(0)

#define STGAPPLY4(f,v1,v2,v3,v4)				\
  do {								\
    assert(getInfoPtr(f.op)->objType == FUN);			\
    assert(getInfoPtr(f.op)->funFields.arity == 4);		\
    STGJUMP5(getInfoPtr(f.op)->entryCode, f, v1, v2, v3, v4);	\
  } while(0)

#define STGAPPLY5(f,v1,v2,v3,v4,v5)				\
  do {								\
    assert(getInfoPtr(f.op)->objType == FUN);			\
    assert(getInfoPtr(f.op)->funFields.arity == 5);		\
    STGJUMP6(getInfoPtr(f.op)->entryCode, f, v1, v2, v3, v4, v5);	\
  } while(0)

#define STGAPPLY6(f,v1,v2,v3,v4,v5,v6)					\
  do {									\
    assert(getInfoPtr(f.op)->objType == FUN);				\
    assert(getInfoPtr(f.op)->funFields.arity == 6);			\
    STGJUMP7(getInfoPtr(f.op)->entryCode, f, v1, v2, v3, v4, v5, v6);	\
  } while(0)

#define STGAPPLY7(f,v1,v2,v3,v4,v5,v6,v7)				\
  do {									\
    assert(getInfoPtr(f.op)->objType == FUN);				\
    assert(getInfoPtr(f.op)->funFields.arity == 7);			\
    STGJUMP8(getInfoPtr(f.op)->entryCode, f, v1, v2, v3, v4, v5, v6, v7);	\
  } while(0)

#endif  //ifdef stg_h
