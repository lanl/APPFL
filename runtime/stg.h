#ifndef stg_h
#define stg_h
#include <stdint.h>
#include <stddef.h>
#include <assert.h>

//------ stack and heap objects

typedef enum {          // superfluous, for sanity checking
  INT, 
  DOUBLE, 
  HEAPOBJ 
} ArgType;

typedef enum {
  // heap objects
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
  FUNCONT        // for strict evaluation
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

// PtrOrLiteral -- pointer to heap object or literal value
typedef struct {
  ArgType argType;        // superfluous, for sanity checking
  union {
    int i;
    double d;
    Obj *op;
  };
} PtrOrLiteral;

/*
  payload
    FUN - free variables
    PAP - free variables, initial arguments
    CON - arguments (includes free variables)
    THUNK - free variables
    BLACKHOLE - n/a
    //
    INDIRECT - pointer to next node in chain
    //
    UPDCONT  - pointer to HO to update
    CASECONT - free variables
    CALLCONT - in theory arguments (includes free variables), in practice???
    FUNCONT  - the function (?) and args (not yet evaluated?)
*/

// stack or heap object
// in a proper implementation there will be no such struct declaration--
// they'll be variably-sized and self-describing

struct _Obj {
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  int argCount;             // for PAP, how many args already applied to?
  InfoTab *infoPtr;         // canonical location of ObjType field
  PtrOrLiteral payload[16]; // fixed for now
};

typedef struct {
  //
} LayoutInfo;

typedef struct {
  int arity;
  // curry paper suggests that we need type info
} FUNfields;

typedef struct {
  int tag;
  int argCount;  // change this to "arity"
} CONfields;

typedef struct {
} PAPfields;

typedef struct {
  //
} THUNKfields;

// InfoTab
struct _InfoTab {
  char name[32];  // for debugging
  int fvCount;    // lexically determined
  CmmFnPtr entryCode;
  ObjType objType; // kind of object, tag for union
  LayoutInfo layout;
  union {
    FUNfields funFields;
    PAPfields papFields;
    CONfields conFields;
    THUNKfields thunkFields;
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
extern Obj* stgNewHeapObj();
extern void showStgStack();
extern void showStgVal(PtrOrLiteral);

inline void stgPushCont(Obj c) {
  stgSP = (char *)stgSP - sizeof(Obj);
  assert(stgSP >= stgStack);
  *(Obj *)stgSP = c;
}

inline Obj stgPopCont() {
  assert((char *)stgSP + sizeof(Obj) <= (char *) stgStack + stgStackSize);
  Obj o = *(Obj *)stgSP;
  stgSP = (char *)stgSP + sizeof(Obj);
  return o;
}

# define STGCALL0(f)				\
  CALL0_0(f)

# define STGCALL1(f,v1)				\
  CALL1_0(f,v1)

# define STGCALL2(f,v1,v2)			\
  CALL2_0(f,v1,v2)

#define STGJUMP0(f)				\
  JUMP0(f)

#define STGJUMP1(f,v1)				\
  JUMP1(f,v1)

#define STGJUMP2(f,v1,v2)			\
  JUMP2(f,v1,v2)

// return through continuation stack
#define STGRETURN0()				\
  JUMP0(((Obj *)stgSP)->infoPtr->entryCode)

// no explicit return value stack
#define STGRETURN1(r)				\
  do {						\
    stgCurVal = r;				\
    JUMP0(((Obj *)stgSP)->infoPtr->entryCode);	\
  } while(0)





/*
define STGRETURN1(o)				\
  do {						\
  JUMP1(((Obj *)stgSP)->infoPtr->entryCode, o) \
  } while (0)
*/

// STG registers
extern PtrOrLiteral stgCurVal;  // current/return value

#endif  //ifdef stg_h
