#ifndef heap_h
#define heap_h

#include "stg.h"

// heap objects
typedef enum {
  PHONYSTARTOBJ,
  FUN,
  PAP,
  CON,
  THUNK,
  BLACKHOLE,
  INDIRECT,
  PHONYENDOBJ,
} ObjType;
extern const char *objTypeNames[PHONYENDOBJ];

//--------- InfoTab

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

bool isSHO();
bool isHeap(Obj *p);
bool isFrom(void *p);
bool isTo(void *p);

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

//--------- Obj

// with empty payload sizeof(Obj) must be multiple of 4
struct _Obj {
  InfoTab *_infoPtr;         // canonical location of infoPtr--first word
#if USE_OBJTYPE
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
#endif
  char ident[32];           // temporary, just for tracing
  PtrOrLiteral payload[];
};

Obj *derefHO(Obj *op);
Obj *derefPoL(PtrOrLiteral f);
void derefStgCurVal();

Obj* stgNewHeapObj(InfoTab *itp);
Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bm);

int  getObjSize(Obj *);

static inline InfoTab *getInfoPtr(Obj *p)  {
  InfoTab *itp = (InfoTab *)((((uintptr_t)(p->_infoPtr)) >> 3) << 3);
#if DEBUG_INFOTAB
  if (!isLSBset(p->_infoPtr))  // not forwarding
    assert(itp->pi == PI());
#endif
  return itp;
}

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

#endif
