#ifndef stack_h
#define stack_h

#include "stg.h"

  // stack continuations--change the names for compiler help finding them
typedef enum {
  BADCONTTYPE0,
  BADCONTTYPE1,
  BADCONTTYPE2,
  BADCONTTYPE3,
  BADCONTTYPE4,
  BADCONTTYPE5,
  PHONYSTARTCONT,
  UPDCONT,
  CASECONT,
  CALLCONT,
  STACKCONT,
  POPMECONT,
  LETCONT,
  PHONYENDCONT,
} ContType;
const char *contTypeNames[PHONYENDCONT];

struct _Cont {
  CInfoTab *cInfoPtr;     // *going away*
  CmmFnPtr entryCode;    // new
  ContType contType;
  Bitmap64 layout;        // new
  int _contSize;          // for debugging, should go away
  char ident[32];         // temporary, just for tracing
  PtrOrLiteral payload[];
};

typedef struct _CLayoutInfo {
  int payloadSize;
  int boxedCount;
  int unboxedCount;
  Bitmap64 bm;
  char permString[64];  // this is just for e.g. displaying the heap
} CLayoutInfo;

static inline CInfoTab *getCInfoPtr(Cont *p)  { return p->cInfoPtr; }

Bitmap64 cLayoutInfoToBitmap64(CLayoutInfo *lip);

static inline ContType getContType(Cont *p) {
  return p->contType;
}

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
} POPMECONTfields;

// CInfoTab
struct _CInfoTab {
  CmmFnPtr entryCode;
  char name[32];  // for debugging
  ContType contType; // kind of continuation, tag for union
  CLayoutInfo cLayoutInfo;
  /* not currently needed
  union {
    UPDCONTfields updcontFields;
    CASECONTfields casecontFields;
    CALLCONTfields callcontFields;
    POPMECONTfields popmecontFields;
  };
  */
};

// allocate Obj on continuation stack, returning pointer to new Obj
extern Cont *stgAllocCallOrStackCont(CInfoTab *it, int payloadSize);
extern Cont *stgAllocCont(CInfoTab *it);
// remove Obj from top of continuation stack, returning pointer to de-alloced Obj
void stgPopCont();
// get top of stack pointer, must be STACKCONT
Cont *stgGetStackArgp();
Cont *stgJumpAdjust();
Cont *stgAdjustTopContSize(Cont *cp, int delta);
int getContSize(Cont *);


#endif // ifndef stack_h
