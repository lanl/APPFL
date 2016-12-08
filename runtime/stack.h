#ifndef stack_h
#define stack_h

#include "stg.h"

struct _Cont;
typedef struct _Cont Cont;
//struct _CInfoTab;
//typedef struct _CInfoTab CInfoTab;

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

/* not currently needed
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
*/

typedef struct _CLayoutInfo {
  int payloadSize;
  //int boxedCount;
  //int unboxedCount;
  Bitmap64 bm;
  char permString[64];  // this is just for e.g. displaying the heap
} CLayoutInfo;

struct _Cont {
  CInfoTab *cInfoPtr;     // *going away*
  CmmFnPtr entryCode;    // new
  ContType contType;
  Bitmap64 layout;        // new
  int _contSize;          // for debugging, should go away
#if USE_IDENT  
  char ident[32];         // temporary, just for tracing
#endif
  PtrOrLiteral payload[];
};

// CInfoTab
struct _CInfoTab {
  CmmFnPtr entryCode;
#if USE_INFOTAB_NAME
  char name[32];  // for debugging
#endif
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

static inline CInfoTab *getCInfoPtr(Cont *p)  { return p->cInfoPtr; }

Bitmap64 cLayoutInfoToBitmap64(CLayoutInfo *lip);

static inline ContType getContType(Cont *p) {
  return p->contType;
}

int  getContSize(Cont *);

void stgPopContIfPopMe();

// allocate Obj on continuation stack, returning pointer to new Obj
extern Cont *stgAllocCont(int stack, CInfoTab *it);
extern Cont *stgAllocCallOrStackCont(int stack, CInfoTab *it, int payloadSize);
// remove Obj from top of continuation stack, returning pointer to de-alloced Obj
void stgPopCont();
// get top of stack pointer, must be STACKCONT
Cont *stgGetStackArgp(int stack);
Cont *stgJumpAdjust();
Cont *stgAdjustTopContSize(Cont *cp, int delta);

void showCIT(CInfoTab *);

#endif // ifndef stack_h
