#include <stdio.h>
#include <stdbool.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h> // for  mmap()
#include <fcntl.h>
#include <unistd.h> // for 
#include <stdio.h>
#include <errno.h>
#include <string.h>  // for memcpy()
#include <stdlib.h>  // for exit()
#ifndef __APPLE__
#include <malloc.h>  // for memalign()
#endif

#include "stg.h"
#include "cmm.h"
#include "obj.h"

void *stgHeap = NULL;
void *stgHP = NULL;
void *toPtr = NULL;
void *fromPtr = NULL;

void *stgStack = NULL;
void *stgSP = NULL;

#ifndef __clang__
// register PtrOrLiteral stgCurVal asm("%r15");  // current value STG register
#else
PtrOrLiteral stgCurVal;  // current/return value
#endif

const char *objTypeNames[] = {
  "OBJTYPE0BAD",
  "FUN", 
  "PAP", 
  "CON",
  "THUNK",
  "BLACKHOLE",
  "INDIRECT"
};

const char *contTypeNames[] = {
  "BADCONTTYPE0",
  "BADCONTTYPE1",
  "BADCONTTYPE2",
  "BADCONTTYPE3",
  "BADCONTTYPE4",
  "BADCONTTYPE5",
  "BADCONTTYPE6",
  "UPDCONT", 
  "CASECONT", 
  "CALLCONT", 
  "STACKCONT",
  "FUNCONT"
};


// this is a temporary hack as we incorporate Bitmap64s into continuations
Bitmap64 layoutInfoToBitmap64(LayoutInfo *lip) {
  Bitmap64 bm;
  bm.bitmap.mask = (0x1UL << lip->boxedCount) - 1;  // boxed vals first
  bm.bitmap.size = lip->boxedCount + lip->unboxedCount;
  return bm;
}

Cont *stgAllocCont(CInfoTab *citp) {
  assert(citp->contType != CALLCONT &&
          "stgAllocCont: citp->contType == CALLCONT" );
  int payloadSize = citp->layoutInfo.payloadSize;
  size_t contSize = sizeof(Cont) + payloadSize * sizeof(PtrOrLiteral);
  contSize = ((contSize + 7)/8)*8; 
  fprintf(stderr, "allocating continuation with payloadSize %d\n", payloadSize);
  showCIT(citp);
  stgSP = (char *)stgSP - contSize;
  assert(stgSP >= stgStack);
  Cont *contp = (Cont *)stgSP;
  contp->cInfoPtr = citp;
  contp->_contSize = contSize;  // to go away
  contp->layout = layoutInfoToBitmap64(&citp->layoutInfo); // temp hack
  contp->entryCode = citp->entryCode;
  contp->contType = citp->contType;
  strcpy(contp->ident, citp->name);  // may be overwritten
  return contp;
}

// CALLCONTs DON'T have a common InfoTab entries but .layoutInfo is invalid for all
Cont *stgAllocCallOrStackCont(CInfoTab *citp, int argc) {
  assert((citp->contType == CALLCONT || citp->contType == STACKCONT) && 
	 "stgAllocCallOrStackCont: citp->contType != CALLCONT/STACKCONT");
  size_t contSize = sizeof(Cont) + argc * sizeof(PtrOrLiteral);
  contSize = ((contSize + 7)/8)*8; 
  fprintf(stderr, "allocating %s continuation with argc %d\n", 
	  contTypeNames[citp->contType], argc);
  showCIT(citp);
  stgSP = (char *)stgSP - contSize;
  assert(stgSP >= stgStack);
  Cont *contp = (Cont *)stgSP;
  contp->cInfoPtr = citp;
  contp->_contSize = contSize;
  contp->entryCode = citp->entryCode;
  contp->contType = citp->contType;
  strcpy(contp->ident, citp->name);  // may be overwritten
  return contp;
}

// TODO: 64 bit align
// return pointer to popped cont
// NOTE:  stgAllocCont will invalidate this pointer!
// CALLCONT is special
Cont *stgPopCont() {
  Cont *retVal = (Cont *)stgSP;
  CInfoTab *citp = getCInfoPtr(retVal);
  assert(citp->contType == getContType(retVal));
  int payloadSize;
  switch (getContType(retVal)) {
  case UPDCONT:
  case CASECONT:
  case FUNCONT:
    fprintf(stderr, "popping other continuation with payloadSize ");
    payloadSize = getCInfoPtr(retVal)->layoutInfo.payloadSize;
    fprintf(stderr, "%d\n", payloadSize);
    break;
  case CALLCONT:
    fprintf(stderr, "popping CALLCONT with argc ");
    payloadSize = retVal->layout.bitmap.size;
    fprintf(stderr, "%d\n", payloadSize);
    break;
  case STACKCONT:
    fprintf(stderr, "popping STACKCONT with argc ");
    payloadSize = retVal->layout.bitmap.size;
    fprintf(stderr, "%d\n", payloadSize);
    break;
  default:
    fprintf(stderr, "bad continuation value %d\n", getContType(retVal));
    assert(false);
  }
  size_t contSize = sizeof(Cont) + payloadSize * sizeof(PtrOrLiteral);
  contSize = ((contSize + 7)/8)*8; 
  showCIT(getCInfoPtr(retVal));
  assert((char *)retVal + contSize <= (char *)stgStack + stgStackSize);
  stgSP = (char *)retVal + contSize;
  return retVal;
}

// could return pointer to .payload but that thwarts a sanity check,
// though could perhaps pass in expected size--TODO: change name,
// should get args from any continuation
Cont *stgGetStackArgp() {
  Cont *scp = (Cont *)stgSP;
  CInfoTab *citp = getCInfoPtr(scp);
  assert(citp->contType == getContType(scp));
  assert(getContType(scp) == STACKCONT || getContType(scp) == CALLCONT);
  return scp;
}

// top two elements of STG stack should be STACKCONTS
// overwrite penultimate with topmost
Cont *stgJumpAdjust() {
  fprintf(stderr, "ENTER stgJumpAdjust\n");
  Cont *scp = (Cont *)stgSP;
  assert(getContType(scp) == STACKCONT);
  size_t contSize = getContSize(scp);
  fprintf(stderr, "  top cont size is %lu\n", contSize);
  Cont *pscp = (Cont *)((char *)stgSP + contSize);
  assert(getContType(pscp) == STACKCONT);
  size_t pContSize = getContSize(pscp);
  fprintf(stderr, "  penultimate cont size is %lu\n", pContSize);
  stgSP += pContSize;
  memmove(stgSP, scp, contSize);
  fprintf(stderr, "EXIT stgJumpAdjust\n");
  return (Cont *) stgSP;
}



Obj* stgNewHeapObj(InfoTab *itp) {
  fprintf(stderr, "stgNewHeapObj: "); showIT(itp);
  int payloadSize = itp->layoutInfo.payloadSize;
  int fvs = itp->layoutInfo.boxedCount + itp->layoutInfo.unboxedCount;
  // assert(itp->fvCount == fvs);  // fvCount going away
  switch (itp->objType) {
  case FUN:
  case CON: assert(payloadSize == fvs && "stgNewHeapObj"); break;
  case THUNK: assert(payloadSize == fvs + 1 && "stgNewHeapObj"); break;
  default:  assert(false && "stgNewHeapObj");
  }
  size_t objSize = sizeof(Obj) + payloadSize * sizeof(PtrOrLiteral);
  objSize = ((objSize + 7)/8)*8; 
  Obj *objp = (Obj *)stgHP;
  stgHP = (char *)stgHP + objSize;
  memset(objp, 0, objSize); //zero out anything left by previous gc passes
  objp->_infoPtr = itp;
  strcpy(objp->ident, itp->name);  // may be overwritten
#if USE_OBJTYPE
  fprintf(stderr, "stgNewHeapObj setting %s objType %s\n", 
	  objp->ident, objTypeNames[itp->objType]);
  objp->objType = itp->objType;
#endif
  fprintf(stderr, "stgNewHeapObj: "); showStgObj(objp);
  return objp;
}

Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bm) {
  assert(!(((uintptr_t)itp) & 0x7));
  assert(itp->objType == FUN && "stgNewHeapPAPmask:  itp->objType != FUN" );
  int fvs = itp->layoutInfo.boxedCount + itp->layoutInfo.unboxedCount;
  // assert(itp->fvCount == fvs);      // fvCount going away
  assert(itp->layoutInfo.payloadSize == fvs);  // FUN
  fprintf(stderr, "stgNewHeapPap: "); showIT(itp);
  size_t objSize = sizeof(Obj) + 
    (fvs + bm.bitmap.size + 1) * sizeof(PtrOrLiteral);
  objSize = ((objSize + 7)/8)*8;
  Obj *objp = (Obj *)stgHP;
  stgHP = (char *)stgHP + objSize;
  memset(objp, 0, objSize); //zero out anything left by previous gc passes
#if USE_ARGTYPE
  objp->payload[fvs].argType = LONG;
#endif
  objp->payload[fvs].b = bm;
  strcpy(objp->ident, itp->name);  // may be overwritten
  objp->_infoPtr = itp;
  objp->_infoPtr = setLSB2(itp); // set InfoPtr bit to say this is a PAP
#if USE_OBJTYPE
  objp->objType = PAP;
#endif
  fprintf(stderr, "stgNewHeapPAP: "); showStgObj(objp);
  return objp;
}

int getObjSize(Obj *o) {
  size_t objSize;
  ObjType type = getObjType(o);
  switch (type) {
  case PAP: {
    InfoTab *itp = getInfoPtr(o);
    int fvs = itp->layoutInfo.boxedCount + itp->layoutInfo.unboxedCount;
    objSize = sizeof(Obj) + 
      (fvs + 1 + o->payload[fvs].b.bitmap.size) * sizeof(PtrOrLiteral);
    objSize = ((objSize + 7)/8)*8;
    break;
  } // PAP
  case FUN:
  case CON:
  case THUNK:
  case BLACKHOLE:
  case INDIRECT: {
    InfoTab *itp = getInfoPtr(o);
    /* THUNK payload size is one larger, this is a wart, this check should be in GC
    if(itp->layoutInfo.boxedCount + itp->layoutInfo.unboxedCount !=
       itp->layoutInfo.payloadSize) {
      fprintf(stderr, "%s bc %d, ubc %d, pls %d\n", objTypeNames[type],
	      itp->layoutInfo.boxedCount, itp->layoutInfo.unboxedCount,
	      itp->layoutInfo.payloadSize);
      assert(false);
    }
    */
    objSize = sizeof(Obj) + 
              itp->layoutInfo.payloadSize * sizeof(PtrOrLiteral);
    break;
  }
  default:
    fprintf(stderr, "stg.c/getObjSize bad ObjType %d\n", type);
    assert(false);
    break;
  }
  objSize = ((objSize + 7)/8)*8;
  return objSize;
}



void showStgObj(Obj *p) {
  showStgObjPretty(p);
  // showStgObjDebug(Obj *p)
}
void showStgVal(PtrOrLiteral v) {
  showStgValPretty(v);
  // showStgValDebug(v);
}

void showIT(InfoTab *itp) {
  fprintf(stderr, "showIT: %s %s, bc %d ubc %d layoutInfo.payloadSize %d\n", 
	  objTypeNames[itp->objType], 
	  itp->name, 
	  itp->layoutInfo.boxedCount,
          itp->layoutInfo.unboxedCount, 
	  itp->layoutInfo.payloadSize);
}  

void showCIT(CInfoTab *citp) {
  fprintf(stderr, "showCIT: %s %s, bc %d ubc %d", 
	  contTypeNames[citp->contType], 
	  citp->name, 
	  citp->layoutInfo.boxedCount,
          citp->layoutInfo.unboxedCount);
  if (citp->contType != CALLCONT)
    fprintf(stderr, ", layoutInfo.payloadSize %d", citp->layoutInfo.payloadSize);
  fprintf(stderr, "\n");
}  

int getContSize(Cont *o) {
  size_t contSize;
  ContType type = getContType(o);
  switch (type) {
  case CALLCONT:
  case UPDCONT:
  case CASECONT:
  case FUNCONT:
  case STACKCONT:
    contSize = sizeof(Cont) + o->layout.bitmap.size * sizeof(PtrOrLiteral);
    break;
  default:
    fprintf(stderr, "stg.c/getContSize bad ContType %d\n", type);
    assert(false);
  }
  contSize = ((contSize + 7)/8)*8;
  if (contSize != o->_contSize) {
    fprintf(stderr, "contSize is %lu, o->_contSize is %d for %s\n",
	    contSize, o->_contSize, contTypeNames[type]);
    assert(contSize == o->_contSize && "bad contSize");
  }
  return contSize;
}

const size_t stgHeapSize  = (size_t)4*(size_t)(1024*1024*1024);
const size_t stgStackSize  = (size_t)4*(size_t)(1024*1024*1024);

void initStg() {
  stgHeap =
    mmap( NULL,                   // void *address, NULL => no preference
	  stgHeapSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON, // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgHeap == MAP_FAILED) {
    fprintf(stderr,"mmap stg heap failed!!\n"); 
    exit(1);
  }

  stgHP = stgHeap; // first free address

  stgStack = 
    mmap( NULL,                   // void *address, NULL => no preference
	  stgStackSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON,  // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgStack == MAP_FAILED) {
    fprintf(stderr,"mmap stg stack failed!!\n"); 
    exit(1);
  }
  
  stgSP = (char *)stgStack + stgStackSize;

  fprintf(stderr,"Stg stack at %p and heap at %p\n", stgStack, stgHP);

  stgStatObjCount = 0;

}

