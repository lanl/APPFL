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
#include "log.h"

void *stgHeap = NULL;
void *stgHP = NULL;
void *toPtr = NULL;
void *fromPtr = NULL;

void *stgStack = NULL;
void *stgSP = NULL;

#if !defined(__clang__) && !USE_ARGTYPE
// register PtrOrLiteral stgCurVal asm("%r15");  // current value STG register
#else
PtrOrLiteral stgCurVal;  // current/return value
PtrOrLiteral stgCurValU;  // current/return value
#endif

const char *objTypeNames[] = {
  "PHONYSTARTOBJ",
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
  "PHONYSTARTCONT",
  "UPDCONT",
  "CASECONT",
  "CALLCONT",
  "STACKCONT",
  "POPMECONT",
  "LETCONT",
};

void startCheck() {
  if (sizeof(Obj) % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "sizeof(Obj) is %lu not multiple of %d\n",
                    sizeof(Obj), OBJ_ALIGN);
    exit(1);
  }
}

void copyargs(PtrOrLiteral *dest, PtrOrLiteral *src, int count) {
  memcpy(dest, src, count * sizeof(PtrOrLiteral));
}

// this is a temporary hack as we incorporate Bitmap64s into continuations
Bitmap64 layoutInfoToBitmap64(LayoutInfo *lip) {
  Bitmap64 bm;
  bm.bitmap.mask = (0x1UL << lip->boxedCount) - 1;  // boxed vals first
  bm.bitmap.size = lip->boxedCount + lip->unboxedCount;
  return bm;
}

Bitmap64 cLayoutInfoToBitmap64(CLayoutInfo *lip) {
  Bitmap64 bm;
  bm.bitmap.mask = (0x1UL << lip->boxedCount) - 1;  // boxed vals first
  bm.bitmap.size = lip->boxedCount + lip->unboxedCount;
  return bm;
}

Cont *stgAllocCont(CInfoTab *citp) {
  assert(citp->contType != CALLCONT &&
         citp->contType != STACKCONT &&
         citp->contType != POPMECONT &&
          "stgAllocCont: citp->contType == CALLCONT/STACKCONT" );
  int payloadSize = citp->cLayoutInfo.payloadSize;
  size_t contSize = sizeof(Cont) + payloadSize * sizeof(PtrOrLiteral);
  //  contSize = ((contSize + 7)/8)*8;
  LOG(LOG_DEBUG, "allocating %s continuation with payloadSize %d\n",
	  contTypeNames[citp->contType], payloadSize);
  showCIT(citp);
  stgSP = (char *)stgSP - contSize;
  assert(stgSP >= stgStack);
  Cont *contp = (Cont *)stgSP;
  contp->cInfoPtr = citp;
  contp->_contSize = contSize;  // to go away

  // TODO:  this is bad for GC
  contp->layout = citp->cLayoutInfo.bm;

  contp->entryCode = citp->entryCode;
  contp->contType = citp->contType;
  strcpy(contp->ident, citp->name);  // may be overwritten
  return contp;
}

// CALL/STACK CONTs DON'T have common InfoTab entries, .layoutInfo is invalid for all
Cont *stgAllocCallOrStackCont(CInfoTab *citp, int argc) {
  assert((citp->contType == LETCONT ||
	  citp->contType == CALLCONT ||
	  citp->contType == STACKCONT) &&
	 "stgAllocCallOrStackCont: citp->contType != CALLCONT/STACKCONT");
  size_t contSize = sizeof(Cont) + argc * sizeof(PtrOrLiteral);
  //  contSize = ((contSize + 7)/8)*8;
  LOG(LOG_DEBUG,"allocating %s continuation with argc %d\n",
	  contTypeNames[citp->contType], argc);
  showCIT(citp);
  stgSP = (char *)stgSP - contSize;
  assert(stgSP >= stgStack);
  Cont *contp = (Cont *)stgSP;
  contp->cInfoPtr = citp;
  contp->_contSize = contSize;  // to go away

  contp->layout.bitmap.mask = 0x0UL;  // for GC
  contp->layout.bitmap.size = argc;   // make consistent

  contp->entryCode = citp->entryCode;
  contp->contType = citp->contType;
  strcpy(contp->ident, citp->name);  // may be overwritten
  return contp;
}

// not used in favor of StgAdjustTopContSize
// top two elements of STG stack should be STACKCONTS
// overwrite penultimate with topmost
Cont *stgJumpAdjust() {
  LOG(LOG_DEBUG, "ENTER stgJumpAdjust\n");
  Cont *scp = (Cont *)stgSP;
  assert(getContType(scp) == STACKCONT);
  size_t contSize = getContSize(scp);
  LOG(LOG_DEBUG, "  top cont size is %lu\n", contSize);
  Cont *pscp = (Cont *)((char *)stgSP + contSize);
  assert(getContType(pscp) == STACKCONT);
  size_t pContSize = getContSize(pscp);
  LOG(LOG_DEBUG, "  penultimate cont size is %lu\n", pContSize);
  stgSP += pContSize;
  memmove(stgSP, scp, contSize);
  LOG(LOG_DEBUG, "EXIT stgJumpAdjust\n");
  return (Cont *) stgSP;
}



// delta is in units of sizeof(PtrOrLiteral)
// updates bitmap.size but not bitmap.mask
Cont *stgAdjustTopContSize(Cont *cp, int delta) {

  // we're really passing in the TOSP, important when there
  // are multiple stacks
  assert(stgGetStackArgp() == cp);

  // adjust bitmap size
  int oldPayloadSize = cp->layout.bitmap.size;
  int newPayloadSize = oldPayloadSize + delta;
  cp->layout.bitmap.size = newPayloadSize;

  // calculate actual cont sizes
  int oldContSize = sizeof(Cont) + oldPayloadSize * sizeof(PtrOrLiteral);
  //      oldContSize = ((oldContSize + 7)/8)*8;
  int newContSize = sizeof(Cont) + newPayloadSize * sizeof(PtrOrLiteral);
  //      newContSize = ((newContSize + 7)/8)*8;

  if (newContSize == oldContSize)
    return cp;

  cp ->_contSize = newContSize; // to go away
  // move
  char *newStgSP = (char *)stgSP - (newContSize - oldContSize);
  if (newContSize < oldContSize)
    memmove(newStgSP, stgSP, newContSize); // shrinking
  else
    memmove(newStgSP, stgSP, oldContSize); // growing

  // adjust stgSP and return
  stgSP = newStgSP;
  return stgSP;
}
  
void stgPopCont() {
  Cont *cp = (Cont *)stgSP;
  CInfoTab *citp = getCInfoPtr(cp);
  assert(citp->contType == getContType(cp));
  int contType = getContType(cp);
  assert(contType > PHONYSTARTCONT &&
	 contType < PHONYENDCONT &&
	 "bad cont type");
  int payloadSize = cp->layout.bitmap.size;
  LOG(LOG_DEBUG, "popping %s continuation with payloadSize %d\n",
	  contTypeNames[contType], payloadSize);
  size_t contSize = sizeof(Cont) + payloadSize * sizeof(PtrOrLiteral);
  //  contSize = ((contSize + 7)/8)*8;
  showCIT(getCInfoPtr(cp));
  assert((char *)cp + contSize <= (char *)stgStack + stgStackSize);
  stgSP = (char *)cp + contSize;
}

// could return pointer to .payload but that thwarts a sanity check,
// though could perhaps pass in expected size--TODO: change name,
// should get args from any continuation
Cont *stgGetStackArgp() {
  Cont *scp = (Cont *)stgSP;
  CInfoTab *citp = getCInfoPtr(scp);
  assert(citp->contType == getContType(scp));
  return scp;
}

void showIT(InfoTab *itp) {
    LOG(LOG_DEBUG, "showIT: %s %s, bc %d ubc %d layoutInfo.payloadSize %d\n",
	  objTypeNames[itp->objType],
	  itp->name,
	  itp->layoutInfo.boxedCount,
          itp->layoutInfo.unboxedCount,
	  itp->layoutInfo.payloadSize);
}

void showCIT(CInfoTab *citp) {
   LOG(LOG_DEBUG,"showCIT: %s %s, bc %d ubc %d",
	  contTypeNames[citp->contType],
	  citp->name,
	  citp->cLayoutInfo.boxedCount,
          citp->cLayoutInfo.unboxedCount);
  if (citp->contType != CALLCONT) {
    LOG(LOG_DEBUG, ", layoutInfo.payloadSize %d", citp->cLayoutInfo.payloadSize);
  }
  LOG(LOG_DEBUG, "\n");
}

int getContSize(Cont *o) {
  size_t contSize;
  ContType type = getContType(o);
  switch (type) {
  case CALLCONT:
  case UPDCONT:
  case CASECONT:
  case STACKCONT:
  case POPMECONT:
  case LETCONT:
    contSize = sizeof(Cont) + o->layout.bitmap.size * sizeof(PtrOrLiteral);
    break;
  default:
    LOG(LOG_FATAL, "stg.c/getContSize bad ContType %d\n", type);
    assert(false);
  }
  //  contSize = ((contSize + 7)/8)*8;
  if (contSize != o->_contSize) {
    LOG(LOG_FATAL,"contSize is %lu, o->_contSize is %d for %s\n",
	    contSize, o->_contSize, contTypeNames[type]);
    assert(contSize == o->_contSize && "bad contSize");
  }
  return contSize;
}

const size_t stgHeapSize  = (size_t)(1024*1024*1024);
const size_t stgStackSize  = (size_t)(1024*1024*1024);

void initStg() {
  stgHeap =
    mmap( NULL,                   // void *address, NULL => no preference
	  stgHeapSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON, // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgHeap == MAP_FAILED) {
    LOG(LOG_FATAL, "mmap stg heap failed!!\n");
    exit(1);
  }

  if ((uintptr_t)stgHP % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "stgHP not OBJ_ALIGNed\n");
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
    LOG(LOG_FATAL, "mmap stg stack failed!!\n");
    exit(1);
  }
  
  stgSP = (char *)stgStack + stgStackSize;

  LOG(LOG_INFO, "Stg stack at %p and heap at %p\n", stgStack, stgHP);

  stgStatObjCount = 0;

}
