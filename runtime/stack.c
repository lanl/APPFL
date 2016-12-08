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

#include "stack.h"
#include "stg.h"
#include "cmm.h"
#include "obj.h"
#include "log.h"
#include "args.h"

/*
Bitmap64 cLayoutInfoToBitmap64(CLayoutInfo *lip) {
  Bitmap64 bm;
  bm.bitmap.mask = (0x1UL << lip->boxedCount) - 1;  // boxed vals first
  bm.bitmap.size = lip->boxedCount + lip->unboxedCount;
  return bm;
}
*/

Cont *stgAllocCont(int stack, CInfoTab *citp) {
  assert(citp->contType != CALLCONT &&
         citp->contType != STACKCONT &&
         citp->contType != POPMECONT &&
          "stgAllocCont: citp->contType == CALLCONT/STACKCONT" );
  int payloadSize = citp->cLayoutInfo.payloadSize;
  size_t contSize = sizeof(Cont) + payloadSize * sizeof(PtrOrLiteral);
  if (USE_PERFCOUNTERS && rtArg.perfCounters) {
    perfCounter.stackBytesAllocated += contSize;
    perfCounter.stackAllocations++;
    perfCounter.stackHistogram[payloadSize]++;
  }
  LOG(LOG_DEBUG, "allocating %s continuation with payloadSize %d\n",
	  contTypeNames[citp->contType], payloadSize);
  showCIT(citp);
  stgSPs[stack] = (char *)stgSPs[stack] - contSize;
  assert(stgSPs[stack] >= stgStacks[stack]);
  if (USE_PERFCOUNTERS && rtArg.perfCounters > 1) {
    size_t before = stgStacks[stack] + stgStackSizes[stack] - stgSPs[stack];
    if (before > perfCounter.stackMaxSize) perfCounter.stackMaxSize = before;
  }

  Cont *contp = (Cont *)stgSPs[stack];
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
Cont *stgAllocCallOrStackCont(int stack, CInfoTab *citp, int argc) {
  assert((citp->contType == LETCONT ||
	  citp->contType == CALLCONT ||
	  citp->contType == STACKCONT) &&
	 "stgAllocCallOrStackCont: citp->contType != CALLCONT/STACKCONT");
  size_t contSize = sizeof(Cont) + argc * sizeof(PtrOrLiteral);
  
 if (USE_PERFCOUNTERS && rtArg.perfCounters) {
    perfCounter.stackBytesAllocated += contSize;
    perfCounter.stackAllocations++;
    perfCounter.stackHistogram[argc]++;
  }
  LOG(LOG_DEBUG,"allocating %s continuation with argc %d\n",
	  contTypeNames[citp->contType], argc);
  showCIT(citp);
  stgSPs[stack] = (char *)stgSPs[stack] - contSize;
  assert(stgSPs[stack] >= stgStacks[stack]);
 
  if (USE_PERFCOUNTERS && rtArg.perfCounters > 1) {
    size_t before = stgStacks[stack] + stgStackSizes[stack] - stgSPs[stack];
    if (before > perfCounter.stackMaxSize) perfCounter.stackMaxSize = before;
  }

  Cont *contp = (Cont *)stgSPs[stack];
  contp->cInfoPtr = citp;
  contp->_contSize = contSize;  // to go away

  contp->layout.bitmap.mask = 0x0UL;  // for GC
  contp->layout.bitmap.size = argc;   // make consistent

  contp->entryCode = citp->entryCode;
  contp->contType = citp->contType;
  strcpy(contp->ident, citp->name);  // may be overwritten
  memset(contp->payload, 0, argc * sizeof(PtrOrLiteral));
  return contp;
}


/*
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
*/

// delta is in units of sizeof(PtrOrLiteral)
// updates bitmap.size but not bitmap.mask
Cont *stgAdjustTopContSize(Cont *cp, int delta) {
  if (delta == 0) return cp;

  // we're really passing in the TOSP, important when there
  // are multiple stacks
  int tid = myThreadID();
  assert(stgGetStackArgp(tid) == cp);
  int oldContSize = getContSize(cp);
  int oldPayloadSize = cp->layout.bitmap.size;

  // adjust bitmap size
  int newPayloadSize = oldPayloadSize + delta;
  cp->layout.bitmap.size = newPayloadSize;

  // can't use getContSize because it does sanity check with _contSize
  int newContSize = sizeof(Cont) + newPayloadSize * sizeof(PtrOrLiteral);
  cp ->_contSize = newContSize; // to go away
  assert(newContSize == getContSize(cp));

  // move
  char *newStgSP = (char *)cp - (newContSize - oldContSize);
  if (newContSize < oldContSize)
    memmove(newStgSP, cp, newContSize); // shrinking
  else
    memmove(newStgSP, cp, oldContSize); // growing
  // just because
  cp = (Cont *)newStgSP;
  assert(newContSize == getContSize(cp));

  if (rtArg.perfCounters) {
    perfCounter.stackBytesAllocated += (newContSize - oldContSize);
    perfCounter.stackHistogram[oldPayloadSize]--;
    perfCounter.stackHistogram[newPayloadSize]++;
  }

  // adjust stgSP and return
  stgSPs[tid] = newStgSP;
  if (USE_PERFCOUNTERS && rtArg.perfCounters > 1) {
    size_t before = stgStacks[tid] + stgStackSizes[tid] - stgSPs[tid];
    if (before > perfCounter.stackMaxSize) perfCounter.stackMaxSize = before;
  }
  return stgSPs[tid];
}


void stgPopCont() {
  int tid = myThreadID();
  Cont *cp = (Cont *)stgSPs[tid];
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
  assert((char *)cp + contSize <= (char *)stgStacks[tid] + stgStackSizes[tid]);
  stgSPs[tid] = (char *)cp + contSize;
}

void stgPopContIfPopMe() {
  if (getContType(stgGetStackArgp(myThreadID())) == POPMECONT)
    stgPopCont();
}

// could return pointer to .payload but that thwarts a sanity check,
// though could perhaps pass in expected size--TODO: change name,
// should get args from any continuation
Cont *stgGetStackArgp(int stack) {
  assert(0 <= stack && stack < rtArg.nThreads && "stgGetStackArgp(int) bad stack");
  Cont *scp = (Cont *)stgSPs[stack];
  CInfoTab *citp = getCInfoPtr(scp);
  assert(citp->contType == getContType(scp));
  return scp;
}

void showCIT(CInfoTab *citp) {
   LOG(LOG_DEBUG,"showCIT: %s %s %lu",
	  contTypeNames[citp->contType],
	  citp->name,
	  citp->cLayoutInfo.bm.bits);

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
