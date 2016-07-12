#include "stg.h"
#include "heap.h"
#include "gc.h"
#include <string.h>  // for memcpy()

// this is a temporary hack as we incorporate Bitmap64s into continuations
/*
Bitmap64 layoutInfoToBitmap64(LayoutInfo *lip) {
  Bitmap64 bm;
  bm.bitmap.mask = (0x1UL << lip->boxedCount) - 1;  // boxed vals first
  bm.bitmap.size = lip->boxedCount + lip->unboxedCount;
  return bm;
}
*/

// we can't be certain a value is boxed or unboxed without enabling ARG_TYPE
// but we can do some sanity checking.  mayBeBoxed(v) means that v is not
// definitely unboxed
bool mayBeBoxed(PtrOrLiteral f) {
  #if USE_ARGTYPE
  return f.argType == HEAPOBJ &&
         (f.op == NULL || isHeap(f.op) || isSHO(f.op)) &&
         ((f.u & (OBJ_ALIGN - 1)) == 0);
#else
  return (f.op == NULL || isHeap(f.op) || isSHO(f.op)) &&
         ((f.u & (OBJ_ALIGN - 1)) == 0);
#endif
}

bool mayBeUnboxed(PtrOrLiteral f) {
  #if USE_ARGTYPE
  return f.argType != HEAPOBJ;
#else
  return true;
#endif
}

Obj *derefPoL(PtrOrLiteral f) {
  assert(mayBeBoxed(f) && "derefPoL: not a HEAPOBJ");
  return derefHO(f.op);
}

void derefStgCurVal() {
  assert(mayBeBoxed(stgCurVal)); // return stgCurVal
  while (getObjType(stgCurVal.op) == INDIRECT) { // return stgCurVal
    stgCurVal = stgCurVal.op->payload[0]; // return stgCurVal
    assert(mayBeBoxed(stgCurVal)); // return stgCurVal
  }
}

Obj *derefHO(Obj *op) {
  while (getObjType(op) == INDIRECT) {
    PtrOrLiteral v = op->payload[0];
    assert(mayBeBoxed(v));
    op = v.op;
  }
  return op;
}

Obj* stgNewHeapObj(InfoTab *itp) {
  LOG(LOG_DEBUG, "stgNewHeapObj: "); showIT(itp);
#if ALLOC_GC
  LOG(LOG_DEBUG, "******** stgNewHeapObj before GC\n");
  gc();
  LOG(LOG_DEBUG, "******** stgNewHeapObj after GC\n");
#endif
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
  objSize = ((objSize + OBJ_ALIGNM1)/OBJ_ALIGN)*OBJ_ALIGN;
  Obj *objp = (Obj *)stgHP;
  stgHP = (char *)stgHP + objSize;

  // this memset is important for garbage collection with LET construction
  memset(objp->payload, 0, payloadSize * sizeof(PtrOrLiteral));
#if USE_ARGTYPE
  int boxed = itp->layoutInfo.boxedCount + (itp->objType==THUNK ? 1 : 0);
  for (int i = 0; i != boxed; i++) objp->payload[i].argType = HEAPOBJ;
#endif

  objp->_infoPtr = itp;
  strcpy(objp->ident, itp->name);  // may be overwritten
#if USE_OBJTYPE
  LOG(LOG_DEBUG, "stgNewHeapObj setting %s objType %s\n",
	  objp->ident, objTypeNames[itp->objType]);
  objp->objType = itp->objType;
#endif
  //  Can't display it--payload values not set, fix showStgObj
  //  LOG(LOG_INFO,"stgNewHeapObj: "); showStgObj(objp);
  return objp;
}

Obj* stgNewHeapPAPmask(InfoTab *itp, Bitmap64 bm) {
#if ALLOC_GC
  LOG(LOG_DEBUG,"******** stgNewHeapPAPmask before GC\n");
  gc();
  LOG(LOG_DEBUG,"******** stgNewHeapPAPmask after GC\n");
#endif
  assert(!(((uintptr_t)itp) & 0x7));
  assert(itp->objType == FUN && "stgNewHeapPAPmask:  itp->objType != FUN" );
  int fvCount = itp->layoutInfo.boxedCount +
                itp->layoutInfo.unboxedCount;
  // assert(itp->fvCount == fvCount);      // fvCount going away
  assert(itp->layoutInfo.payloadSize == fvCount);  // FUN
  LOG(LOG_DEBUG, "stgNewHeapPap: "); showIT(itp);
  size_t objSize = sizeof(Obj) +
    (fvCount + bm.bitmap.size + 1) * sizeof(PtrOrLiteral);
  objSize = ((objSize + OBJ_ALIGNM1)/OBJ_ALIGN)*OBJ_ALIGN;
  Obj *objp = (Obj *)stgHP;
  stgHP = (char *)stgHP + objSize;
#if USE_ARGTYPE
  objp->payload[fvCount].argType = BITMAP;
#endif
  objp->payload[fvCount].b = bm;
  strcpy(objp->ident, itp->name);  // may be overwritten
  objp->_infoPtr = setLSB2(itp); // set InfoPtr bit to say this is a PAP
#if USE_OBJTYPE
  objp->objType = PAP;
#endif
  return objp;
}

int getObjSize(Obj *o) {
  size_t objSize;
  ObjType type = getObjType(o);
  switch (type) {
  case PAP: {
    InfoTab *itp = getInfoPtr(o);
    int fvCount = itp->layoutInfo.boxedCount +
                  itp->layoutInfo.unboxedCount;
    objSize = sizeof(Obj) +
      (fvCount + 1 + o->payload[fvCount].b.bitmap.size) *
        sizeof(PtrOrLiteral);
    objSize = ((objSize + OBJ_ALIGNM1)/OBJ_ALIGN)*OBJ_ALIGN;
    break;
  } // PAP
  case FUN:
  case CON:
  case THUNK:
  case BLACKHOLE:
  case INDIRECT: {
    InfoTab *itp = getInfoPtr(o);
    objSize = sizeof(Obj) +
              itp->layoutInfo.payloadSize * sizeof(PtrOrLiteral);
    break;
  }
  default:
    LOG(LOG_DEBUG,"stg.c/getObjSize bad ObjType %d\n", type);
    assert(false);
    break;
  }
  objSize = ((objSize + OBJ_ALIGNM1)/OBJ_ALIGN)*OBJ_ALIGN;
  return objSize;
}
