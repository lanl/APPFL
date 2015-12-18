
// no header of this file's own for now
#include <stdio.h>
#include "stg.h"
#include "obj.h"
#include "stdlib.h"
#include "string.h"

static const int showDepthLimit = 1000;
static int depth;
static Obj *stack[1000];

void showStgObjRecDebug(Obj *p);
void showStgObjDebug(Obj *p) {
  depth = 0;
  showStgObjRecDebug(p);
}

void showStgObjRecPretty(Obj *p);
void showStgObjPretty(Obj *p) {
  depth = 0;
  showStgObjRecPretty(p);
  fprintf(stderr,"\n");
}

void showStgCont(Cont *c) {
  ContType type = getContType(c);
  switch (type) {
  case UPDCONT:
    fprintf(stderr,"UPDCONT  %s\n", c->ident);
    return;

  case CASECONT:
    fprintf(stderr,"CASECONT %s\n", c->ident);
    return;

  case CALLCONT:
    fprintf(stderr,"CALLCONT %s\n", c->ident);
    return;

  case FUNCONT:
    fprintf(stderr,"FUNCONT  %s\n", c->ident);
    return;

  default:
    fprintf(stderr,"showStgCont default case! %d %s\n", type, objTypeNames[type]);
    exit(0);
  }
}

void showStgObjRecPretty(Obj *p) {

  // depth check first
  if (depth+1 >= showDepthLimit) {
    fprintf(stderr, "******showStgObjRec depth exceeded\n");
    return;
  }

  InfoTab it = *(getInfoPtr(p));

  for (int i=0; i != depth; i++) {
    if (p == stack[i]) {
      fprintf(stderr, "((%s))", it.name);
      return;
    }
  }
  stack[depth++] = p;

  ObjType type = getObjType(p);
  if (type != BLACKHOLE &&
      type != INDIRECT &&
      type != it.objType) {
    if (!(type == PAP && it.objType == FUN)) {
      fprintf(stderr, "getObjType(p) = %s, it.objType = %s\n",
	      objTypeNames[type], objTypeNames[it.objType]);
      assert(false);
    }
  }
  if (strcmp(it.name, p->ident)) {
    if(type != PAP) {
      fprintf(stderr, "mismatch in infotab and object names \"%s\" != \"%s\"\n",
	      it.name, p->ident);
      assert(false);
    }
  }

  switch (type) {
  case FUN:
  case PAP:
  case THUNK:
  case BLACKHOLE:
    fprintf(stderr, "%s = <%s>", p->ident, objTypeNames[type]);
    break;

  case CON:
    fprintf(stderr, "%s = %s", p->ident, it.conFields.conName );
    int arity = it.conFields.arity;
    if (arity > 0) {
      if (arity > 1) fprintf(stderr, "(");
      else fprintf(stderr, " ");
      showStgValPretty(p->payload[0]);
      for (int i = 1; i < arity; i++) {
	    fprintf(stderr, ", ");
	    showStgValPretty(p->payload[i]);
      }
      if (arity > 1) fprintf(stderr, ")");
    }
    break;

  case INDIRECT:
    fprintf(stderr, "%s --> ", p->ident );
    showStgObjRecPretty(p->payload[0].op);
    break;

  default:
    fprintf(stderr,"********* default in showStgObj!\n");
    // exit(0);
  }
  depth--;
}

void showStgValPretty(PtrOrLiteral v) {
#if USE_ARGTYPE
  switch (v.argType) {
  case INT:
#ifdef __clang__
    fprintf(stderr,"%lld", v.i);
#else
    fprintf(stderr,"%ld", v.i);
#endif
    break;
  case DOUBLE:
    fprintf(stderr,"%f", v.d);
    break;
    //  case FLOAT:
    //    fprintf(stderr,"%f", v.f);
    //    break;
  case HEAPOBJ:
    showStgObjRecPretty(v.op);
    break;
  default:
    fprintf(stderr,"undefined PtrOrLiteral.tag! tag=%d\n", v.argType);
    exit(1);
  }
#endif
}


void showStgObjRecDebug(Obj *p) {

  // depth check first
  if (depth+1 >= showDepthLimit) {
    fprintf(stderr, "******showStgObjRec depth exceeded\n");
    return;
  }
  for (int i=0; i != depth; i++) {
    if (p == stack[i]) {
      fprintf(stderr, "   ***cycle\n");
      return;
    }
  }
  stack[depth++] = p;



  InfoTab it = *(getInfoPtr(p));
  ObjType type = getObjType(p);
  fprintf(stderr, "%s %s %s ", objTypeNames[type],
	  objTypeNames[it.objType], it.name);
  switch (type) {
  case FUN:
    fprintf(stderr,"\n");
    break;

  case PAP:
    fprintf(stderr,"\n");
    break;

  case CON:
    fprintf(stderr,"tag %d arity %d\n", it.conFields.tag, it.conFields.arity );
    for (int i = 0; i != it.conFields.arity; i++)
      showStgValDebug(p->payload[i]);
    break;

  case THUNK:
    fprintf(stderr,"\n");
    break;

  case BLACKHOLE:
    fprintf(stderr,"\n");
    break;

  case INDIRECT:
    fprintf(stderr,"INDIRECT to\n");
    showStgObjRecDebug(p->payload[0].op);
    break;

  default:
    fprintf(stderr,"default in showStgObj!\n");
    exit(1);
  }
  depth--;
}

void showStgValDebug(PtrOrLiteral v) {
#if USE_ARGTYPE
  switch (v.argType) {
  case INT:
#ifdef  __clang__
    fprintf(stderr,"INT %lld\n", v.i);
#else
    fprintf(stderr,"INT %ld\n", v.i);
#endif
    break;
  case DOUBLE:
    fprintf(stderr,"DOUBLE %f\n", v.d);
    break;
  case HEAPOBJ:
    fprintf(stderr,"HEAPOBJ %p ", v.op);
    showStgObjRecDebug(v.op);
    break;
  default:
    fprintf(stderr,"undefined PtrOrLiteral.tag!\n");
    exit(0);
  }
#endif
}

void showObjSpaceInfo();

void checkStgObjRec(Obj *p) {
  size_t i;

  assert(depth + 1 < showDepthLimit && "hc: checkStgObjRec depth exceeded\n");
  assert((uintptr_t)p % 8 == 0 && "hc: bad Obj alignment");
  // following causes test symbolT10 to fail!!!
  if (!(isHeap(p) || isSHO(p))) {
    showObjSpaceInfo();
    fprintf(stderr, "hc:  bad Obj location, object addr is %p, recursive depth %d\n", p, depth);
    showStgObjRecPretty(p);
    assert(false);
  }

  InfoTab *itp = getInfoPtr(p);
  assert((uintptr_t)itp % 8 == 0 && "hc: bad infoPtr alignment");
  InfoTab it = *itp;

  for (i = 0; i != depth; i++) if (p == stack[i]) return;
  stack[depth++] = p;

  ObjType type = getObjType(p);
  assert(type > OBJTYPE0BAD && type < PHONYENDOBJ && "hc: bad obj type");

  if (strcmp(it.name, p->ident)) {
    if (type != PAP) {
      fprintf(stderr, "mismatch in infotab and object names \"%s\" != \"%s\"\n",
          it.name, p->ident);
      assert(false);
    }
  }

  switch (type) {
  case FUN: {
    assert(it.objType == FUN && "hc: FUN infotab type mismatch");
    int FVCount = endFUNFVsU(p);
    if (FVCount) {
      // check that unboxed FVs really are unboxed
      for (i = startFUNFVsU(p); i < FVCount; i++) {
        assert(isUnboxed(p->payload[i]) && "hc: unexpected boxed FV in FUN");
      }
      // check that boxed FVs really are boxed
      for (i = startFUNFVsB(p); i < endFUNFVsB(p); i++) {
        assert(isBoxed(p->payload[i]) && "hc: unexpected unboxed FV in FUN");
        checkStgObjRec(p->payload[i].op);
      }
    }
    break;
  }

  case PAP: {
    assert(it.objType == FUN && "hc: PAP infotab type mismatch");
    int FVCount = endPAPFVsU(p);
    if (FVCount) {
      // check that unboxed FVs really are unboxed
      for (i = startPAPFVsU(p); i < FVCount; i++) {
        assert(isUnboxed(p->payload[i]) && "hc: unexpected boxed FV in PAP");
      }
      // check that boxed FVs really are nboxed
      for (i = startPAPFVsB(p); i < endPAPFVsB(p); i++) {
        assert(isBoxed(p->payload[i]) && "hc: unexpected unboxed FV in PAP");
        checkStgObjRec(p->payload[i].op);
      }
    }
    /* mkd gc
    // check that unboxed args really are unboxed
    for (i = startPAPargsU(p); i < endPAPargsU(p); i++) {
      assert(isUnboxed(p->payload[i]) && "hc: unexpected boxed arg in PAP");
    }

    // check that boxed args really are boxed
    for (i = startPAPargsB(p); i < endPAPargsB(p); i++) {
      assert(isBoxed(p->payload[i]) && "hc: unexpected unboxed arg in PAP");
      checkStgObjRec(p->payload[i].op);
    }
    */
    {
      Bitmap64 bm = p->payload[endPAPFVsU(p)].b;
      uint64_t mask = bm.bitmap.mask;
      int i = endPAPFVsU(p) + 1;
      for (int size = bm.bitmap.size; size != 0; size--, i++, mask >>= 1) {
        if (mask & 0x1UL) {
	  assert(isBoxed(p->payload[i]) && "hc: unexpected unboxed arg in PAP");
	} else {
	  assert(isUnboxed(p->payload[i]) && "hc: unexpected boxed arg in PAP");
	}
      }
    } /* mkd gc */
    break;
  }
  case CON:
    assert(it.objType == CON && "hc: CON infotab type mismatch");
    checkCONargs(p);
    // check that unboxed args really are unboxed
    for (i = startCONargsU(p); i < endCONargsU(p); i++) {
      assert(isUnboxed(p->payload[i]) && "hc: unexpected boxed arg in CON");
    }
    // check that boxed args really are boxed
    for (i = startCONargsB(p); i < endCONargsB(p); i++) {
      assert(isBoxed(p->payload[i]) && "hc: unexpected unboxed arg in CON");
      checkStgObjRec(p->payload[i].op);
    }
    break;
  case THUNK:
    assert(it.objType == THUNK && "hc: THUNK infotab type mismatch");
    //fallthrough..
  case BLACKHOLE:
    // check that unboxed FVs really are unboxed
    for (i = startTHUNKFVsU(p); i < endTHUNKFVsU(p); i++) {
      assert(isUnboxed(p->payload[i]) && "hc: unexpected boxed arg in THUNK");
    }
    // check that boxed FVs really are boxed
    for (i = startTHUNKFVsB(p); i < endTHUNKFVsB(p); i++) {
        assert(isBoxed(p->payload[i]) && "hc: unexpected unboxed arg in THUNK");
        checkStgObjRec(p->payload[i].op);
    }
    break;

  case INDIRECT:
    assert(isBoxed(p->payload[0]) && "hc: unexpected unboxed arg in INDIRECT");
    checkStgObjRec(p->payload[0].op);
    break;

  default:
    assert(false && "hc: bad obj in checkStgObjRec");
  }
  depth--;
}

void checkStgObj(Obj *p) {
  depth = 0;
  fprintf(stderr, "checkSTgObj \n");
  showStgObjRecPretty(p);   fprintf(stderr, "\n");

  checkStgObjRec(p);
}

void checkStgHeap() {
  for (char *p = (char*) stgHeap; 
       p < (char*) stgHP; 
       p += getObjSize((Obj *) p)) {
    checkStgObj((Obj *) p);
  }
}


size_t stgStatObjCount;
Obj * stgStatObj[100];

//tmp hack!
void *getToPtr();
void showObjSpaceInfo() {
  fprintf(stderr, "SHO range is %p to %p\n", 
	  &stgStatObj[0], 
	  &stgStatObj[stgStatObjCount-1]);
  fprintf(stderr, "heap range is %p to %p\n", stgHeap, stgHP);
  fprintf(stderr, "heap toPtr is %p\n", getToPtr());
}

bool isSHO(Obj *p) {
  return (p >= stgStatObj[0] && p <= stgStatObj[stgStatObjCount-1]);
}

bool isHeap(Obj *p) {
  return (isTo(p) || isFrom(p));
}

bool isFrom(void *p) {
  return (p >= fromPtr && (char *) p < (char *) fromPtr + stgHeapSize / 2);
}

bool isTo(void *p) {
  return (p >= toPtr && (char *) p < (char *) toPtr + stgHeapSize / 2);
}

bool isBoxed(PtrOrLiteral f) {
#if USE_ARGTYPE
  return (f.argType == HEAPOBJ && (isHeap(f.op) || isSHO(f.op)));
#else
  return (isHeap(f.op) || isSHO(f.op));
#endif
}

bool isUnboxed(PtrOrLiteral f) {
#if USE_ARGTYPE
  return (f.argType != HEAPOBJ);
#else
  return true;
#endif
}

void showStgStack() {
  fprintf(stderr,"\nSTG Stack:\n\n");
  for (char *p = (char*)stgSP;
       p < (char*)stgStack + stgStackSize;
       p += getContSize((Cont *)p)) {
     showStgCont((Cont *)p);
   }
}

void showStgHeap() {
  //  return;
  fprintf(stderr,"\nSTG static objects: \n\n");
  for (int i = 0; i != stgStatObjCount; i++) {
    showStgObj(stgStatObj[i]);
    fprintf(stderr,"\n");
  }

  fprintf(stderr,"\nSTG heap:\n\n");
  char *p = (char*)stgHeap;
  while (p < (char*)stgHP) {
    showStgObj((Obj *)p);
    p += getObjSize((Obj *)p);
    while (p < (char*)stgHP && *((uintptr_t *)p) == 0) {
      p += sizeof(uintptr_t);
    }
  }

  showStgStack();
}

