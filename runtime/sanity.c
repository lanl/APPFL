#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"
#include "obj.h"
#include "options.h"
#include "log.h"
#include "show.h"
#include "sanity.h"

__attribute__((always_inline)) inline char *getIdent(Obj *obj) {
  #if USE_IDENT
    return obj->ident;
  #else
    return NULL;
  #endif
}

__attribute__((always_inline)) inline bool checkObjType(Obj *obj, ObjType objType) {
  #if USE_OBJTYPE
    return (obj->objType == objType);
  #else
    return true;
  #endif
}

__attribute__((always_inline)) inline bool checkInfoTabName(InfoTab *it, char *name) {
  #if USE_INFOTAB_NAME
    return (strcmp(it->name, "stgIndirect") == 0);
  #else
    return true;
  #endif
}

__attribute__((always_inline)) inline bool checkIdent(Obj *obj) {
  #if USE_IDENT
    for (int i = 0; obj->ident[i] != '\0'; i++) {
      assert(i < IDENT_SIZE && "sanity: bad ident size");
      if(!isprint(obj->ident[i])) return false;
    }
  #endif
  return true;
}

__attribute__((always_inline)) inline bool checkInfoTabHeader(InfoTab *it) {
  #if DEBUG_INFOTAB
    return (it->pi = PI());
  #else
    return true;
  #endif
}



bool checkPtr8BitAligned(void *p) {
  // make sure the object is aligned
  if ((uintptr_t)p % OBJ_ALIGN != 0)
    return false;
  return true;
}


// two simple checks for any obj. make sure it's not null and is aligned
void sanityCheckObj(Obj *obj) {
  assert(obj != NULL && "sanity: pointer points to null");
  assert(checkPtr8BitAligned(obj) && "bad object alignment");
}


void sanityCheckIndirect(Obj *obj) {
  assert(checkObjType(obj, INDIRECT) && "sanity: bad indirect objtype");
  InfoTab *it = getInfoPtr(obj);
  assert(checkPtr8BitAligned(it) && "sanity: bad indirect infotab alignment");
  assert(checkIdent(obj) && "sanity: bad indirect ident");

  // see it_stgInderect in stgutils.c
  assert(checkInfoTabHeader(it) && "sanity: bad IT header");
  assert(checkInfoTabName(it, "stgIndirect") && "sanity: bad indirect name");
  assert(it->objType == INDIRECT && "sanity: bad IT objType");
  assert(it->layoutInfo.payloadSize == 1 && "sanity: bad payload size");
  assert(it->layoutInfo.boxedCount == 1 && "sanity: bad boxedCount");
  assert(it->layoutInfo.unboxedCount == 0 && "sanity: bad unboxedCount");
}


bool isInObjArray(Obj *objArray[], size_t objCount, Obj *obj) {
  for (int i = 0; i < objCount; i++) {
    if (objArray[i] == obj) {
      LOG(LOG_SPEW, "found obj %s at %d\n", getIdent(obj), i);
      return true;
    }
  }
  return false;
}


// convert enum ObjType value to a string
char *objTypeToString(Obj *obj) {

  switch (getObjType(obj)) {
    case PHONYSTARTOBJ:
      return "PHONYSTARTOBJ";
    case FUN:
      return "FUN";
    case PAP:
      return "PAP";
    case CON:
      return "CON";
    case THUNK:
      return "THUNK";
    case BLACKHOLE:
      return "BLACKHOLE";
    case INDIRECT:
      return "INDIRECT";
    case PHONYENDOBJ:
      return "PHONYENDOBJ";
  }
  return "BAD OBJTYPE";
}


// add an object to the array if it is not in the array already
void addObject(Obj *obj, Obj *objArray[], int *objCount, bool heap) {
  // add things pointed to by indirects rather than indirects
  if (getObjType(obj) == INDIRECT) {
    sanityCheckIndirect(obj);
    Obj *obi = derefHO(obj);
    if (!isInObjArray(objArray, *objCount, obi)) {
      sanityCheckObj(obi);
      objArray[*objCount] = obi;
      LOG(LOG_SPEW, "add HO %s %d\n", getIdent(obi), *objCount);
      (*objCount)++;
    }
  } else {
    if (!isInObjArray(objArray, *objCount, obj)) {
      sanityCheckObj(obj);
      if (heap) assert(isHeap(obj) && "sanity: pointer is not in heap range");
      objArray[*objCount] = obj;
      LOG(LOG_SPEW, "add HO %s %d\n", getIdent(obj), *objCount);
      ( *objCount)++;
    }
  }
}


// add objects to the object array
int addObjects(Obj *objArray[]) {
  LOG(LOG_DEBUG, "nSHO %d\n", stgStatObjCount);
  int objCount = 0;
  //add static heap objects
  for (int i = 0; i < stgStatObjCount; i++) {
    // before adding the object, make sure that the pointer to the
    // object itself is valid
    sanityCheckObj(stgStatObj[i]);
    assert(isSHO(stgStatObj[i]) && "sanity: object isn't a SHO");
    addObject(stgStatObj[i], objArray, &objCount, false);
  }

  //search for / add stgCurVal
  if (stgCurVal.op != NULL &&
      !isInObjArray(objArray, objCount, stgCurVal.op)) {
    assert(mayBeBoxed(stgCurVal) && "sanity: unexpected unboxed val in stgCurVal");
    addObject(stgCurVal.op, objArray, &objCount, false);
  }

  int i = 0;
  while (i < objCount) {
    //LOG(LOG_SPEW, "i %d\n",i);
    size_t start, end;

    assert(getObjType(objArray[i]) != INDIRECT && "sanity: indirect in objArray");
    Obj *obj = objArray[i];

    switch (getObjType(obj)) {
      case FUN:
        start = startFUNFVsB(obj);
        end = endFUNFVsB(obj);
        break;
      case PAP:
        start = startPAPFVsB(obj);
        end = endPAPFVsB(obj);
        break;
      case CON:
        start = startCONargsB(obj);
        end = endCONargsB(obj);
        break;
      case THUNK:
      case BLACKHOLE:
        start = startTHUNKFVsB(obj);
        end = endTHUNKFVsB(obj);
        break;
      default:
        LOG(LOG_FATAL, "bad object type\n");
        assert(false);
    }
    LOG(LOG_SPEW, "found %s %s %zu->%zu\n", objTypeToString(obj), getIdent(obj), start, end);

    for (int j = start; j < end; j++) {
      Obj *objj = obj->payload[j].op;
      LOG(LOG_SPEW, "payload[%d] = %s\n", j, getIdent(objj));
      //if the payload isn't in the array, add it
      addObject(objj, objArray, &objCount, true);
    }
    i++;
  }
  return objCount;
}


// create an array of pointers to all of the objects.
Obj **mallocArrayOfAllObjects() {
  //the total array size will be the number of heap objects + the number of static heap objects
  //times size the of an object pointer
  //remember: make more space for stgCurVal
  size_t numHeapObjs = ((char *)stgHP - (char *)stgHeap) / sizeof(Obj);
  Obj **objArray = malloc (sizeof(Obj *) * (numHeapObjs + stgStatObjCount + 1));

  return objArray;
}


void checkObjFull(Obj *obj) {
  if (!(isHeap(obj) || isSHO(obj))) {
    LOG(LOG_ERROR, "sanity: bad obj location");
    assert(false);
  }
  InfoTab *itp = getInfoPtr(obj);
  InfoTab it = *itp;
  ObjType type = getObjType(obj);
  assert(type > PHONYSTARTOBJ && type < PHONYENDOBJ && "sanity: bad obj type");

  #if USE_IDENT && USE_INFOTAB_NAME
    if (strcmp(it.name, obj->ident)) {
      if (type != PAP) {
        LOG(LOG_ERROR, "sanity: mismatch in infotab and object names \"%s\" != \"%s\"\n",
          it.name, obj->ident);
        assert(false);
      }
    }
  #endif

  switch (type) {
  case FUN: {
    assert(it.objType == FUN && "sanity: FUN infotab type mismatch");
    int FVCount = endFUNFVsU(obj);
    if (FVCount) {
      // check that unboxed FVs really are unboxed
      for (int i = startFUNFVsU(obj); i < FVCount; i++) {
        assert(mayBeUnboxed(obj->payload[i]) && "sanity: unexpected boxed FV in FUN");
      }
      // check that boxed FVs really are boxed
      for (int i = startFUNFVsB(obj); i < endFUNFVsB(obj); i++) {
        assert(mayBeBoxed(obj->payload[i]) && "sanity: unexpected unboxed FV in FUN");
      }
    }
    break;
  }

  case PAP: {
    assert(it.objType == FUN && "sanity: PAP infotab type mismatch");
    int FVCount = endPAPFVsU(obj);
    if (FVCount) {
      // check that unboxed FVs really are unboxed
      for (int i = startPAPFVsU(obj); i < FVCount; i++) {
        assert(mayBeUnboxed(obj->payload[i]) && "sanity: unexpected boxed FV in PAP");
      }
      // check that boxed FVs really are boxed
      for (int i = startPAPFVsB(obj); i < endPAPFVsB(obj); i++) {
        assert(mayBeBoxed(obj->payload[i]) && "sanity: unexpected unboxed FV in PAP");
      }
    }

    Bitmap64 bm = obj->payload[endPAPFVsU(obj)].b;
    uint64_t mask = bm.bitmap.mask;
    int i = endPAPFVsU(obj) + 1;
    for (int size = bm.bitmap.size; size != 0; size--, i++, mask >>= 1) {
      if (mask & 0x1UL) {
        assert(mayBeBoxed(obj->payload[i]) && "sanity: unexpected unboxed arg in PAP");
      } else {
        assert(mayBeUnboxed(obj->payload[i]) && "sanity: unexpected boxed arg in PAP");
      }
    }
    break;
  }
  case CON:
    assert(it.objType == CON && "sanity: CON infotab type mismatch");
    checkCONargs(obj);
    // check that unboxed args really are unboxed
    for (int i = startCONargsU(obj); i < endCONargsU(obj); i++) {
      assert(mayBeUnboxed(obj->payload[i]) && "sanity: unexpected boxed arg in CON");
    }
    // check that boxed args really are boxed
    for (int i = startCONargsB(obj); i < endCONargsB(obj); i++) {
      assert(mayBeBoxed(obj->payload[i]) && "sanity: unexpected unboxed arg in CON");
    }
    break;
  case THUNK:
    assert(it.objType == THUNK && "sanity: THUNK infotab type mismatch");
    //fallthrough..
  case BLACKHOLE:
    // check that unboxed FVs really are unboxed
    for (int i = startTHUNKFVsU(obj); i < endTHUNKFVsU(obj); i++) {
      assert(mayBeUnboxed(obj->payload[i]) && "sanity: unexpected boxed arg in THUNK");
    }
    // check that boxed FVs really are boxed
    for (int i = startTHUNKFVsB(obj); i < endTHUNKFVsB(obj); i++) {
        assert(mayBeBoxed(obj->payload[i]) && "sanity: unexpected unboxed arg in THUNK");
    }
    break;

  case INDIRECT:
    assert(mayBeBoxed(obj->payload[0]) && "sanity: unexpected unboxed arg in INDIRECT");
    break;

  default:
    assert(false && "sanity: bad obj in checkObjFull");
  }

}


void heapCheck(bool display, LogLevel logLevel) {
  Obj **objArray = mallocArrayOfAllObjects();
  int objCount = addObjects(objArray);
  if (display) LOG(logLevel, "stg HEAP:\n------------\n");
  for (int i = 0; i < objCount; i++) {
    checkObjFull(objArray[i]);
    if (display) showStgObj(logLevel, objArray[i]);
  }
  if (display) LOG(logLevel, "\n");
}


void stackCheck(bool display, LogLevel logLevel) {
  if (display) LOG(logLevel, "stg STACK:\n-----------\n");
  for (Cont *p = (Cont *)stgSP;
       (char *)p < (char*) stgStack + stgStackSize;
       p = (Cont *)((char*)p + getContSize(p))) {
    int contType = getContType(p);
    assert(contType > PHONYSTARTCONT &&
     contType < PHONYENDCONT && "sanity: bad cont type");
    LOG(logLevel, "check Cont %s %s\n", contTypeNames[contType], p->ident);
    Bitmap64 bm = p->layout;
    uint64_t mask = bm.bitmap.mask;
    int size = bm.bitmap.size;
    if (contType != LETCONT) {
     for (int i = 0; i != size; i++, mask >>= 1) {
     	  if (mask & 0x1UL) {
          assert(mayBeBoxed(p->payload[i]) && "sanity: unexpected unboxed arg in CONT");
     	  } else {
          assert(mayBeUnboxed(p->payload[i]) && "sanity: unexpected boxed arg in CONT");
   	    }
     }  // for
    } else { // LETCONT
      for (int i = 0; i != size; i++) {
        if (p->payload[i].op != NULL) {
          assert(mayBeBoxed(p->payload[i]) && "sanity: unexpected unboxed arg in LETCONT");
        }
      }
    }
    if (display) showStgCont(logLevel, (Cont *)p);
  }
  if (display) LOG(logLevel, "\n");
}
