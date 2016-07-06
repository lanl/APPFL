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
#include "sanity.h"

/**
 *this function creates an array of pointers to all of the objects.
 */
Obj **mallocArrayOfAllObjects() {

  //the total array size will be the number of heap objects + the number of static heap objects
  //times size the of an object pointer
  //remember: make more space for stgCurVal
  size_t numHeapObjs = ((char *)stgHP - (char *)stgHeap) / sizeof(Obj);
  Obj **objArray = malloc (sizeof(Obj *) * (numHeapObjs + stgStatObjCount + 1));

  return objArray;
}

/**
 * this function will add objects to the array that is malloc'd in the function above
 * the first chunk of the objects that are added will be the static heap objects, and the
 * rest of the objects will be the heap objects
 * @param objArray for the array of object pointers that the function works with
 */
void addObjects (Obj *objArray[]) {
  LOG(LOG_DEBUG, "nSHO %d\n", stgStatObjCount);
  int objCount = 0;
  //add static heap objects
  for (objCount = 0; objCount < stgStatObjCount; objCount++) {
    // before adding the object, make sure that the pointer to the
    // object itself is valid
    sanityCheckSingleSHO(stgStatObj[objCount]);
    LOG(LOG_SPEW, "add SHO %s %d\n", stgStatObj[objCount]->ident, objCount);
    objArray[objCount] = stgStatObj[objCount];
  }

  //search for / add stgCurVal
  if (stgCurVal.op != NULL &&
      !isInObjArray(objArray, objCount, stgCurVal.op)) {
    LOG(LOG_SPEW, "add stgCurVal %s %d\n", stgCurVal.op->ident, objCount);
    objArray[objCount] = stgCurVal.op;
    objCount++;
  }

  //Cont. stack
  for (Cont *p = (Cont *)stgSP;
      (char *)p < (char*) stgStack + stgStackSize;
      p = (Cont *)((char*)p + getContSize(p))) {

  }

  int i = 0;
  while (i < objCount) {
    size_t start, end;
    Obj *op = derefHO(objArray[i]);

    switch (getObjType(op)) {
      case FUN:
        start = startFUNFVsB(op);
        end = endFUNFVsB(op);
        break;
      case PAP:
        start = startPAPFVsB(op);
        end = endPAPFVsB(op);
        break;
      case CON:
        start = startCONargsB(op);
        end = endCONargsB(op);
        break;
      case THUNK:
      case BLACKHOLE:
        start = startTHUNKFVsB(op);
        end = endTHUNKFVsB(op);
        break;
      default:
        LOG(LOG_FATAL, "bad object type\n");
        assert(false);
    }
    LOG(LOG_SPEW, "found %s %s %zu->%zu\n", objTypeToString(op), op->ident, start, end);

    for (int j = start; j < end; j++) {
      LOG(LOG_SPEW, "payload[%d]\n", j);
      //otherwise if the payload isn't in the array, add it
      if (!isInObjArray(objArray, objCount, op->payload[j].op)) {
          //make sure pointer is aligned, not null, and in the heap before you
          //add it
          sanityCheckPtr(op->payload[j].op);
          objArray[objCount] = op->payload[j].op;
          objCount++;
          LOG(LOG_SPEW, "add %s %d", objTypeToString(op->payload[j].op), objCount);
      }
    }
    i++;
  }
}

bool isInObjArray (Obj *objArray[], size_t objCount, Obj *obj) {
  for (int i = 0; i < objCount; i++) {
    if (objArray[i] == obj) {
      LOG(LOG_SPEW, "found obj %s at %d\n", obj->ident, i);
      return true;
    }
  }
  return false;
}

/**
 *printing stuff for debugging
 */
void printObjInfo (Obj *obj) {
  LOG(LOG_INFO, "\nobject identifier: %s\n", obj->ident);
  LOG(LOG_INFO, "object name: %s\n", obj->_infoPtr->name);
  LOG(LOG_INFO, "object type: %s\n", objTypeToString(obj));
  LOG(LOG_INFO, "object boxed count: %d\n", obj->_infoPtr->layoutInfo.boxedCount);
  LOG(LOG_INFO, "object unboxed count: %d\n\n", obj->_infoPtr->layoutInfo.unboxedCount);
}

/**
 *convert enum ObjType value to a string
 */
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

/**
 *checks the validity of the pointer to a static heap object
 */
void sanityCheckSingleSHO (Obj *obj) {

  sanityCheckObj(obj);

  if (!isSHO(obj)) {
    LOG(LOG_FATAL, "object isn't a SHO");
    assert(false);
  }
}

/**
 *to check if a pointer is not null, aligned, and is in the heap range
 */
void sanityCheckPtr(Obj *obj) {

  sanityCheckObj(obj);
  if (!(isHeap(obj))) {
    LOG(LOG_FATAL, "pointer is not in heap range");
    assert(false);
  }

}

/**
 *two simple checks for any obj. make sure it's not null, has the right size, and is aligned
 */
void sanityCheckObj (Obj *obj) {

  if (obj == NULL) {
    LOG(LOG_FATAL, "pointer points to null");
    assert(false);
  }

  if (!checkPtr8BitAligned(obj)) {
    LOG(LOG_FATAL, "bad object alignment");
    assert(false);
  }
}

/**
 *checks if a pointer to an object is properly aligned
 */
bool checkPtr8BitAligned (Obj *obj) {
  // make sure the object is aligned
  if ((uintptr_t)obj % OBJ_ALIGN != 0)
    return false;
  return true;
}
