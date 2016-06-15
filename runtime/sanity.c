#include <stdio.h>
#include <assert.h>
#include <stdbool.h>
#include <string.h>

#include "gc.h"
#include "stg.h"
#include "stgutils.h"
#include "obj.h"
#include "options.h"

#include "sanity.h"

//number of heap objects. a safe overestimate is (stgHP - stgHeap) / sizeof(Obj)
#define numHeapObjs (((char *)stgHP - (char *)stgHeap) / sizeof(Obj))

//number of static heap objects
#define numStaticHeapObjs stgStatObjCount

//macro for fprintf
#define EPRINTF(...) fprintf(stderr, __VA_ARGS__)

/**
 *this function creates an array of pointers to all of the objects.
 */
Obj **mallocArrayOfAllObjects() {

  //the total array size will be the number of heap objects + the number of static heap objects
  //times size the of an object pointer
  //remember: make more space for stgCurVal
  Obj **objArray = malloc (sizeof(Obj *) * (numHeapObjs + numStaticHeapObjs + 1));

  return objArray;
}
/**
 * this function will add objects to the array that is malloc'd in the function above
 * the first chunk of the objects that are added will be the static heap objects, and the
 * rest of the objects will be the heap objects
 * @param objArray for the array of object pointers that the function works with
 */
void addObjects (Obj *objArray[]) {


  //int i, j = 0, k;
  //mark the end of the static heap object range

  int i;
  //first add static heap objects
  for (i = 0; i != stgStatObjCount; i++) {
    // before you add the object, make sure that the pointer to the 
    // object itself is valid
    sanityCheckSingleSHO(stgStatObj[i]);
    objArray[i] = stgStatObj[i];
    //printObjInfo(objArray[i]);
  }

  //could be j = i
  int j = stgStatObjCount;
  
  //search for / add stgCurVal
  if (stgCurVal.op != NULL) {
    int k;
    for(k = 0; k != j; k++) {
      if (objArray[k] == stgCurVal.op) {
        break;
      }
    }
    if (k == j) {
      objArray[j] = stgCurVal.op;
      j++;
    }
  }

  //reset i to mark beginning of array
  i = 0;
  int p;
  while (i != j) {
 
  //these change based on whether the object is a thunk / blackhole or not
  int start = 0;
  int end = getInfoPtr(objArray[i])->layoutInfo.boxedCount;
   
    switch (getObjType(objArray[i])) {
      
      case THUNK:
      case BLACKHOLE:
        start = startTHUNKFVsB(objArray[i]);
        end = endTHUNKFVsB(objArray[i]);
        break; 
      default:
        break;
    }

    for (p = start; p != end; p++) {
      //otherwise if the payload isn't in the array, add it
      if (!isInObjArray(objArray, objArray[i]->payload[p].op, 0, j-1)) {
          //make sure pointer is aligned, not null, and in the heap before you
          //add it 
          sanityCheckPtr(objArray[i]->payload[p].op);
          objArray[j] = objArray[i]->payload[p].op;
          j++;
      }
    }
    i++;
  }
}

bool isInObjArray (Obj *objArray[], Obj *obj, int start, int end) {
  
  int i;
  for (i = start; i != end; i++) {
    if (objArray[i] == obj) {
      return true;
    }
  }
  return false;
}

/**
 *printing stuff for debugging
 */
void printObjInfo (Obj *obj) {
  EPRINTF("\nobject identifier: %s\n", obj->ident);
  EPRINTF("object name: %s\n", obj->_infoPtr->name);
  EPRINTF("object type: %s\n", objTypeToString(obj));
  EPRINTF("object boxed count: %d\n", obj->_infoPtr->layoutInfo.boxedCount);
  EPRINTF("object unboxed count: %d\n\n", obj->_infoPtr->layoutInfo.unboxedCount);
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
    LOG(LOG_FATAL, "problem: object isn't a SHO");
    exit(1); 
  }
}

/**
 *to check if a pointer is not null, aligned, and is in the heap range
 */
void sanityCheckPtr(Obj *obj) {

  sanityCheckObj(obj);
  if (!(isHeap(obj))) {
    LOG(LOG_FATAL, "problem: pointer is not in heap range");
    exit(1);
  }
  
}

/**
 *two simple checks for any obj. make sure it's not null, has the right size, and is aligned
 */
void sanityCheckObj (Obj *obj) {

  if (obj == NULL) {
    LOG(LOG_FATAL, "problem: pointer points to null");
    exit(1);
  }

  if (!checkPtr8BitAligned(obj)) {
    LOG(LOG_FATAL, "problem: bad object alignment");
    exit(1);
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

