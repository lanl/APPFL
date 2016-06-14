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
  Obj **objArray = malloc (sizeof(Obj *) * (numHeapObjs + numStaticHeapObjs));

  return objArray;
}
/**
 * this function will add objects to the array that is malloc'd in the function above
 * the first chunk of the objects that are added will be the static heap objects, and the
 * rest of the objects will be the heap objects
 * @param objArray for the array of object pointers that the function works with
 */
void addObjects (Obj *objArray[]) {

  //i stays at the beginning of the array, 
  //j moves to keep track of how many objects have been added
  //k is for going through the payload boxed objects
  int i, j = 0, k;
  //flag to check whether or not we need to add stgCurVal to the objArray
  int addStgCurVal = 0, extraFlag = 1;
  //mark the end of the static heap object range
  int end = stgStatObjCount;

  //first add static heap objects
  for (i = 0; i < stgStatObjCount; i++) {
    EPRINTF("address of stgCurVal.op = %p\n", stgCurVal.op);
    EPRINTF("address of SHO number %d = %p\n\n", i, stgStatObj[i]);
    //check if StgCurVal points to a SHO
    if (stgCurVal.op == stgStatObj[i] && extraFlag) {
        //to avoid infinite loop in checking stgCurVal
        //with current statObj
        extraFlag = 0;
        addStgCurVal = 1;
        i--;
        continue;
    }
    // before you add the object, make sure that the pointer to the 
    // object itself is valid
    sanityCheckSingleSHO(stgStatObj[i]);
    objArray[i] = stgStatObj[i];
    //printObjInfo(objArray[i]);
  }

  //add stgCurVal if it doesn't point to anything
  //in the SHO array
  if (addStgCurVal) {
    objArray[end] = stgCurVal.op;
    end++;
  }

  //outer (i) loop traverses the objarray by object
  for (i = 0; i < end; i++) {    
    //if the object doesn't have boxed variables in it's payload
    //go to the next one
    if (objArray[i]->_infoPtr->layoutInfo.boxedCount == 0) {
      continue;
    }
    //inner loop (k) goes through the payload of each object
    for (k = 0; k < objArray[i]->_infoPtr->layoutInfo.boxedCount; k++) {
      //the j loop goes through the objarray, from o to end to make sure we don't add duplicates
      for (j = 0; j < end; j++) {      
        //if payload already exists in obj array, don't copy it over
        if (objArray[j] == objArray[i]->payload[k].op) {
          break;
        }
      }
    }
    //otherwise add it when j = end
    if (j == end) {
      //make sure pointer is to a heap/static heap object
      if (!(isSHO(objArray[i]->payload[k].op) || isHeap(objArray[i]->payload[k].op))) {
        LOG(LOG_FATAL, "Object isn't a heap object or a SHO");
        end++;
        break;
      }
      //make sure pointer is aligned and right size 
      if (!checkPtr8BitAligned(objArray[i]->payload[k].op) || !(checkPtrCorrectSize(objArray[i]->payload[k].op))) {
        LOG(LOG_FATAL, "problem: bad object alignment");
        end++;
        break;
      }
      //add new payload object to end of array
      objArray[end] = objArray[i]->payload[k].op;
      end++;
      break;
    }
  }
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
 *three simple checks for any obj. make sure it's not null, has the right size, and is aligned
 */
void sanityCheckObj (Obj *obj) {

  if (obj == NULL) {
    LOG(LOG_FATAL, "problem: pointer to object is wonky");
    exit(1);
  }

  if (!checkPtr8BitAligned(obj)) {
    LOG(LOG_FATAL, "problem: bad object alignment");
    exit(1);
  }

  if (!checkPtrCorrectSize(obj)) {
    LOG(LOG_FATAL, "problem: sizeof obj is not a multiple of OBJ_ALIGN");
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

/**
 *makes sure the size of the object is not funky
 */
bool checkPtrCorrectSize (Obj *obj) {
  //make sure the pointer is of a size which is a multiple of OBJ_ALIGN
  if ((sizeof(obj) % OBJ_ALIGN) != 0)
    return false;
  return true;
}
