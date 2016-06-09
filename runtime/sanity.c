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
#define PRINTF(...) fprintf(stderr, __VA_ARGS__)

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
void addObjects (Obj **objArray) {

  //i stays at the beginning of the array, 
  //j moves to keep track of how many objects have been added
  //k is for going through the payload boxed objects
  //dup_exists is for forced continuation in while loop so as to not copy duplicate objects over
  int i, j, k, dup_exists;
  
  //mark the end of the static heap object range
  int end = stgStatObjCount;

  //first add static heap objects
  for (i = 0; i < stgStatObjCount; i++) {
    // before you add the object, make sure that the pointer to the 
    // object itself is valid
    sanityCheckSingleSHO(stgStatObj[i]);
    objArray[i] = stgStatObj[i];
  }

  //now we have to check the payloads of the static heap objects
  //and add them to the part of the array which isn't comprised
  //of static heap objects

  j = 0;

  PRINTF("\nstgStatObjCount: %zd\n\n", stgStatObjCount);
  while (j != end) {
   
    //TODO: wtf is going on

    //why you're doing this:
    //you just wanted to make sure that the first chunk of the array
    //was the static heap objects loaded correctly
    //that's why the last j+1 should match stgStatObjCount


    //current dilemma: 
    //apparently, everything you've added is a SHO
    //and you always get two more SHO's than stgStatObjCount
    //?

    if (isSHO(objArray[j])) {
      PRINTF("SHO #%d", j+1);
    }

    printObjInfo(objArray[j]);
    
    //if there are no boxed variables to copy over, move to the next
    //object in the array and continue
    if (objArray[j]->_infoPtr->layoutInfo.boxedCount == 0) {
      j++;
      continue;
    }
    
    dup_exists = 0;
    //search the range of added objects to see if the current object
    //you're adding is already there
    for (i = 0; i < j; i++) {
      if (objArray[i] == objArray[j]) {
        dup_exists = 1;
        break;
      }
    }
    //if you found the object in the array already, you don't want to add it again
    //duplicates will screw up malloc
    if (dup_exists) {
      continue;
    }
    
    //get infoTab pointer for current object
    InfoTab *itp = getInfoPtr(objArray[j]);
    
    //copy all boxed values, or pointers to objects, into the
    //array of objects
    for (k = 0; k < itp->layoutInfo.boxedCount; k++) {
      objArray[end + k] = objArray[j]->payload[k].op;
    }
    
    //move high water mark
    end += (k + 1);
    
    j++;
  }
}

/**
 *printing stuff for debugging
 */
void printObjInfo (Obj *obj) {
  PRINTF("\nobject identifier: %s\n", obj->ident);
  PRINTF("object name: %s\n", obj->_infoPtr->name);
  PRINTF("object type: %s\n", objTypeToString(obj));
  PRINTF("object boxed count: %d\n", obj->_infoPtr->layoutInfo.boxedCount);
  PRINTF("object unboxed count: %d\n\n", obj->_infoPtr->layoutInfo.unboxedCount);
}

/**
 *convert enum ObjType to a string
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
 *makes sure the size of the object is not 
 */
bool checkPtrCorrectSize (Obj *obj) {
  //make sure the pointer is of a size which is a multiple of OBJ_ALIGN
  if ((sizeof(Obj) % OBJ_ALIGN) != 0)
    return false;
  return true;
}

/**
 *goes through the heap and checks the static and dynamically allocated heap objects
 */
/*void heapCheck () {
  int i;
//static heap object checking
for (i = 0; i < stgStatObjCount; i++) {
printf("object number: %d\n", i);
printf("objects address: %p\n", &stgStatObj[i]);
printf("object identifier: %s\n", stgStatObj[i]->ident);
checkSingleHeapObject(stgStatObj[i]);
}

}*/

/**
 *sanity checks any single object.
 *@param obj for the the object that this function sanity checks
 */
/*void checkSingleHeapObject (Obj *obj) {

// all Obj types have three fields: a pointer to an information table,
// an object type, and a payload array (pointer to the payload)

int i;

//make sure that the pointer to the object is okay in the first place
if (obj == NULL)
return;

//make sure object is of the right size
if (sizeof(obj) % OBJ_ALIGN != 0) {
LOG(LOG_FATAL, "problem: sizeof obj is not a multiple of OBJ_ALIGN");
exit(1);
}

// make sure the object is aligned
if ((uintptr_t)obj % OBJ_ALIGN != 0) {
LOG(LOG_FATAL, "problem: bad object alignment");
exit(1);
}

//make sure it's a HeapObject or a SHO
if (!(isHeap(obj) || isSHO(obj))) {
LOG(LOG_FATAL, "problem: object isn't a SHO or Heap Object");
exit(1);
}

//store the infotab pointer in a variable so i have to type less later
InfoTab *info = (getInfoPtr(obj));
//check if the _infoptr is valid
if(info == NULL) {
LOG(LOG_FATAL, "problem: infoPtr is invalid");
exit(1); 
}

//store object type for later
ObjType type = getObjType(obj);
//make sure the object type is valid
assert (type > PHONYSTARTOBJ && type < PHONYENDOBJ && "problem: bad object type");

//first check that sanity of the static heap objects
// note, we don't care where they are, they're managed by C stuff
if (isSHO(obj)) {
//make sure LSB is set
assert(!isLSBset(info));
for (i = 0; i < stgStatObjCount; i++) {
//printf("hi, i'm paul\n");
}

}

//now sanity check the dynamically allocated heap objects
else if(isHeap(obj)) {

//make sure LSB is set
assert(!isLSBset(info));

// make sure the dynamically allocated heap object resides in the right area of the heap, i.e.
// correctly between stgHeap and stgHP
if ((char *)obj < ((char *)stgHeap - (sizeof((char *)obj) + sizeof(PtrOrLiteral)))) {
LOG(LOG_FATAL, "problem: object is below the allocated space for the heap");
exit(1);
}

else if ((char *)obj > ((char *)stgHP - (sizeof((char *)obj) + sizeof(PtrOrLiteral)))) {
LOG(LOG_FATAL, "problem: object is above the allocated space for the heap");
exit(1);
}
}

//then check the sanity of the infotable

//check if the LSB is set
//if(!isLSBset(info)) {
//LOG(LOG_FATAL, "problem: LSB isn't set in infoPtr");
//exit(1);
//}

//check if the first word in the infoptr struct is pi
//#if DEBUG_INFOTAB
//  if (info.pi != PI()) {
//    LOG (LOG_FATAL, "problem: pi isn't what we made it to be, the laws of the universe have been broken");
//    exit(1);
//  }
//#endif
}*/
