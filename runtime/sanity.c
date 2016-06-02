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

/**
 *sanity checks any single object.
 *@param obj for the the object that this function sanity checks
 */
void checkSingleHeapObject (Obj *obj) {

  /* all Obj types have three fields: a pointer to an information table,
   * an object type, and a payload array (pointer to the payload)*/

  /* make sure that the pointer to the object is okay in the first place*/
  if (obj == NULL)
    return;

  /*make sure object is of the right size*/
  if (sizeof(obj) % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "problem: sizeof obj is not a multiple of OBJ_ALIGN");
    exit(1);
  }

  /* make sure the object is aligned*/
  if ((uintptr_t)obj % OBJ_ALIGN != 0) {
    LOG(LOG_FATAL, "problem: bad object alignment");
    exit(1);
  }

  /*make sure it's a HeapObject or a SHO*/ 
  if (!(isHeap(obj) || isSHO(obj))) {
    LOG(LOG_FATAL, "problem: object isn't a SHO or Heap Object");
    exit(1);
  }

  /* make sure the object resides in the right area of the heap, i.e.
   * correctly between stgHeap and stgHP*/
   if (obj < stgHeap || obj > stgHP) {
     LOG(LOG_FATAL, "problem: object isn't in the heap");
     exit(1);
   }

  /*then check the sanity of the infotable*/
  /*check if the _infoptr is valid*/
  if(obj->infoPtr == NULL) {
     LOG(LOG_FATAL, "problem: infoPtr is invalid");
     exit(1); 
  }
  
  /*check if the LSB is set*/
  if(!isLSBset(obj->infoPtr)) {
     LOG(LOG_FATAL, "problem: LSB isn't set in infoPtr");
     exit(1);
  }

  /*check if the first word in the infoptr struct is pi*/
  if (obj->infoPtr.pi != PI()) {
    LOG (LOG_FATAL, "problem: pi isn't what we made it to be, the laws of the universe have been broken");
    exit(1);
  }

}
