#ifndef SANITY_H
#define SANITY_H

//void heapCheck ();
//void checkSingleHeapObject (Obj *obj);

Obj **mallocArrayOfAllObjects();
void addObjects (Obj **objArray);
void sanityCheckSingleSHO (Obj *obj);
void sanityCheckObj (Obj *obj);
bool checkPtr8BitAligned (Obj *obj);
bool checkPtrCorrectSize (Obj *obj);

#endif
