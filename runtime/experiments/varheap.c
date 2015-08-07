#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <sys/mman.h> // for  mmap()


typedef enum {
  // heap and stack objects
  FUN,
  PAP,
  CON,
  THUNK,
  BLACKHOLE,
  INDIRECT,
  // stack objects
  UPDCONT,
  CASECONT,
  CALLCONT,
  FUNCONT,        // for strict evaluation
  // garbage collection
  FORWARD
} ObjType;

struct _Obj;
typedef struct _Obj Obj;

// for testing
typedef char PtrOrLiteral;

struct _Obj {
  Obj *prev;
  int payloadSize;
  //InfoTab *infoPtr;         // canonical location of ObjType field
  ObjType objType;          // to distinguish PAP, FUN, BLACKHOLE, INDIRECT
  int argCount;             // for PAP, how many args already applied to?
  char ident[64];           // temporary, just for tracing
  PtrOrLiteral payload[1];
};

void *stgHeap = NULL, *stgHP = NULL;

const size_t stgHeapSize  = (size_t)4*(size_t)(1024*1024*1024);

void initStg() {

  stgHeap =
    mmap( NULL,                   // void *address, NULL => no preference
	  stgHeapSize,           // size_t length
	  PROT_READ | PROT_WRITE, // int protect, write may require read
	  MAP_PRIVATE | MAP_ANON, // int flags
	  -1,                     // int filedes
	  0 );                    // off_t offset

  if (stgHeap == MAP_FAILED) {
    fprintf(stderr,"mmap stg heap failed!!\n");
    exit(1);
  }

  stgHP = stgHeap; // first free address
}


Obj* stgNewHeapObj(int size) {
  printf("new HO %d\n", size);
  Obj *curp = (Obj *)stgHP;
  curp->payloadSize = size; //TODO: do we want to update this here?

  stgHP = (char *)stgHP + sizeof(Obj) + (size - 1)*sizeof(PtrOrLiteral);
  ((Obj *)stgHP)->prev = curp;
  printf("ptrs HP=%x cur=%x\n",stgHP, curp);
  return curp;
}

Obj *stgHeapAt(size_t n) {

  if (n == -1) {

    printf("back -1 %s %x\n", ((Obj*)stgHP)->prev->ident, ((Obj*)stgHP)->prev);

    return ((Obj*)stgHP)->prev;

  } else if (n == -2) {
    printf("back -2 %s %x\n",  ((Obj*)stgHP)->prev->prev->ident, ((Obj*)stgHP)->prev->prev);
    return ((Obj*)stgHP)->prev->prev;

  } else {
    printf("error back > -2\n");
    return NULL;
  }

}

int main() {
	initStg();

	Obj *left = stgNewHeapObj(32);
	left->objType = CON;
	left->ident[5] = "left";
	printf("left %s %x\n", ((Obj*)stgHP)->prev->ident, ((Obj*)stgHP)->prev);

	Obj *middle = stgNewHeapObj(32);
	middle->objType = CON;
	middle->ident[7] = "middle";
	printf("middle %s %x\n", ((Obj*)stgHP)->prev->ident, ((Obj*)stgHP)->prev);

	Obj *right = stgNewHeapObj(32);
	right->objType = CON;
	right->ident[6] = "right";
	printf("right %s %x\n", ((Obj*)stgHP)->prev->ident, ((Obj*)stgHP)->prev);

	stgHeapAt(-1);

	stgHeapAt(-2);
}
