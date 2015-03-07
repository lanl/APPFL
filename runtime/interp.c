// STG interpreter

// stack

StackObj stack[1000];
StackObj *sp = &stack[1000];

// global "registers"

  // pointer to expression (in heap) under evaluation
  HeapObj *stgNode;

  PtrOrLiteral stgRetVal;



// main4 = (HO4) THUNK( let { x = (ho4_1) CON(I 1) } 
//                       in x );

HeapObj *ho_4_1() {
  // build heap object for each bound variable
  HeapObj *x = mymalloc( sizeof( HeapObj ) );
  x->infoTabPtr = &it_I;
  x->payload[0].tag = INT;
  x->payload[0].i = 1;

  // next evaluate body of let, an expression
  // return (*x->InfoTabPtr->entryCode)();
  // something missing here...need a this pointer???
  return x;
}

// top-level interpreter
void interp() {
  switch (stgNode->infoTabPtr->objType) {
  case THUNK:
    
    break;
  case FUN:
    printf("interp() switch(stgNode) on FUN--not implemented\n");
    break;
  case PAP:
    printf("interp() switch(stgNode) on PAP--not implemented\n");
    break;
  case CON:
    printf("interp() switch(stgNode) on CON--not implemented\n");
    break;
  case BLACKHOLE:
    printf("interp() switch(stgNode) on BLACKHOLE--not implemented\n");
    break;
  default:
    printf("interp() switch(stgNode) on default--this should never happen\n");
  }
}


