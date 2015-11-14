#include "stgc.h"
#include "stgApply.h"
void registerSHOs();



void registerSHOs() {
}


DEFUN0(start) {
  registerSHOs();
  Obj *showResultCont = stgAllocCallCont2(&it_stgShowResultCont, 0);
  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));
  STGRETURN0();
  ENDFUN;
}

int main (int argc, char **argv) {
  parseArgs(argc, argv);
  initStg();
  initCmm();
  initGc();
  CALL0_0(start);
  showStgHeap();
  GC();
  return 0;
}

