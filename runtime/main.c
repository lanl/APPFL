
#include <stdio.h>
#include <assert.h>

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"
#include "predefs.h"

DEFUN0(start) {
  stgPushCont(sho_stgShowResultCont);
  stgCurVal = (PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main3};
  while (stgCurVal.argType == HEAPOBJ &&
	 stgCurVal.op->objType == THUNK) {
    stgPushCont((Obj){.infoPtr = &it_stgCallCont});
    STGCALL1(stgCurVal.op->infoPtr->entryCode, stgCurVal);
    stgPopCont();
  }
  STGRETURN0(); // return through stgShowResultCont
  ENDFUN;
}

int main (int argc, char **argv) {
  fprintf(stderr,"running stg\n");
  initStg();
  initCmm();
  initPredefs();
  fprintf(stderr,"main() calling user program \n");

  CALL0_0(start);
  
  fprintf(stderr,"back from user program\n");
  showStgHeap();
  return 0;
}
