
#include <stdio.h>
#include <assert.h>

#include "stg.h"
#include "cmm.h"
#include "stgcmm.h"
#include "predefs.h"

int main (int argc, char **argv) {
  fprintf(stderr,"running stg\n");
  initStg();
  initCmm();
  initPredefs();
  fprintf(stderr,"main() calling user program \n");

  // off to cmm land
  CALL0_0(start);
  
  fprintf(stderr,"back from user program\n");
  showStgHeap();
  return 0;
}
