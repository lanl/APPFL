#include <stdio.h>
#include <assert.h>
#include "../types.h"
#include "../cmm.h"

CmmRetVal f();
CmmRetVal g();
CmmRetVal h();
CmmRetVal j();
CmmRetVal k();
CmmRetVal l();
CmmRetVal m();

DEFUN0(f) {
  fprintf(stderr,"f CALLing g\n");
  CALL0_0(ADDR(g));
  fprintf(stderr,"f back from...somewhere\n");
  RETURN0();
  ENDFUN;  // ; optional
}

// alternate syntax
DEFUN0(g)
  fprintf(stderr,"g JUMPing to h\n");
  JUMP0(ADDR(h));
  fprintf(stderr,"ERROR g back from h!\n");
  RETURN0();
ENDFUN


DEFUN0(h) {
  fprintf(stderr,"h JUMPing to j(99)\n");
  CMMVal arg = { .valTag = CMMInt, .cmmInt = 99 };
  JUMP1(ADDR(j),arg);
  RETURN0();
  ENDFUN;
}

DEFUN1(j,v) {
  fprintf(stderr,"j got a %d\n", v.cmmInt);
  fprintf(stderr,"j calling k with l\n");
  CALL1_0(ADDR(k),ADDR(l));
  RETURN0();
  ENDFUN;
}

DEFUN1(k,fp) {
  assert(fp.valTag == CMMFP);
  fprintf(stderr,"k got a function, calling it...\n");
  CALL0_0(fp);
  fprintf(stderr,"back to k\n");
  RETURN0();
  ENDFUN;
}

DEFUN0(l) {
  fprintf(stderr,"l here, calling m\n");
  CMMVal x;
  CALL1_1( x, ADDR(m), ((CMMVal) { .valTag = CMMInt, .cmmInt = 77 }) );
  fprintf(stderr,"l here, m returned %d\n", x.cmmInt);
  RETURN0();
  ENDFUN;
}

DEFUN1(m,i) {
  fprintf(stderr,"m here, I got a %d\n", i.cmmInt);
  RETURN1( ((CMMVal) {.valTag = CMMInt, .cmmInt = i.cmmInt * 2 }) );
  ENDFUN;
}
  

int main (int argc, char **argv) {
  fprintf(stderr,"running mini-interpreter\n");
  CALL0_0(ADDR(f));
  fprintf(stderr,"exiting mini-interpreter\n");
  return 0;
}
