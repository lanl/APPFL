
// this compiles to tail calls with gcc -O2 and clang -O1
// gcc -O2 -S tailcall3.c
// clang -O1 -S tailcall3.c -- look for comment "TAILCALL"


// clang -O1 -emit-llvm -S -o tailcall3.ll tailcall3.c
//  NOTE:  the .ll file is misleading, it appears that "tail call" is just
//     a hint that a tail call might be possible.  To see the truth do it
//     in one step as above, or run "llc tailcall3.ll" for a .s file.

#include <stdio.h>
#include <stdint.h>
#include "tailcall_separate.h"
#include "f.h"

// return through continuation stack
// in practice the payload size will be variable
//

Cont stgStack[10];
Cont *stgSP;

void g(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("g(%d)\n", (int) i);
  // go where I'm supposed to go
  STGRETURN0();
  printf("back to g()???\n");
}

/*
void f(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("f(%d)\n", (int) i);
  STGRETURN0();
  printf("back to f()???\n");
}
*/
void finish(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("finish!\n");
  // ordinary return to C land
  return;
}

void start(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("start(%d)\n", (int) i);
  // last place to go
  stgSP = stgStack;
  *stgSP = (Cont) {.fp = &finish,
                   .argc = 0};
  // f will unwittingly jump to g(), then to finish()
  *++stgSP = (Cont) {.fp = &g,
		     .argc = 1,
		     .pl[0] = 88,};
  // prime the pump
  *++stgSP = (Cont) {.fp = &f,
		     .argc = 1,
		     .pl[0] = 99,};
  // never to return
  STGRETURN0();
  printf("back to start()???\n");
}

int main() {
  printf("main start\n");
  // cannot be a tail call because start's sig doesn't match main's
  JUMP0(start);
  printf("back to main()\n");
}
