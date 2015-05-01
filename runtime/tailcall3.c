#include <stdio.h>
#include <stdint.h>

typedef struct {
  int whatever;
  double whatever2;
} Obj;

typedef void (*Fun4)(Obj * /* self */, uint64_t, uint64_t, uint64_t, uint64_t /* args */);

// JUMPN, N > 4 use the user stack for excess args
// 6 registers are available on x86-64, arbitrarily using 5 here
// the overhead is trivial, just zeroing the unused ones
// just a demo so self is NULL
#define JUMP0(f)          f(NULL, 0, 0, 0, 0)
#define JUMP1(f,x)        f(NULL, x, 0, 0, 0)
#define JUMP2(f,x,y)      f(NULL, x, y, 0, 0)
#define JUMP3(f,x,y,z)    f(NULL, x, y, z, 0)
#define JUMP4(f,x,y,z,t)  f(NULL, x, y, z, t)

// return through continuation stack
// in practice the payload size will be variable
//
#define STGRETURN0()	     \
  switch ((stgSP--)->argc) {				\
  case 0 : return JUMP0(((stgSP+1)->fp));			\
  case 1 : return JUMP1(((stgSP+1)->fp), (stgSP+1)->pl[0]);		\
  case 2 : return JUMP2(((stgSP+1)->fp), (stgSP+1)->pl[0], (stgSP+1)->pl[1]);		\
  case 3 : return JUMP3(((stgSP+1)->fp), (stgSP+1)->pl[0], (stgSP+1)->pl[1], (stgSP+1)->pl[2]);		\
  case 4 : return JUMP4(((stgSP+1)->fp), (stgSP+1)->pl[0], (stgSP+1)->pl[1], (stgSP+1)->pl[2], (stgSP+1)->pl[3]); \
  }

typedef struct {
  Fun4 fp;
  int argc;
  uint64_t pl[5];
} Cont;

Cont stgStack[10];
Cont *stgSP;

void g(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("g(%d)\n", (int) i);
  // go where I'm supposed to go
  STGRETURN0();
  printf("back to g()???\n");
}

void f(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("f(%d)\n", (int) i);
  STGRETURN0();
  printf("back to f()???\n");
}

void finish(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("finish!\n");
  // ordinary return to C land
}

void start(Obj *self , uint64_t i , uint64_t j, uint64_t k, uint64_t l) {
  printf("start()\n");
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
  // this will not be a tail call because start's sig doesn't match main's
  JUMP0(start);
  printf("back to main()\n");
}
