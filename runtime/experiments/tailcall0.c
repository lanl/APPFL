#include <stdio.h>
#include <stdint.h>

typedef struct {
  int whatever;
  double whatever2;
} Obj;

typedef void (*Fun0)();

#define JUMP0(f)          f()

// return through continuation stack
#define STGRETURN0() \
  do {						\
    stgSP--;					\
    JUMP0(((stgSP+1)->fp));			\
    return;					\
  } while(0)

typedef struct {
  Fun0 fp;
  int argc;
  uint64_t pl[5];
} Cont;

Cont stgStack[10];
Cont *stgSP;

void g() {
  printf("g()\n");
  // go where I'm supposed to go
  STGRETURN0();
  printf("back to g()???\n");
}

void f() {
  printf("f()\n");
  STGRETURN0();
  printf("back to f()???\n");
}

void finish() {
  printf("finish!\n");
  // ordinary return to C land
  return;
}

void start() {
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
