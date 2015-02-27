// using the C call stack for calls
// inlining the dispatcher


#include <stdio.h>
#include <stdlib.h>

//------ fake typedef fp (*fp)()

typedef void (*vvfp)();
typedef vvfp (*FnPtr)();

#define RETURN() return NULL

#define JUMP(f)	return ((FnPtr)f)

#define CALL(f)	for (FnPtr _f = (FnPtr)f; _f; _f = (FnPtr)_f())

FnPtr f(), g(), h(), q(), r();

FnPtr f() {
  fprintf(stderr,"f CALLing g\n");
  CALL(g);
  fprintf(stderr,"f back from...somewhere\n");
  RETURN();
}

FnPtr g() {
  fprintf(stderr,"g JUMPing to h\n");
  JUMP(h);
  fprintf(stderr,"ERROR g back from somewhere!\n"); exit(0);
}

FnPtr h() {
  fprintf(stderr,"h JUMPing to q\n");
  JUMP(q);
  fprintf(stderr,"ERROR h back from somewhere!\n"); exit(0);
}

FnPtr q() {
  fprintf(stderr,"q calling r\n");
  CALL(r);
  fprintf(stderr,"q back from r\n");
  RETURN();
}

FnPtr r() {
  fprintf(stderr,"r here\n");
  RETURN();
}
  
int main (int argc, char **argv) {
  fprintf(stderr,"running mini-interpreter\n");

  CALL(f);

  fprintf(stderr,"exiting mini-interpreter\n");
}
