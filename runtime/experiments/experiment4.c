// using the C call stack for calls

#include <stdio.h>

//------ fake typedef fp (*fp)()
typedef void (*vvfp)();
typedef vvfp (*FnPtr)();

typedef enum {
  CmmReturn,
  CmmJump
} CmmReturnKind;

typedef struct {
  CmmReturnKind retKind;
  FnPtr fp;
} CmmRetVal;

typedef CmmRetVal (*CmmFnPtr)();
void cmmCall(CmmFnPtr f);

#define CALL0(f)				\
  do {						\
    cmmCall(f);					\
  } while (0)

#define RETURN0()				\
  do {						\
    CmmRetVal rv;				\
    rv.retKind = CmmReturn;			\
    return rv;					\
  } while (0)

#define JUMP0(f)				\
  do {						\
    CmmRetVal rv;				\
    rv.retKind = CmmJump;			\
    rv.fp = (FnPtr)&f;				\
    return rv;					\
  } while (0)


CmmRetVal f();
CmmRetVal g();
CmmRetVal h();

CmmRetVal f() {
  printf("f CALLing g\n");
  CALL0(g);
  printf("f back from...somewhere\n");
  RETURN0();
}

CmmRetVal g() {
  printf("g JUMPing to h\n");
  JUMP0(h);
  printf("ERROR g back from h!\n");
  RETURN0();
}

CmmRetVal h() {
  printf("h here\n");
  RETURN0();
}

void cmmCall(CmmFnPtr f) {
  CmmRetVal rv;
  rv = ((CmmFnPtr)(*f))();
  while (1) {
    if (rv.retKind == CmmReturn)
      return;
    rv = ((CmmFnPtr)(*rv.fp))();
  }
}

int main (int argc, char **argv) {
  printf("running mini-interpreter\n");

  cmmCall((CmmFnPtr)f);

  printf("exiting mini-interpreter\n");
  return 0;
}
