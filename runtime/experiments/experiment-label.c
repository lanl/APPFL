// using the C call stack for calls

#include <stdlib.h>
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
  void *retaddr;
} CmmRetVal;

typedef CmmRetVal (*CmmFnPtr)();
void cmmCall(CmmFnPtr f);

void cmmCall(CmmFnPtr f) {
  CmmRetVal rv = ((CmmFnPtr)(*f))();
  while (1) {
    if (rv.retKind == CmmReturn)
      return;
    rv = ((CmmFnPtr)(*rv.fp))();
  }
}

void cmmCall2(CmmFnPtr f) {
  CmmRetVal rv = { .fp = f };
  do {
    rv = ((CmmFnPtr)(*rv))();
    if (rv.retKind == CmmReturn)
      return;
  } while (1);
}

#define CALL(f)				\
  do {						\
    cmmCall(f);					\
  } while (0)

#define RETURN()				\
  do {						\
    CmmRetVal rv;				\
    rv.retKind = CmmReturn;			\
    return rv;					\
  } while (0)

#define JUMP(f)				\
  do {						\
    CmmRetVal rv;				\
    rv.retKind = CmmJump;			\
    rv.fp = (FnPtr)f;				\
    return rv;					\
  } while (0)


void lCall(CmmFnPtr f, void *retaddr) {
  CmmRetVal rv = ((CmmFnPtr)(*f))(retaddr);
  while (1) {
    if (rv.retKind == CmmReturn)
      return;
    rv = ((CmmFnPtr)(*rv.fp))(retaddr);
  }
}

#define LCALL(F)				\
  do {						\
    __label__ retaddr;				\
    lCall(F,&&retaddr)				\
retaddr:					\
  } while (0)

  

CmmRetVal f();
CmmRetVal g();
CmmRetVal h();
CmmRetVal j();
CmmRetVal k();

CmmRetVal f() {
    printf("f CALLing g\n");
    LCALL(g);
    printf("f back from...somewhere\n");
    LRETURN();
}

CmmRetVal g() {
  printf("g JUMPing to h\n");
  LJUMP(h);
  printf("ERROR g back from somewhere!\n"); exit(0);
}

CmmRetVal h() {
  printf("h here\n");
  printf("h jumping to k\n");
  LJUMP(k);
  printf("ERROR h back from somewhere!\n"); exit(0);
}

CmmRetVal k() {
  printf("k here\n");
  LRETURN();
}

int main (int argc, char **argv) {
  printf("entering label land\n");

  LCALL(f);

  printf("back from label land\n");
  return 0;
}
