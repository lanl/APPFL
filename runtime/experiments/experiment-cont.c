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
} CmmRetVal;

typedef CmmRetVal (*CmmFnPtr)();
void cmmCall(CmmFnPtr f);

void cmmCall(CmmFnPtr f) {
  CmmRetVal rv;
  rv = ((CmmFnPtr)(*f))();
  while (1) {
    if (rv.retKind == CmmReturn)
      return;
    rv = ((CmmFnPtr)(*rv.fp))();
  }
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

// ************* STG stuff

CmmFnPtr contStack[10];
int contSP = -1;

#define CONTPUSH(f)				\
  do {						\
    contStack[++contSP] = f;			\
  } while (0)

#define CONTPOP()				\
  do {						\
    contSP--;					\
  } while (0)

// this continuation means return to caller
CmmRetVal contGoBack() {
  printf("contGoBack here\n");
  CONTPOP();
  RETURN();
}

#define STGCALL(f)				\
  do {						\
    CONTPUSH(contGoBack);			\
    CALL(f);					\
  } while (0)

#define STGJUMP(f)				\
  do {						\
    JUMP(f);					\
  } while (0)

#define STGRETURN()				\
  do {						\
    JUMP(contStack[contSP]);			\
  } while (0)

// stg rules:
// - must end execution with STGJUMP or STGRETURN
// - may optionally push a continuation before STGJUMP
// - continuations `know' they're continuations, must clean up continuation stack
//     before STGJUMP or STGRETURN


CmmRetVal f();
CmmRetVal g();
CmmRetVal h();
CmmRetVal j();
CmmRetVal k();
CmmRetVal acont();

CmmRetVal f() {
  printf("f CALLing g\n");
  STGCALL(g);
  printf("f back from...somewhere\n");
  STGRETURN();
}

CmmRetVal g() {
  printf("g JUMPing to h\n");
  STGJUMP(h);
  printf("ERROR g back from somewhere!\n"); exit(0);
}

CmmRetVal h() {
  printf("h here\n");
  printf("h jumping to k with continuation acont\n");
  CONTPUSH(acont);
  STGJUMP(k);
  printf("ERROR h back from somewhere!\n"); exit(0);
}

CmmRetVal k() {
  printf("k here\n");
  STGRETURN();
}

CmmRetVal acont() {
  printf("acont here\n");
  CONTPOP();
  STGRETURN();
}

// exit stg land
//x CmmRetVal done() {
//x   printf("exiting STG land\n");
//x   RETURN();
//x }

int main (int argc, char **argv) {
  printf("entering stg land\n");

  //x CONTPUSH(done);
  //x cmmCall((CmmFnPtr)f);

  STGCALL(f);

  printf("back from stg land\n");
  return 0;
}
