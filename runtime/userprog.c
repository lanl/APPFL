#include "stgc.h"
#include "stgApply.h"


void registerSHOs();
FnPtr fun_bp();

enum tycon_Bool{
  _False,
  _True };

enum tycon_BoolPair{
  _BP };

InfoTab it_bp __attribute__((aligned(8))) = 
  { .name                = "bp",
    // fvs []
    .entryCode           = &fun_bp,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };


InfoTab it_f_0 __attribute__((aligned(8))) = 
  { .name                = "f_0",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "False",
  };

InfoTab it_t_0 __attribute__((aligned(8))) = 
  { .name                = "t_0",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "True",
  };

InfoTab it_r_0 __attribute__((aligned(8))) = 
  { .name                = "r_0",
    // fvs [("f_0",Bool[B] ),("t_0",Bool[B] )]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "BP",
  };


InfoTab it_false __attribute__((aligned(8))) = 
  { .name                = "false",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "False",
  };

InfoTab it_true __attribute__((aligned(8))) = 
  { .name                = "true",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 0,
    // argPerm = []
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "",
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "True",
  };

extern Obj sho_bp;
extern Obj sho_false;
extern Obj sho_true;

Obj sho_bp =
{
  .infoPtr   = (uintptr_t)&it_bp,
  .objType   = THUNK,
  .ident     = "bp",
  .payload = {0}
};

Obj sho_false =
{
  .infoPtr   = (uintptr_t)&it_false,
  .objType   = CON,
  .ident     = "false",
  .payload = {
    },
};

Obj sho_true =
{
  .infoPtr   = (uintptr_t)&it_true,
  .objType   = CON,
  .ident     = "true",
  .payload = {
    },
};

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_bp;
  stgStatObj[stgStatObjCount++] = &sho_false;
  stgStatObj[stgStatObjCount++] = &sho_true;
}


// BoolPair[B] 
DEFUN1(fun_bp, self) {
  fprintf(stderr, "bp here\n");
  stgThunk(self);
  Obj *f_0 = stgNewHeapObj( &it_f_0 );
  Obj *t_0 = stgNewHeapObj( &it_t_0 );
  Obj *r_0 = stgNewHeapObj( &it_r_0 );
  r_0->payload[0] = HOTOPL((Obj *)STGHEAPAT(2,3)); // f_0
  r_0->payload[1] = HOTOPL((Obj *)STGHEAPAT(2,2)); // t_0
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // r_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "bp returning\n");
  STGRETURN0();
  ENDFUN;
}

DEFUN0(start) {
  registerSHOs();
  Obj *showResultCont = stgAllocCallCont2(&it_stgShowResultCont, 0);
  STGEVAL(((PtrOrLiteral){.argType = HEAPOBJ, .op = &sho_main}));
  STGRETURN0();
  ENDFUN;
}

int main (int argc, char **argv) {
  parseArgs(argc, argv);
  initStg();
  initCmm();
  initGc();
  CALL0_0(start);
  showStgHeap();
  GC();
  return 0;
}

