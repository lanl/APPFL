#include <inttypes.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

#include "stgutils.h"
#include "stg.h"
#include "cmm.h"
#include "obj.h"
#include "show.h"
#include "sanity.h"

// ****************************************************************
// since we always jump through the top of the stg stack we need some
// place to go when we're done this continuation is special, dropping
// from stg land back to cmm land via RETURN0() rather than STGRETURN(0)

// stg_case_not_exhaustiveP(self, x)
FnPtr stg_case_not_exhaustiveP() {
  Cont *stg_sc = stgGetStackArgp(myThreadID());
  //  PtrOrLiteral self = stg_sc->payload[0];
  PtrOrLiteral x    = stg_sc->payload[1];
  LOG(LOG_ERROR, "stg_case_not_exhaustive_boxed: ");
  showStgVal(LOG_ERROR, x);
  LOG(LOG_ERROR, "\n");
  heapCheck(true, LOG_ERROR);
  exit(0);
}

InfoTab it_stg_case_not_exhaustiveP __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "stg_case_not_exhaustiveP",
  .entryCode = &stg_case_not_exhaustiveP,
#if USE_OBJTYPE
  .objType = FUN,
#endif
  //  .fvCount = 0,
  .funFields.arity = 1,
  .layoutInfo.boxedCount = 0,
  .layoutInfo.unboxedCount = 0,
};

Obj sho_stg_case_not_exhaustiveP = {
#if USE_OBJTYPE
  .objType = FUN,
#endif
  ._infoPtr = &it_stg_case_not_exhaustiveP,
  .ident = "stg_case_not_exhaustiveP",
};

// stg_case_not_exhaustiveN(self, x)
FnPtr stg_case_not_exhaustiveN() {
  Cont *stg_sc = stgGetStackArgp(myThreadID());
  //  PtrOrLiteral self = stg_sc->payload[0];
  PtrOrLiteral x    = stg_sc->payload[1];
  LOG(LOG_ERROR, "stg_case_not_exhaustive_unboxed: %" PRIx64 "\n", x.u);
  heapCheck(true, LOG_ERROR);
  exit(0);
}

InfoTab it_stg_case_not_exhaustiveN __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "stg_case_not_exhaustiveN",
  .entryCode = &stg_case_not_exhaustiveN,
  .objType = FUN,
  //  .fvCount = 0,
  .funFields.arity = 1,
  .layoutInfo.boxedCount = 0,
  .layoutInfo.unboxedCount = 0,
};

Obj sho_stg_case_not_exhaustiveN = {
#if USE_OBJTYPE
  .objType = FUN,
#endif
  ._infoPtr = &it_stg_case_not_exhaustiveN,
  .ident = "stg_case_not_exhaustiveN",
};

FnPtr stg_funcall() {
  LOG(LOG_INFO, "stg_funcall, returning self\n");
  STGRETURN0();
}

FnPtr stg_papcall() {
  LOG(LOG_INFO, "top-level PAP call, returning self\n");
  STGRETURN0();
}

FnPtr stg_concall() {
  LOG(LOG_INFO, "stg_concall, returning self\n");
  STGRETURN0();
}

FnPtr stgBlackhole() {
  LOG(LOG_ERROR, "stgBlackhole, exiting!\n");
  exit(0);
}

// we can't use this until FVs are stashed first
InfoTab it_stgBlackhole __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "default stgBlackhole",
  //    .fvCount = 1, // self
  .entryCode = &stgBlackhole,
  .objType = BLACKHOLE,
  .layoutInfo.payloadSize = 1, // space for indirect
  .layoutInfo.boxedCount = 1,
  .layoutInfo.unboxedCount = 0,
};

FnPtr stgIndirect() {
  LOG(LOG_INFO, "stgIndirect, jumping through indirection\n");
  int id = myThreadID();
  stgCurVal[id] = stgCurVal[id].op->payload[0]; // Obj takes self in stgCurVal
  STGJUMP0(getInfoPtr(stgCurVal[id].op)->entryCode);
}


InfoTab it_stgIndirect __attribute__((aligned(8))) = {
#if DEBUG_INFOTAB
  .pi = PI(),
#endif
  .name = "stgIndirect",
  // .fvCount = 1, // target of indirect
  .entryCode = &stgIndirect,
  .objType = INDIRECT,
  .layoutInfo.payloadSize = 1, // target of indirect
  .layoutInfo.boxedCount = 1,
  .layoutInfo.unboxedCount = 0,
};

FnPtr stgUpdateCont() {
  int id = myThreadID();
  Cont *contp = stgGetStackArgp(id);
  assert(getContType(contp) == UPDCONT && "I'm not an UPDCONT!");
  PtrOrLiteral p = contp->payload[0];
  assert(mayBeBoxed(p) && "not a HEAPOBJ!");
  LOG(LOG_INFO, "stgUpdateCont updating\n  ");
  showStgObj(LOG_INFO, p.op);
  LOG(LOG_INFO, "with\n  ");
  showStgObj(LOG_INFO, stgCurVal[id].op);
  if (getObjType(p.op) != BLACKHOLE) {
    LOG(LOG_INFO, "but updatee is %s not a BLACKHOLE!\n",
	    objTypeNames[getObjType(p.op)]);
    heapCheck(true, LOG_ERROR);
    assert(getObjType(p.op) == BLACKHOLE);
  }

  int oldObjSize = getObjSize(p.op);

  // the order of the following two operations is important for concurrency
  p.op->payload[0] = stgCurVal[id];  // return stgCurVal, not the indirect, for efficiency

  if (rtArg.sharing) {
    p.op->_infoPtr = &it_stgIndirect;
#if USE_OBJTYPE
    p.op->objType = INDIRECT;
#endif
    strcpy( p.op->ident, it_stgIndirect.name );
    int newObjSize = getObjSize(p.op);
    assert(newObjSize <= oldObjSize);

    // this is for displaying a fragmented heap
    memset((char*)p.op+newObjSize, 0, oldObjSize-newObjSize);

    LOG(LOG_INFO, "stgUpdateCont leaving...\n  ");
  } 
  stgPopCont();

  // this returns stgCurVal, not the indirect, for efficiency
  STGRETURN0();
}

CInfoTab cit_stgUpdateCont __attribute__((aligned(8))) =
  { .name = "default stgUpdateCont",
    .entryCode = &stgUpdateCont,
    .contType = UPDCONT,
    .cLayoutInfo.payloadSize = 1, // self
    //.cLayoutInfo.boxedCount = 1,
    //.cLayoutInfo.unboxedCount = 0,
    //    .cLayoutInfo.bm = (Bitmap64) {.bitmap.mask = 0x1,
    //				  .bitmap.size = 1},
    .cLayoutInfo.bm.bitmap.mask = 0x1,
    .cLayoutInfo.bm.bitmap.size = 1,
  };

FnPtr fun_stgShowResultCont() {
  int id = myThreadID();
  LOG(LOG_INFO, "done!\n");
  stgPopCont();  // clean up--normally the job of the returnee
  LOG(LOG_RESULT, "The answer is\n");
#if USE_ARGTYPE
  showStgVal(LOG_RESULT, stgCurVal[id]); stgCurVal[id].op = NULL;
#else
  showStgObj(LOG_RESULT, stgCurVal[id].op); stgCurVal[id].op = NULL;
#endif
  LOG(LOG_RESULT, "\n");
  RETURN0();
}

CInfoTab cit_stgShowResultCont __attribute__((aligned(8))) =
  { .name       = "fun_showResultCont",
    //    .fvCount    = 0,
    .entryCode  = &fun_stgShowResultCont,
    .contType    = CALLCONT,
    //.cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    //.cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

void stgThunk(PtrOrLiteral self) {
  LOG(LOG_DEBUG, "stgThunk thread=%d\n",myThreadID());
  assert(mayBeBoxed(self) && "stgThunk:  not HEAPOBJ\n");
  Cont *contp = stgAllocCont(myThreadID(), &cit_stgUpdateCont);
  contp->payload[0] = self;
  strcpy(contp->ident, self.op->ident); //override default
  // can't do this until we capture the variables in a stack frame
  // self.op->infoPtr = &it_stgBlackHole;
  LOG(LOG_INFO, "BLACKHOLING %s thread=%d\n", self.op->ident, myThreadID());
#if USE_OBJTYPE
  self.op->objType = BLACKHOLE;
#endif
  self.op->_infoPtr = setLSB2(self.op->_infoPtr); // this is a Blackhole
  assert(getObjType(self.op) == BLACKHOLE);
}

FnPtr stgStackCont() {
  LOG(LOG_INFO, "stgStackCont returning\n");
  stgPopCont();
  STGRETURN0();  // return through continuation stack
}

CInfoTab cit_stgStackCont __attribute__((aligned(8))) =
  { .name = "stgStackCont",
    .entryCode = &stgStackCont,
    .contType = STACKCONT,
    //.cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    //.cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

CInfoTab cit_stgLetCont __attribute__((aligned(8))) =
  { .name = "stgStackCont",
    .entryCode = &stgStackCont,
    .contType = LETCONT,
    //.cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    //.cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

FnPtr stgPopMeCont() {
  LOG(LOG_INFO, "stgPopMeCont returning\n");
  stgPopCont();
  STGRETURN0();  // return through continuation stack
}

CInfoTab cit_stgPopMeCont __attribute__((aligned(8))) =
  { .name = "stgPopMeCont",
    .entryCode = &stgPopMeCont,
    .contType = POPMECONT,
    //.cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    //.cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

void stgCaseToPopMe(Cont *contp) {
  assert(contp->contType == CASECONT);
  contp->cInfoPtr = &cit_stgPopMeCont;
  contp->entryCode = cit_stgPopMeCont.entryCode;
  contp->contType = cit_stgPopMeCont.contType;
  // keep contp->ident unchanged
}

FnPtr stgCallCont() {
  stgPopCont();
  LOG(LOG_INFO, "stgCallCont returning\n");
  RETURN0();  // fall back to the cmm trampoline
}

CInfoTab cit_stgCallCont __attribute__((aligned(8))) =
  { .name = "stgCallCont",
    .entryCode = &stgCallCont,
    .contType = CALLCONT,
    //.cLayoutInfo.boxedCount = -1,  // shouldn't be using this
    //.cLayoutInfo.unboxedCount = -1,  // shouldn't be using this
    .cLayoutInfo.bm.bitmap.mask = 0x0,   // shouldn't be using this
    .cLayoutInfo.bm.bitmap.size = 0,  // shouldn't be using this
  };

// forall t27,t29.t27 -> t29 -> t29
// par(self, x, y)
FnPtr fun_par()
{
    LOG(LOG_INFO, "par here thread=%d\n", myThreadID());
    
    PtrOrLiteral *argp = &stgGetStackArgp(myThreadID())->payload[0];
    
    (void) argp;
    // suppress warning
    ;
    // y
    stgCurVal[myThreadID()] = argp[2];
    // boxed EAtom, stgCurVal updates itself
    STGJUMP();
}

InfoTab it_par __attribute__((aligned(OBJ_ALIGN))) = {
  .name ="par",
  .entryCode = &stg_funcall,
#if USE_OBJTYPE
  .objType = FUN,
#endif
  .layoutInfo.payloadSize = 0,
  .layoutInfo.boxedCount = 0,
  .layoutInfo.unboxedCount = 0,
  .funFields.arity =2,
  .funFields.trueEntryCode = fun_par
};


// built-in SHOs need to be contiguous in memory
Obj sho_par __attribute__((aligned(OBJ_ALIGN))) = {
  ._infoPtr =&it_par,
#if USE_OBJTYPE
  .objType =FUN,
#endif
  .ident ="par",
  .payload = {{0}}
};

const int stgStatObjCount = 1;
Obj *const stgStatObjTable[1] = {&sho_par};
