#include "stg.h"
#include "stgutils.h"
#include "stgapply.h"

FnPtr stgApply() {

  // STACKCONT with actual parameters
  Cont *stackframe = stgGetStackArgp();

  // &argv[0] is args to stgApply, &argv[1] is args to funoid
  PtrOrLiteral *argv = stackframe->payload;
  Bitmap64 bm = stackframe->layout;
  // argc is number of arguments funoid applied to
  int argc = bm.bitmap.size - 1;

  PRINTF("stgApply %s\n", getInfoPtr(argv[0].op)->name);
  argv[0].op = derefPoL(argv[0]);
  // this if just saves a possibly unneeded call
  if (getObjType(argv[0].op) == THUNK) {
    PRINTF("stgApply THUNK\n");
    STGEVAL(argv[0]);
    // argType already set
    argv[0].op = derefPoL(argv[0]);  // must be indirect
  } // if THUNK

  switch (getObjType(argv[0].op)) {
  
  case FUN: {
    int arity = getInfoPtr(argv[0].op)->funFields.arity;
    PRINTF("stgapply FUN %s arity %d\n", 
           getInfoPtr(argv[0].op)->name, 
           getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;  // may be negative
  
    // too many args?
    if (excess > 0) {
      PRINTF("stgApply FUN %d excess\n", excess);

      // call with return FUN with arity args
      // funoid + arity payload
      { Cont *newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1 + arity );
      	Bitmap64 newbm = bm; 
      	newbm.bitmap.size = 1 + arity; // high excess bits will be ignored
      	newframe->layout = newbm;
      	memcpy(newframe->payload, argv, (1+arity) * sizeof(PtrOrLiteral));
      	PRINTF("stgApply CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      	STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);
      	PRINTF("stgApply back from CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      } // scope

      // re-use existing stgApply frame
      // new funoid
      argv[0] = stgCurVal;
      // shift excess args
      memmove(&argv[1], &argv[1 + arity], excess * sizeof(PtrOrLiteral));
      // adjust the bitmap
      bm.bitmap.mask >>= arity;  // arity + 1 - 1
      bm.bitmap.mask |= 0x1;  // funoid is boxed
      stackframe->layout = bm;
      // adjust stackframe size, invalidates argv, stackframe, updates bitmap.size
      stackframe = stgAdjustTopContSize(stackframe, -arity);
      // tail call stgApply
      STGJUMP0(stgApply);
    } else 
  
    // just right?
    if (excess == 0) {
      // reuse call frame
      STGJUMP0(getInfoPtr(stackframe->payload[0].op)->funFields.trueEntryCode);

    // excess < 0, too few args
    } else {

      PRINTF("stgApply FUN %d too few args\n", -excess);
      int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount + 
                    getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;
      // stgNewHeapPAPmask puts layout info at payload[fvCount]
      bm.bitmap.mask >>= 1; // remove funoid
      bm.bitmap.size -= 1;        
      Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), bm);
      // copy fvs
      PRINTF("stgApply FUN inserting %d FVs into new PAP\n", fvCount);
      memcpy(&pap->payload[0], &argv[0].op->payload[0], fvCount * sizeof(PtrOrLiteral));
      // copy args to just after fvs and layout info
      PRINTF("stgApply FUN inserting %d args into new PAP\n", argc);
      memcpy(&pap->payload[fvCount+1], &argv[1], argc * sizeof(PtrOrLiteral));
      stgCurVal = HOTOPL(pap);
      // pop stgApply cont - superfluous, it's self-popping
      stgPopCont();
      STGRETURN0();
    } // if excess
  } // case FUN
  
  case PAP: {
    int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount + 
                  getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;
    Bitmap64 papargmap = argv[0].op->payload[fvCount].b;
    int papargc = papargmap.bitmap.size;
    PtrOrLiteral *papargv = &argv[0].op->payload[0];
    int arity = getInfoPtr(argv[0].op)->funFields.arity - papargc;
    PRINTF("stgapply PAP/FUN %s arity %d\n", 
            getInfoPtr(argv[0].op)->name, 
            getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;    // may be negative
  
    // too many args?
    if (excess > 0) {
      PRINTF("stgApply PAP %d excess\n", excess);

      { Cont *newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1 + arity + papargc);
        // make space for funoid
      	papargmap.bitmap.mask <<= 1;
      	papargmap.bitmap.mask |= 0x1;
      	papargmap.bitmap.size += 1;

      	// need first arity bits of bm less funoid
      	Bitmap64 newbm = bm;
      	newbm.bitmap.mask >>= 1; // remove funoid
      	newbm.bitmap.mask <<= (papargc + 1);  // excess high bits ignored
      	newbm.bitmap.size = arity;
      	// combine and insert
      	papargmap.bits += newbm.bits;
      	newframe->layout = papargmap;
        // CALLCONT args
      	newframe->payload[0] = argv[0]; // self
      	// copy old args
      	memcpy(&newframe->payload[1], 
      	       &argv[0].op->payload[1 + fvCount], 
      	       papargc * sizeof(PtrOrLiteral));
      	// copy new args
      	memcpy(&newframe->payload[1 + papargc], 
      	       &argv[1], 
      	       arity * sizeof(PtrOrLiteral));
      	// call-with-return the funoid
      	PRINTF("stgApply CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      	STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);
      	PRINTF("stgApply back from CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      } // scope
      // re-use existing stgApply frame
      // restore the funoid
      argv[0] = stgCurVal;
      // shift the args down
      memmove(&argv[1], &argv[1 + arity], arity * sizeof(PtrOrLiteral));
      // adjust the bitmap
      // stgAdjustContSize will adjust the size
      bm.bitmap.mask >>= arity;  // arity + 1 - 1
      bm.bitmap.mask |= 0x1;  // funoid is boxed
      stackframe->layout = bm;
      // adjust stackframe size, invalidates argv, stackframe, updates bitmap.size
      stackframe = stgAdjustTopContSize(stackframe, -arity); // units are PtrOrLiterals
      // try again - tail call stgApply
      STGJUMP0(stgApply);
    } else 

    // just right?
    if (excess == 0) {
      PRINTF("stgApply PAP just right: %d args in PAP, %d new args\n", 
             papargc, arity);

      // re-use existing stgApply frame
      // adjust stackframe size, invalidates argv, stackframe, updates bitmap.size
      stackframe = stgAdjustTopContSize(stackframe, papargc); 
      PtrOrLiteral *argv = &stackframe->payload[0];
      // shift new args up
      memmove(&argv[1 + papargc], &argv[1], argc * sizeof(PtrOrLiteral));
      // copy old args in
      memcpy(&argv[1], &papargv[0], papargc * sizeof(PtrOrLiteral));
      // adjust bitmap
      bm.bitmap.mask <<= papargc; // overwrite funoid bit
      bm.bitmap.mask |= (papargmap.bitmap.mask << 1); // room for funoid bit
      bm.bitmap.mask |= 0x1;  // funoid bit
      stackframe->layout = bm;
      // tail call the FUN
      STGJUMP0(getInfoPtr(stackframe->payload[0].op)->funFields.trueEntryCode);
  
    // excess < 0, too few args
    } else {
      PRINTF("stgApply PAP too few args\n");
      // invalidates argv, adjusts bitmap size (not used)
      stackframe = stgAdjustTopContSize(stackframe, argc);
      PtrOrLiteral *argv = stackframe->payload;
      // papargmap for new args
      bm.bitmap.mask <<= argc;
      bm.bits += papargmap.bits;
      // stgNewHeapPAP puts layout info at payload[fvCount]
      Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), bm);
      // copy fvs
      PRINTF("stgApply PAP inserting %d FVs into new PAP\n", fvCount);
      memcpy(&pap->payload[0], 
	     &argv[0].op->payload[0], 
	     fvCount * sizeof(PtrOrLiteral));
      // copy old args
      PRINTF("stgApply PAP inserting %d old args into new PAP\n", papargc);
      memcpy(&pap->payload[fvCount+1], 
	     &argv[0].op->payload[fvCount+1], 
	     papargc * sizeof(PtrOrLiteral));
      // copy new args to just after fvs, layout info, and old args
      PRINTF("stgApply PAP inserting %d new args into new PAP\n", argc);
      memcpy(&pap->payload[fvCount+1+papargc], &argv[1], argc * sizeof(PtrOrLiteral));
      stgCurVal = HOTOPL(pap);
      stgPopCont();
      STGRETURN0();
    } // if excess
  } // case PAP
  
  case BLACKHOLE: {
    PRINTF("infinite loop detected in stgApply!\n");
    showStgHeap();
    assert(0);
  } // case BLACKHOLE
  
  default:
    PRINTF("stgApply not a THUNK, FUN, or PAP\n");
    exit(0);
  }  // switch
}


  
