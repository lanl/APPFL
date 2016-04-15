#include "args.h"
#include "stg.h"
#include "heap.h"
#include "stack.h"
#include "stgutils.h"
#include "stgapply.h"

// might want to pass in bitmap and argv instead
void stgEvalStackFrameArgs(Cont *cp) {
  // don't evaluate the funoid
  int i = cp->layout.bitmap.size - 1;
  uintptr_t bits = (cp->layout.bitmap.mask >> 1);
  PtrOrLiteral *polp = &cp->payload[1];
  for ( ; i != 0; i--, polp++, bits >>= 1) {
    if (bits & 0x1) {
      STGEVAL(*polp);
      // stgCurValB
      polp->op = derefPoL(stgCurVal);
      stgCurVal.op = NULL;
    }
  }
}


FnPtr stgApply() {

  // STACKCONT with actual parameters
  Cont *stackframe = stgGetStackArgp();
  assert(getContType(stackframe) == STACKCONT);

  // &argv[0] is args to stgApply, &argv[1] is args to funoid
  PtrOrLiteral *argv = stackframe->payload;
  Bitmap64 bm = stackframe->layout;
  // argc is number of arguments funoid applied to
  int argc = bm.bitmap.size - 1;

  LOG(LOG_INFO, "stgApply %s ", getInfoPtr(argv[0].op)->name);
  for (int i = 1; i != argc+1; i++) {
    showStgVal(LOG_INFO, argv[i]); LOG(LOG_INFO, " ");
  } LOG(LOG_INFO, "\n");

  if (evalStrategy == STRICT1) stgEvalStackFrameArgs(stackframe);

  argv[0].op = derefPoL(argv[0]);
  // this if just saves a possibly unneeded call
  if (getObjType(argv[0].op) == THUNK) {
    LOG(LOG_INFO, "stgApply THUNK\n");
    STGEVAL(argv[0]);
    // argType already set
    // might be INDIRECT if no GC
    // argv[0].op = derefPoL(argv[0]);
    argv[0].op = derefPoL(stgCurVal);
    stgCurVal.op = NULL;
  } // if THUNK

  switch (getObjType(argv[0].op)) {
  
  case FUN: {
    int arity = getInfoPtr(argv[0].op)->funFields.arity;
    LOG(LOG_INFO, "stgapply FUN %s arity %d\n",
           getInfoPtr(argv[0].op)->name,
           getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;  // may be negative
  
    // too many args?
    if (excess > 0) {
      LOG(LOG_INFO, "stgApply FUNPOS %d\n", excess);

      // call with return FUN with arity args
      // funoid + arity payload
      { Cont *newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1 + arity );
      	Bitmap64 newbm = bm;
        // keep arity #bits, zero high bits to avoid overflow in combining
        newbm.bitmap.mask &= (0x1UL << (1 + arity)) - 1;
      	newbm.bitmap.size = 1 + arity;
      	newframe->layout = newbm;
      	memcpy(newframe->payload, argv, (1 + arity) * sizeof(PtrOrLiteral));
      	LOG(LOG_INFO,"stgApply CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      	STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);
      	LOG(LOG_INFO, "stgApply back from CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      } // scope

      // re-use existing stgApply frame
      // new funoid
      argv[0] = stgCurVal;
      stgCurVal.op = NULL;
      // shift excess args
      memmove(&argv[1], &argv[1 + arity], excess * sizeof(PtrOrLiteral));
      // adjust the bitmap
      bm.bitmap.mask >>= arity;  // arity + 1 - 1
      bm.bitmap.mask |= 0x1UL;  // funoid is boxed
      stackframe->layout = bm;
      // adjust stackframe size, invalidates argv, stackframe, updates bitmap.size
      stackframe = stgAdjustTopContSize(stackframe, -arity);
      // tail call stgApply
      STGJUMP0(stgApply);
    } else
  
    // just right?
    if (excess == 0) {
      LOG(LOG_INFO,"stgApply FUNEQ\n");
      // reuse call frame
      STGJUMP0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);

    // excess < 0, too few args
    } else {

      LOG(LOG_INFO, "stgApply FUNNEG %d\n", -excess);
      int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount +
                    getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;
      // stgNewHeapPAPmask puts layout info at payload[fvCount]
      bm.bitmap.mask >>= 1; // remove funoid
      bm.bitmap.size -= 1;
      Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), bm);
      // copy fvs
      LOG(LOG_INFO, "stgApply FUN inserting %d FVs into new PAP\n", fvCount);
      memcpy(&pap->payload[0],
             &argv[0].op->payload[0],
             fvCount * sizeof(PtrOrLiteral));
      // copy args to just after fvs and layout info
      LOG(LOG_INFO, "stgApply FUN inserting %d args into new PAP\n", argc);
      memcpy(&pap->payload[fvCount+1], &argv[1], argc * sizeof(PtrOrLiteral));
      // this is return value, don't NULLify
      stgCurVal = HOTOPL(pap);
      LOG(LOG_INFO, "new PAP:  "); showStgObj(LOG_INFO, pap);
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
    int arity = getInfoPtr(argv[0].op)->funFields.arity - papargc;
    LOG(LOG_INFO, "stgapply PAP/FUN %s arity %d\n",
            getInfoPtr(argv[0].op)->name,
            getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;    // may be negative
  
    // too many args?
    if (excess > 0) {
      LOG(LOG_INFO, "stgApply PAPPOS %d\n", excess);

      { Cont *newframe = stgAllocCallOrStackCont( &it_stgCallCont,
						  1 + arity + papargc);
        // make space for funoid
      	papargmap.bitmap.mask <<= 1;
      	papargmap.bitmap.mask |= 0x1UL;
      	papargmap.bitmap.size += 1;

      	// need first arity bits of bm less funoid
      	Bitmap64 newbm = bm;
      	newbm.bitmap.mask >>= 1; // remove funoid
        // keep arity #bits, zero high bits to avoid overflow in combining
        newbm.bitmap.mask &= (0x1UL << arity) - 1;
        // make room for papargc args
      	newbm.bitmap.mask <<= 1 + papargc;
        // arity new args
      	newbm.bitmap.size = arity;
      	// combine and insert
      	newframe->layout.bits = papargmap.bits + newbm.bits;

        // CALLCONT args
      	newframe->payload[0] = argv[0]; // self
      	// copy old args
      	memcpy(&newframe->payload[1],
	       &argv[0].op->payload[fvCount + 1],
      	       papargc * sizeof(PtrOrLiteral));
      	// copy new args
      	memcpy(&newframe->payload[1 + papargc],
      	       &argv[1],
      	       arity * sizeof(PtrOrLiteral));
      	// call-with-return the funoid
      	LOG(LOG_INFO, "stgApply CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      	STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);
      	LOG(LOG_INFO, "stgApply back from CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      } // scope
      // re-use existing stgApply frame
      // restore the funoid
      argv[0] = stgCurVal;
      stgCurVal.op = NULL;
      // shift the args down
      memmove(&argv[1], &argv[1 + arity], excess * sizeof(PtrOrLiteral));
      // adjust the bitmap
      // stgAdjustContSize will adjust the size
      bm.bitmap.mask >>= arity;  // arity + 1 - 1
      bm.bitmap.mask |= 0x1UL;  // funoid is boxed
      stackframe->layout = bm;
      // adjust stackframe size, invalidates argv, stackframe, updates bitmap.size
      stackframe = stgAdjustTopContSize(stackframe, -arity); // units are PtrOrLiterals
      // try again - tail call stgApply
      STGJUMP0(stgApply);
    } else

    // just right?
    if (excess == 0) {
      LOG(LOG_INFO, "stgApply PAPEQ: %d args in PAP, %d new args\n",
             papargc, argc);

      // re-use existing stgApply frame
      // grow stackframe, invalidates argv, stackframe
      stackframe = stgAdjustTopContSize(stackframe, papargc);
      PtrOrLiteral *argv = &stackframe->payload[0];
      // shift new args up
      memmove(&argv[1 + papargc],
	      &argv[1],
	      argc * sizeof(PtrOrLiteral));
      // copy old args in
      memcpy(&argv[1],
	     &argv[0].op->payload[fvCount + 1],
	     papargc * sizeof(PtrOrLiteral));
      // adjust bitmap
      bm.bitmap.mask <<= papargc; // overwrite funoid bit
      bm.bitmap.mask |= (papargmap.bitmap.mask << 1); // room for funoid bit
      bm.bitmap.mask |= 0x1UL;  // restore funoid bit
      bm.bitmap.size = 1 + papargc + argc;
      stackframe->layout = bm;
      // tail call the FUN
      STGJUMP0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);
  
    // excess < 0, too few args
    } else {
      LOG(LOG_INFO, "stgApply PAPNEG %d too few args\n", -excess);
      /* this should be correct, not tried
      bm.bitmap.mask >>= 1;  // zap funoid
      bm.bitmap.mask <<= papargc;  // make room for old args
      bm.bitmap.mask |= papargmap.bitmap.mask; // add in old args
      bm.bitmap.size = papargc + argc; // correct size
      // stgNewHeapPAP puts layout info at payload[fvCount]
      Obj *newpap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), bm);
      */
      
      papargmap.bitmap.mask |= ((bm.bitmap.mask >> 1) << papargc);
      papargmap.bitmap.size += argc;

      // stgNewHeapPAP puts layout info at payload[fvCount]
      Obj *newpap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), papargmap);

      // copy fvs
      LOG(LOG_INFO, "stgApply PAP inserting %d FVs into new PAP\n", fvCount);
      memcpy(&newpap->payload[0],
	     &argv[0].op->payload[0],
	     fvCount * sizeof(PtrOrLiteral));
      // copy old args
      LOG(LOG_INFO,"stgApply PAP inserting %d old args into new PAP\n", papargc);
      for (int i = 0; i != papargc; i++) {
	      LOG(LOG_INFO, "  ");
        showStgVal(LOG_INFO, argv[0].op->payload[fvCount+1+i]);
        LOG(LOG_INFO,"\n");
      }
      memcpy(&newpap->payload[fvCount+1],
	     &argv[0].op->payload[fvCount+1],
	     papargc * sizeof(PtrOrLiteral));

      // copy new args to just after fvs, layout info, and old args
      LOG(LOG_INFO, "stgApply PAP inserting %d new args into new PAP\n", argc);
      for (int i = 0; i != argc; i++) {
	      LOG(LOG_INFO, "  ");
        showStgVal(LOG_INFO, argv[i+1]);
        LOG(LOG_INFO, "\n");
      }
      memcpy(&newpap->payload[fvCount+1+papargc],
	     &argv[1],
	     argc * sizeof(PtrOrLiteral));

      LOG(LOG_INFO, "new PAP:  ");
      showStgObj(LOG_INFO, newpap);
      // this is return value
      stgCurVal = HOTOPL(newpap);
      stgPopCont();
      STGRETURN0();
    } // if excess
  } // case PAP
  
  case BLACKHOLE: {
    LOG(LOG_ERROR, "stgApply terminating on BLACKHOLE\n");
    showStgHeap(LOG_ERROR);
    exit(0);
  } // case BLACKHOLE
  
  default:
    LOG(LOG_ERROR, "stgApply not a FUN or PAP\n");
    exit(0);
  }  // switch
}


  
