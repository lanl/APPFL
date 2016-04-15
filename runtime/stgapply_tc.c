#include "args.h"
#include "stg.h"
#include "heap.h"
#include "stack.h"
#include "stgutils.h"
#include "stgapply.h"

// this could also be done by creating just one new Cont
// for arity args, adjusting the old Cont, and shifting both
Cont *stgFunContSplit(int arity1, int excess, FnPtr (*dest)()) {
  // arity1 is arity of funoid
  // cont1 is cont to split
  Cont *cont1 = (Cont *)stgSP;
  // funoid + excess, subsequent application
  int pls2 = 1 + excess; 
  Cont *cont2 = stgAllocCallOrStackCont( &it_stgStackCont, pls2 );
  // funoid + arity1
  int pls3 = arity1 + 1;
  Cont *cont3 = stgAllocCallOrStackCont( &it_stgStackCont, pls3 );

  // fill in cont2
  Bitmap64 bm = cont1->layout;
  // bm.bitmap.size is set by alloc
  bm.bitmap.mask >>= arity1; // room for funoid, eliminate just right
  bm.bitmap.mask |= 0x1UL;   // funoid is boxed
  cont2->layout.bitmap.mask = bm.bitmap.mask;
  cont2->entryCode = dest;  // goto dest
  cont2->payload[0].op = NULL;  // no funoid yet
  #if USE_ARGTYPE
  cont2->payload[0].argType = HEAPOBJ;
  #endif
  memcpy(&cont2->payload[1],
         &cont1->payload[1 + arity1],
	 excess * sizeof(PtrOrLiteral));

  // fill in cont3
  bm = cont1->layout;
  bm.bitmap.size = pls3;  // mask can stay the same
  assert(cont3->layout.bitmap.size == pls3);
  cont3->layout = bm;
  memcpy(&cont3->payload[0],
         &cont1->payload[0],
	 pls3 * sizeof(PtrOrLiteral));

  // quash cont1
  int cont1Size = getContSize(cont1);
  memmove((char *)cont3 + cont1Size, 
	  cont3, 
	  getContSize(cont2) + getContSize(cont3));
  stgSP += cont1Size;
  return (Cont *)stgSP;
}

Cont *stgPapContSplit(int arity, 
		      int excess, 
		      FnPtr (*dest)(), // where apply with excess goes
		      PtrOrLiteral *papargv, // PAP args, not whole payload
                      Bitmap64 papargmap) // of the PAPs args
{ 
  Cont *contold = (Cont *)stgSP;
  Cont *contexcess = stgAllocCallOrStackCont( &it_stgStackCont, excess + 1);
  int papargc = papargmap.bitmap.size;
  int exactpls = 1 + papargc + arity;
  Cont *contexact = stgAllocCallOrStackCont( &it_stgStackCont, exactpls);

  // fill in contexcess
  Bitmap64 bm = contold->layout;
  bm.bitmap.mask >>= arity; // room for funoid, eliminate just right
  bm.bitmap.mask |= 0x1UL;   // funoid is boxed
  contexcess->layout.bitmap.mask = bm.bitmap.mask;
  contexcess->entryCode = dest;  // goto dest
  contexcess->payload[0].op = NULL;  // no funoid yet
  #if USE_ARGTYPE
  contexcess->payload[0].argType = HEAPOBJ;
  #endif
  memcpy(&contexcess->payload[1],
         &contold->payload[1 + arity],
	 excess * sizeof(PtrOrLiteral));

  // fill in contexact
  // make space for funoid
  papargmap.bitmap.mask <<= 1;
  papargmap.bitmap.mask |= 0x1UL;
  papargmap.bitmap.size += 1;

  // need first arity bits of bm less funoid
  Bitmap64 newbm = contold->layout;
  newbm.bitmap.mask >>= 1; // remove funoid
  // keep arity #bits, zero high bits to avoid overflow in combining
  newbm.bitmap.mask &= (0x1UL << arity) - 1;
  // make room for papargc args
  newbm.bitmap.mask <<= 1 + papargc;
  // arity new args
  newbm.bitmap.size = arity;
  // combine and insert
  contexact->layout.bits = papargmap.bits + newbm.bits;

  // args
  contexact->payload[0] = contold->payload[0];
  // copy old args
  memcpy(&contexact->payload[1],
	 &papargv[0],
	 papargc * sizeof(PtrOrLiteral));
  // copy new args
  memcpy(&contexact->payload[1 + papargc],
	 &contold->payload[1],
	 arity * sizeof(PtrOrLiteral));

   // quash oldcont
  int contoldsize = getContSize(contold);
  memmove((char *)contexact + contoldsize,
	  contexact, 
	  getContSize(contexcess) + getContSize(contexact));
  stgSP += contoldsize;
  return (Cont *)stgSP;
}


FnPtr stgApply1();
FnPtr stgApply2();
FnPtr stgApply3();


FnPtr stgApplyCurVal() {
  Cont *stackframe = stgGetStackArgp();
  stackframe->payload[0] = stgCurVal;
  stackframe->entryCode = &stgApply;
  STGRETURN0();
}

// hack for now, should do in codegen
FnPtr stgApply() {
  Cont *stackframe = stgGetStackArgp();
  int argc = stackframe->layout.bitmap.size;
  // extra param to stgApply2
  stackframe = stgAdjustTopContSize(stackframe, 1);
  // updates size but not mask
  stackframe->layout.bitmap.mask <<= 1; // new param is unboxed
  memmove(&stackframe->payload[1], 
	  &stackframe->payload[0], 
	  argc * sizeof(PtrOrLiteral));
  // arg index 0 (after new param) will have been forced
  stackframe->payload[0].i = 0;  
  #if USE_ARGTYPE
  stackframe->payload[0].argType = INT;
  #endif

  stackframe->entryCode = &stgApply2;
  stgCurVal = stackframe->payload[1];  // the funoid
  STGJUMP();  // bye bye
}

// eval the funoid first
FnPtr stgApplyNew() {
  Cont *stackframe = stgGetStackArgp();
  stackframe->entryCode = &stgApply2;
  stgCurVal = stackframe->payload[1];  // the funoid
  STGJUMP();  // bye bye
}

FnPtr stgApply2() {
  Cont *stackframe = stgGetStackArgp();
  derefStgCurVal();
  // capture something just evaluated
  int argvInd = stackframe->payload[0].i;
  // argv is old stgApply args (including funoid)
  PtrOrLiteral *argv = &stackframe->payload[1];
  // argvInd == 0 is funoid
  argv[argvInd] = stgCurVal;
  // any more?
  int argsToEval = 0;
  int appargc = stackframe->layout.bitmap.size - 2;  // skip new arg, funoid
  switch (evalStrategy) {
  case LAZY:
    argsToEval = 0;
    break;
  case STRICT1: {
    int arity = getInfoPtr(argv[0].op)->funFields.arity;
    if (getObjType(argv[0].op) == PAP) {
      int fvCount = getInfoPtr(argv[0].op)->layoutInfo.boxedCount +
	            getInfoPtr(argv[0].op)->layoutInfo.unboxedCount;
      Bitmap64 papargmap = argv[0].op->payload[fvCount].b;
      int papargc = papargmap.bitmap.size;
      arity -= papargc;
    } // if PAP
    int excess = appargc - arity;
    argsToEval = excess >= 0 ? arity : 0;
    break;
  case STRICT2:
    argsToEval = appargc;
    break;
  } // STRICT1
  } // switch

  for (argvInd++;  // next one
       argvInd <= argsToEval && // more left
	 !(stackframe->layout.bitmap.mask & (0x1 << (1+argvInd))); 
       argvInd++);  // skip to next

  if (argvInd <= argsToEval) {
    stackframe->payload[0].i = argvInd;
    stgCurVal = argv[argvInd];
    // tail call self after evaluating arg
    STGJUMP();
  }

  // fix stack frame for old stgApply
  int argc = stackframe->layout.bitmap.size;
  memmove(&stackframe->payload[0], 
	  &stackframe->payload[1], 
	  (argc - 1) * sizeof(PtrOrLiteral));
  // updates size but not mask
  stackframe = stgAdjustTopContSize(stackframe, -1); // elim extra param
  stackframe->layout.bitmap.mask >>= 1; 
  stackframe->entryCode = &stgApply3;
  STGRETURN0();
}

FnPtr stgApply3() {

  // STACKCONT with actual parameters
  Cont *stackframe = stgGetStackArgp();
  assert(getContType(stackframe) == STACKCONT);
  // back to normal
  stackframe->entryCode = &stgStackCont;

  // &argv[0] is args to stgApply, &argv[1] is args to funoid
  PtrOrLiteral *argv = stackframe->payload;
  Bitmap64 bm = stackframe->layout;
  // argc is number of arguments funoid applied to
  int argc = bm.bitmap.size - 1;

  LOG(LOG_INFO, "stgApply %s ", getInfoPtr(argv[0].op)->name);
  for (int i = 1; i != argc+1; i++) {
    showStgVal(LOG_INFO, argv[i]); LOG(LOG_INFO, " ");
  } LOG(LOG_INFO, "\n");

  switch (getObjType(argv[0].op)) {
  
  case FUN: {
    int arity = getInfoPtr(argv[0].op)->funFields.arity;
    LOG(LOG_INFO, "stgapply FUN %s arity %d\n",
           getInfoPtr(argv[0].op)->name,
           getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;  // may be negative
  
    // too many args?


    if (excess > 0) {
      // in the new scheme we need to split the stack frame into two,
      // and have an stgEval function that takes the funoid from stgCurVal

      LOG(LOG_INFO, "stgApply FUNPOS %d\n", excess);

      /**/
      // split current stack frame into two, one to tail call the FUN
      // the other to stgApplyCurVal to the excess args
      Cont *topCont = stgFunContSplit(arity, excess, &stgApplyCurVal);
      STGJUMP0(getInfoPtr(topCont->payload[0].op)->funFields.trueEntryCode);
      // don't want to put funoid in cont.entryCode and use STGRETURN0()
      // because funoid expects a self-popping stack cont

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

      Cont *topCont = stgPapContSplit(arity, 
				      excess, 
				      &stgApplyCurVal,
				      &argv[0].op->payload[fvCount + 1],
				      papargmap);
      STGJUMP0(getInfoPtr(topCont->payload[0].op)->funFields.trueEntryCode);

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


  
