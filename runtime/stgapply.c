
FnPtr stgApply() {

  // STACKCONT with actual parameters
  Cont *stackframe = stgGetStackArgp();

  // argv[0] is args to stgApply, argv[1] is args to funoid
  PtrOrLiteral * const argv = stackframe->payload;
  Bitmap64 bm = stackframe->layout;

  // argc is number of arguments to funoid to be applied
  int argc = bm.bitmap.size - 1;

  // STACKCONT to be possibly constructed for excess args call
  Cont *newframe;
  Bitmap64 newbm;

  PRINTF("stgApply %s\n", getInfoPtr(argv[0].op)->name);
  argv[0].op = derefPoL(argv[0]);
  // this if just saves a possibly unneeded call
  if (getObjType(argv[0].op) == THUNK) {
    PRINTF("stgApplyN THUNK\n");
    STGEVAL(argv[0]);
    // this works because stgCurVal is a GC root
    argv[0].op = derefPoL(stgCurVal);
  } // if THUNK

  switch (getObjType(argv[0].op)) {
  
  case FUN: {
    int arity = getInfoPtr(argv[0].op)->funFields.arity;
    PRINTF("FUN %s arity %d\n", 
           getInfoPtr(argv[0].op)->name, 
           getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;  // may be negative
  
    // too many args?
    if (excess > 0) {
      PRINTF("stgApply FUN %d excess\n", excess);

      newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1 + arity );
      newbm = bm; 
      newbm.bitmap.size = 1 + arity; // high excess bits will be ignored
      newframe->layout = newbm;
      memcpy(newframe->payload, argv, (1+arity) * sizeof(PtrOrLiteral));
      // call-with-return the FUN
      PRINTF("stgApply CALLing  %s\n", getInfoPtr(argv[0].op)->name);
      STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);
      PRINTF("stgApplyNN back from CALLing  %s\n", getInfoPtr(argv[0].op)->name);

      // re-use existing stgApply frame
      // restore the funoid
      argv[0] = stgCurVal;
      // shift the args down
      memmove(&argv[1], &argv[1 + arity], arity * sizeof(PtrOrLiteral));
      // adjust the bitmap
      newbm = bm;
      // newbm.bitmap.size = 1 + excess; // stgAdjustContSize does this
      newbm.bitmap.mask >>= arity;  // arity + 1 - 1
      newbm.bitmap.mask |= 0x1;  // funoid is boxed
      stackframe->layout = newbm;
      // adjust stackframe size, invalidates argv, stackframe
      stackframe = stgAdjustTopContSize(stackframe, -arity); // units are PtrOrLiterals
      // try again - tail call stgApply
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
      newbm = bm;
      newbm.bitmap.mask >>= 1; // remove funoid
      newbm.bitmap.size -= 1;        
      Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), newbm);
      // copy fvs
      PRINTF("stgApply FUN inserting %d FVs into new PAP\n", fvCount);
      copyargs(&pap->payload[0], &argv[0].op->payload[0], fvCount);
      // copy args to just after fvs and layout info
      PRINTF("stgApply FUN inserting %d args into new PAP\n", argc);
      copyargs(&pap->payload[fvCount+1], &argv[1], argc);
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
    Bitmap64 newmap2;
    int papargc = papargmap.bitmap.size;
    int arity = getInfoPtr(argv[0].op)->funFields.arity - papargc;
    PRINTF("PAP/FUN %s arity %d\n", 
            getInfoPtr(argv[0].op)->name, 
            getInfoPtr(argv[0].op)->funFields.arity);
    int excess = argc - arity;    // may be negative
  
    // too many args?
    if (excess > 0) {
      PRINTF("stgApply PAP %d too many args\n", excess);

      // make space for funoid
      papargmap.bitmap.mask <<= 1;
      papargmap.bitmap.mask |= 0x1;
      papargmap.bitmap.size += 1;

      // "newmap2 = " ++ npStrToBMStr (take arity npstring) ++ ";\n" ++
      // need first arity bits of bm less funoid
      newmap2 = bm;
      newmap2.bitmap.mask >>= 1; // remove funoid
      newmap2.bitmap.mask <<= (papargc + 1);  // excess high bits ignored
      newmap2.bigmap.size = arity;
      // combine
      papargmap.bits += newmap2.bits;

      // new call
      newframe = stgAllocCallOrStackCont( &it_stgCallCont, 1+papargc+arity );
      newframe->layout = papargmap;

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
      STGCALL0(getInfoPtr(argv[0].op)->funFields.trueEntryCode);

      // stash the FUN-oid
      argv[0] = stgCurVal;
      newframe = stgAllocCallOrStackCont(&it_stgStackCont, 1 + excess);

      // newframe layout from bm
      // "newframe->layout = " ++ 
      // npStrToBMStr ('P' : drop arity npstring) ++ ";\n" ++
      newmap2 = bm;

      // drop used and funoid, add funoid back in
      newmap2.bitmap.mask >>= arity;
      newmap2.bitmap.mask |= 0x1;

      // fun-oid
      newframe->payload[0] = argv[0];

      // copy in excess args
      memcpy(&newframe->payload[1], 
             &argv[1+arity], 
             excess * sizeof(PtrOrLiteral));

      // try again - tail call stgApply 
      stgJumpAdjust();
      STGJUMP0(stgApply);
    } else 

    // just right?
    if (excess == 0) {
      PRINTF("stgApply PAP just right: %d args in PAP, %d new args\n", 
             papargc, arity);

      // make space for funoid
      papargmap.bitmap.mask <<= 1;
      papargmap.bitmap.mask |= 0x1;
      papargmap.bitmap.size += 1;


      newmap2 = (Bitmap64)0x0800000000000000UL;
      newmap2.bitmap.mask <<= (papargc + 1);
      papargmap.bits += newmap2.bits;
      newframe = stgAllocCallOrStackCont( &it_stgStackCont, papargc+1+arity );
      newframe->layout = papargmap;
      // self
      newframe->payload[0] = argv[0];
      // old args
      memcpy(&newframe->payload[1], &argv[0].op->payload[1 + fvCount], papargc * sizeof(PtrOrLiteral));
      // new args
      memcpy(&newframe->payload[1 + papargc], &argv[1], arity * sizeof(PtrOrLiteral));
      // stgJumpAdjust invalidates argv and newframe
      newframe = stgJumpAdjust();
      // tail call the FUN
      STGJUMP0(getInfoPtr(newframe->payload[0].op)->funFields.trueEntryCode);
  
    // excess < 0, too few args
    } else {
      PRINTF("stgApplyNN PAP too few args\n");
      // papargmap for new args
      Bitmap64 bmold = argv[0].op->payload[fvCount].b;
      Bitmap64 bmnew = (Bitmap64)0x0800000000000000UL;
      // shift mask by known only at runtime #existing PAP args
      bmnew.bitmap.mask <<= papargc;
      bmnew.bits += bmold.bits;
      // stgNewHeapPAP puts layout info at payload[fvCount]
      Obj *pap = stgNewHeapPAPmask(getInfoPtr(argv[0].op), bmnew);
      // copy fvs
      PRINTF("stgApply PAP inserting %d FVs into new PAP\n", fvCount);
      copyargs(&pap->payload[0], &argv[0].op->payload[0], fvCount);
      // copy old args
      PRINTF("stgApply PAP inserting %d old args into new PAP\n", papargc);
      copyargs(&pap->payload[fvCount+1], &argv[0].op->payload[fvCount+1], papargc);
      // copy new args to just after fvs, layout info, and old args
      PRINTF("stgApply PAP inserting 2 new args into new PAP\n");
      copyargs(&pap->payload[fvCount+1+papargc], &argv[1], 2);
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


  
