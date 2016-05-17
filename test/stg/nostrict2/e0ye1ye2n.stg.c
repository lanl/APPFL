#include "stgc.h"

void registerSOs();
FnPtr thunk_blackhole();
FnPtr fun_const();
FnPtr thunk_constBH();
FnPtr fun_seq();
FnPtr alts_55();
FnPtr thunk_main();

enum tycon_Unit {
  con_Unit };

enum tycon_Int {
  con_I };

enum tycon_List {
  con_Nil,
  con_Cons };

enum tycon_Bool {
  con_False,
  con_True };

enum tycon_Tupl2 {
  con_TP2 };

enum tycon_Tupl3 {
  con_TP3 };

InfoTab it_blackhole __attribute__((aligned(8))) = {.name ="blackhole",
                                                    .entryCode =
                                                    &thunk_blackhole, .objType =
                                                    THUNK,
                                                    .layoutInfo.payloadSize =1,
                                                    .layoutInfo.boxedCount =0,
                                                    .layoutInfo.unboxedCount =
                                                    0};

InfoTab it_const __attribute__((aligned(8))) = {.name ="const", .entryCode =
                                                &stg_funcall, .objType =FUN,
                                                .layoutInfo.payloadSize =0,
                                                .layoutInfo.boxedCount =0,
                                                .layoutInfo.unboxedCount =0,
                                                .funFields.arity =2,
                                                .funFields.trueEntryCode =
                                                fun_const};

InfoTab it_constBH __attribute__((aligned(8))) = {.name ="constBH", .entryCode =
                                                  &thunk_constBH, .objType =
                                                  THUNK,
                                                  .layoutInfo.payloadSize =1,
                                                  .layoutInfo.boxedCount =0,
                                                  .layoutInfo.unboxedCount =0};

InfoTab it_seq __attribute__((aligned(8))) = {.name ="seq", .entryCode =
                                              &stg_funcall, .objType =FUN,
                                              .layoutInfo.payloadSize =0,
                                              .layoutInfo.boxedCount =0,
                                              .layoutInfo.unboxedCount =0,
                                              .funFields.arity =2,
                                              .funFields.trueEntryCode =
                                              fun_seq};


CInfoTab it_alts_55 __attribute__((aligned(8))) = {.name ="alts_55",
                                                   .entryCode =&alts_55,
                                                   .contType =CASECONT,
                                                   .cLayoutInfo.payloadSize =2,
                                                   .cLayoutInfo.boxedCount =1,
                                                   .cLayoutInfo.unboxedCount =0,
                                                   .cLayoutInfo.bm =
                                                   576460752303423490};


InfoTab it_true __attribute__((aligned(8))) = {.name ="true", .entryCode =
                                               &stg_concall, .objType =CON,
                                               .layoutInfo.payloadSize =0,
                                               .layoutInfo.boxedCount =0,
                                               .layoutInfo.unboxedCount =0,
                                               .layoutInfo.permString ="",
                                               .conFields.arity =0,
                                               .conFields.tag =con_True,
                                               .conFields.conName ="True"};
InfoTab it_main __attribute__((aligned(8))) = {.name ="main", .entryCode =
                                               &thunk_main, .objType =THUNK,
                                               .layoutInfo.payloadSize =1,
                                               .layoutInfo.boxedCount =0,
                                               .layoutInfo.unboxedCount =0};

extern Obj sho_blackhole;
extern Obj sho_const;
extern Obj sho_constBH;
extern Obj sho_seq;
extern Obj sho_true;
extern Obj sho_main;

Obj sho_blackhole = {._infoPtr =&it_blackhole, .objType =THUNK, .ident =
                     "blackhole", .payload ={0}};
Obj sho_const = {._infoPtr =&it_const, .objType =FUN, .ident ="const",
                 .payload ={0}};
Obj sho_constBH = {._infoPtr =&it_constBH, .objType =THUNK, .ident ="constBH",
                   .payload ={0}};
Obj sho_seq = {._infoPtr =&it_seq, .objType =FUN, .ident ="seq", .payload ={0}};
Obj sho_true = {._infoPtr =&it_true, .objType =CON, .ident ="true", .payload =
                {}};
Obj sho_main = {._infoPtr =&it_main, .objType =THUNK, .ident ="main", .payload =
                {0}};

void registerSOs() {
  stgStatObj[stgStatObjCount++] = &sho_blackhole;
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_constBH;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// forall t0.t0
FnPtr thunk_blackhole() {
  LOG(LOG_INFO, "blackhole here\n");
  // access free vars through frame pointer for GC safety
  // is this really necessary???
  Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);
  stg_fp->layout = (Bitmap64)0x0400000000000001UL;
  stg_fp->payload[0] = stgCurVal;
  PtrOrLiteral *fvpp = &(stg_fp->payload[0]);
  stgThunk(stgCurVal);
  stgCurVal.op = NULL;
  stgCurVal = HOTOPL(&sho_blackhole)/* blackhole */; // blackhole
  // boxed EAtom, stgCurVal updates itself 
  STGJUMP();
}


// forall t2,t3.t3 -> t2 -> t3
// const(self, x, y)
FnPtr fun_const() {
  LOG(LOG_INFO, "const here\n");
  PtrOrLiteral *argp = &(stgGetStackArgp()->payload[0]);
  stgCurVal = argp[1]/* x */; // x
  // boxed EAtom, stgCurVal updates itself 
  STGJUMP();
}


// forall t16,t17.t16 -> t17
FnPtr thunk_constBH() {
  LOG(LOG_INFO, "constBH here\n");
  // access free vars through frame pointer for GC safety
  // is this really necessary???
  Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);
  stg_fp->layout = (Bitmap64)0x0400000000000001UL;
  stg_fp->payload[0] = stgCurVal;
  PtrOrLiteral *fvpp = &(stg_fp->payload[0]);
  stgThunk(stgCurVal);
  stgCurVal.op = NULL;
  { Cont *cp = stgAllocCallOrStackCont( &it_stgStackCont, 3);
    cp->layout = (Bitmap64)0x0C00000000000006UL;
    cp->payload[ 0 ] = ((PtrOrLiteral){.argType = INT, .i = 0 });
    cp->payload[ 1 ] = HOTOPL(&sho_const)/* const */;
    cp->payload[ 2 ] = HOTOPL(&sho_blackhole)/* blackhole */;
    // INDIRECT TAIL CALL const blackhole
    STGJUMP0(stgApplyNew);
  }
}


// forall t6,t7.t6 -> t7 -> t7
// seq(self, x, y)
FnPtr fun_seq() {
  LOG(LOG_INFO, "seq here\n");
  PtrOrLiteral *argp = &(stgGetStackArgp()->payload[0]);
  // scrutinee may STGJUMP or STGRETURN
  Cont *ccont_alts_55 = stgAllocCont( &it_alts_55);
  // dummy value for scrutinee, InfoTab initializes to unboxed
  ccont_alts_55->payload[0].i = 0;
  ccont_alts_55->payload[0].argType = INT;
  // load payload with FVs y
  ccont_alts_55->payload[1] = argp[2]/* y */; // y
  stgCurVal = argp[1]/* x */; // x
  // boxed EAtom, stgCurVal updates itself 
  STGJUMP();
  STGRETURN0();
}


// t7
FnPtr alts_55() {
  LOG(LOG_INFO, "alts_55 here\n");
  Cont *ccont_alts_55 = stgGetStackArgp();
  // make self-popping
  stgCaseToPopMe(ccont_alts_55);
  PtrOrLiteral *fvp = &(ccont_alts_55->payload[0]);
  fvp[0] = stgCurVal;
  ccont_alts_55->layout.bitmap.mask |= 0x1;
  // z ->
  stgCurVal = fvp[1]/* y */; // y
  // boxed EAtom, stgCurVal updates itself 
  STGJUMP();
}


// Bool[B] 
FnPtr thunk_main() {
  LOG(LOG_INFO, "main here\n");
  // access free vars through frame pointer for GC safety
  // is this really necessary???
  Cont *stg_fp = stgAllocCallOrStackCont(&it_stgStackCont, 1);
  stg_fp->layout = (Bitmap64)0x0400000000000001UL;
  stg_fp->payload[0] = stgCurVal;
  PtrOrLiteral *fvpp = &(stg_fp->payload[0]);
  stgThunk(stgCurVal);
  stgCurVal.op = NULL;
  { Cont *cp = stgAllocCallOrStackCont( &it_stgStackCont, 4);
    cp->layout = (Bitmap64)0x100000000000000EUL;
    cp->payload[ 0 ] = ((PtrOrLiteral){.argType = INT, .i = 0 });
    cp->payload[ 1 ] = HOTOPL(&sho_seq)/* seq */;
    cp->payload[ 2 ] = HOTOPL(&sho_constBH)/* constBH */;
    cp->payload[ 3 ] = HOTOPL(&sho_true)/* true */;
    // INDIRECT TAIL CALL seq constBH true
    STGJUMP0(stgApplyNew);
  }
}


FnPtr start() {
  Cont *showResultCont =   stgAllocCallOrStackCont(&it_stgShowResultCont, 0);
  stgCurVal.argType = HEAPOBJ;
  stgCurVal.op = &sho_main;
  STGJUMP0(getInfoPtr(stgCurVal.op)->entryCode);
}

int main (int argc, char **argv) {
  startCheck();
  parseArgs(argc, argv);
  initStg();
  initGc();
  registerSOs();
  CALL0_0(start);
  showStgHeap(LOG_DEBUG);
  GC();
  return 0;
}

