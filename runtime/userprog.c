#include "stgc.h"
#include "stgApply.h"
void registerSHOs();
FnPtr fun__ile();
FnPtr fun_int();
FnPtr fun__intPrimop();
FnPtr alts_17();
FnPtr alts_18();
FnPtr alts_19();
FnPtr fun__intComp();
FnPtr alts_20();
FnPtr alts_21();
FnPtr fun_eqInt();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_seq();
FnPtr alts_55();
FnPtr fun_forcelist();
FnPtr alts_37();
FnPtr fun_rec_2();
FnPtr fun_head();
FnPtr alts_30();
FnPtr fun_alts_30_exhaust();
FnPtr fun_intLE();
FnPtr fun_minInt();
FnPtr alts_65();
FnPtr alts_66();
FnPtr alts_67();
FnPtr fun_rep();
FnPtr alts_62();
FnPtr alts_63();
FnPtr alts_64();
FnPtr fun_res1_0();
FnPtr fun_repminlist();
FnPtr fun_m_2();
FnPtr alts_60();
FnPtr fun_mlist_0();
FnPtr alts_61();
FnPtr fun_output();
FnPtr fun_result_6();
FnPtr fun_f_0();
FnPtr fun_hout();
FnPtr fun_main();
InfoTab it__ile = 
  { .name                = "_ile",
    .fvCount             = 0,
    .entryCode           = &fun__ile,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_int = 
  { .name                = "int",
    .fvCount             = 0,
    .entryCode           = &fun_int,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 1,
  };
InfoTab it_i_0 = 
  { .name                = "i_0",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 1,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it__intPrimop = 
  { .name                = "_intPrimop",
    .fvCount             = 0,
    .entryCode           = &fun__intPrimop,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 3,
  };
InfoTab it_alts_17 = 
  { .name                = "alts_17",
    .fvCount             = 2,
    .entryCode           = &alts_17,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_alts_18 = 
  { .name                = "alts_18",
    .fvCount             = 2,
    .entryCode           = &alts_18,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_alts_19 = 
  { .name                = "alts_19",
    .fvCount             = 0,
    .entryCode           = &alts_19,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_false = 
  { .name                = "false",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 0,
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "False",
  };
InfoTab it_true = 
  { .name                = "true",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 0,
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "True",
  };
InfoTab it__intComp = 
  { .name                = "_intComp",
    .fvCount             = 0,
    .entryCode           = &fun__intComp,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 3,
  };
InfoTab it_alts_20 = 
  { .name                = "alts_20",
    .fvCount             = 0,
    .entryCode           = &alts_20,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_alts_21 = 
  { .name                = "alts_21",
    .fvCount             = 0,
    .entryCode           = &alts_21,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_eqInt = 
  { .name                = "eqInt",
    .fvCount             = 0,
    .entryCode           = &fun_eqInt,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_2 = 
  { .name                = "alts_2",
    .fvCount             = 1,
    .entryCode           = &alts_2,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_alts_3 = 
  { .name                = "alts_3",
    .fvCount             = 1,
    .entryCode           = &alts_3,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_alts_4 = 
  { .name                = "alts_4",
    .fvCount             = 0,
    .entryCode           = &alts_4,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_error = 
  { .name                = "error",
    .fvCount             = 0,
    .entryCode           = &stg_error,
    .objType             = BLACKHOLE,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_seq = 
  { .name                = "seq",
    .fvCount             = 0,
    .entryCode           = &fun_seq,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_55 = 
  { .name                = "alts_55",
    .fvCount             = 1,
    .entryCode           = &alts_55,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 0,
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "Unit",
  };
InfoTab it_forcelist = 
  { .name                = "forcelist",
    .fvCount             = 0,
    .entryCode           = &fun_forcelist,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_37 = 
  { .name                = "alts_37",
    .fvCount             = 0,
    .entryCode           = &alts_37,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_rec_2 = 
  { .name                = "rec_2",
    .fvCount             = 1,
    .entryCode           = &fun_rec_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_head = 
  { .name                = "head",
    .fvCount             = 0,
    .entryCode           = &fun_head,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_30 = 
  { .name                = "alts_30",
    .fvCount             = 0,
    .entryCode           = &alts_30,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_alts_30_exhaust = 
  { .name                = "alts_30_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_30_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 0,
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "Nil",
  };
InfoTab it_two = 
  { .name                = "two",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 1,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_list2 = 
  { .name                = "list2",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 1,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_list12 = 
  { .name                = "list12",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_list212 = 
  { .name                = "list212",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_intLE = 
  { .name                = "intLE",
    .fvCount             = 0,
    .entryCode           = &fun_intLE,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_minInt = 
  { .name                = "minInt",
    .fvCount             = 0,
    .entryCode           = &fun_minInt,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_65 = 
  { .name                = "alts_65",
    .fvCount             = 2,
    .entryCode           = &alts_65,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_alts_66 = 
  { .name                = "alts_66",
    .fvCount             = 3,
    .entryCode           = &alts_66,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
  };
InfoTab it_alts_67 = 
  { .name                = "alts_67",
    .fvCount             = 2,
    .entryCode           = &alts_67,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_rep = 
  { .name                = "rep",
    .fvCount             = 0,
    .entryCode           = &fun_rep,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_62 = 
  { .name                = "alts_62",
    .fvCount             = 1,
    .entryCode           = &alts_62,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_res_2 = 
  { .name                = "res_2",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Pair",
  };
InfoTab it_alts_63 = 
  { .name                = "alts_63",
    .fvCount             = 3,
    .entryCode           = &alts_63,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
  };
InfoTab it_mlist_1 = 
  { .name                = "mlist_1",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_res_3 = 
  { .name                = "res_3",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Pair",
  };
InfoTab it_alts_64 = 
  { .name                = "alts_64",
    .fvCount             = 2,
    .entryCode           = &alts_64,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_res1_0 = 
  { .name                = "res1_0",
    .fvCount             = 2,
    .entryCode           = &fun_res1_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_res2_0 = 
  { .name                = "res2_0",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_res_4 = 
  { .name                = "res_4",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .layoutInfo.payloadSize = 2,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Pair",
  };
InfoTab it_repminlist = 
  { .name                = "repminlist",
    .fvCount             = 0,
    .entryCode           = &fun_repminlist,
    .objType             = FUN,
    .layoutInfo.payloadSize = 0,
    .funFields.arity     = 1,
  };
InfoTab it_m_2 = 
  { .name                = "m_2",
    .fvCount             = 1,
    .entryCode           = &fun_m_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_alts_60 = 
  { .name                = "alts_60",
    .fvCount             = 0,
    .entryCode           = &alts_60,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_mlist_0 = 
  { .name                = "mlist_0",
    .fvCount             = 2,
    .entryCode           = &fun_mlist_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
  };
InfoTab it_alts_61 = 
  { .name                = "alts_61",
    .fvCount             = 0,
    .entryCode           = &alts_61,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
  };
InfoTab it_output = 
  { .name                = "output",
    .fvCount             = 0,
    .entryCode           = &fun_output,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_result_6 = 
  { .name                = "result_6",
    .fvCount             = 0,
    .entryCode           = &fun_result_6,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_f_0 = 
  { .name                = "f_0",
    .fvCount             = 1,
    .entryCode           = &fun_f_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_hout = 
  { .name                = "hout",
    .fvCount             = 0,
    .entryCode           = &fun_hout,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &fun_main,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
  };

extern Obj sho__ile;
extern Obj sho_int;
extern Obj sho__intPrimop;
extern Obj sho_false;
extern Obj sho_true;
extern Obj sho__intComp;
extern Obj sho_eqInt;
extern Obj sho_error;
extern Obj sho_seq;
extern Obj sho_unit;
extern Obj sho_forcelist;
extern Obj sho_head;
extern Obj sho_nil;
extern Obj sho_two;
extern Obj sho_list2;
extern Obj sho_one;
extern Obj sho_list12;
extern Obj sho_list212;
extern Obj sho_intLE;
extern Obj sho_minInt;
extern Obj sho_rep;
extern Obj sho_repminlist;
extern Obj sho_output;
extern Obj sho_hout;
extern Obj sho_main;

Obj sho__ile =
{
  .infoPtr   = &it__ile,
  .objType   = FUN,
  .ident     = "_ile",
  .payload = {
    },
  };
Obj sho_int =
{
  .infoPtr   = &it_int,
  .objType   = FUN,
  .ident     = "int",
  .payload = {
    },
  };
Obj sho__intPrimop =
{
  .infoPtr   = &it__intPrimop,
  .objType   = FUN,
  .ident     = "_intPrimop",
  .payload = {
    },
  };
Obj sho_false =
{
  .infoPtr   = &it_false,
  .objType   = CON,
  .ident     = "false",
  .payload = {
    },
  };
Obj sho_true =
{
  .infoPtr   = &it_true,
  .objType   = CON,
  .ident     = "true",
  .payload = {
    },
  };
Obj sho__intComp =
{
  .infoPtr   = &it__intComp,
  .objType   = FUN,
  .ident     = "_intComp",
  .payload = {
    },
  };
Obj sho_eqInt =
{
  .infoPtr   = &it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  .payload = {
    },
  };
Obj sho_error =
{
  .infoPtr   = &it_error,
  .objType   = BLACKHOLE,
  .ident     = "error",
  .payload = {0}
  };
Obj sho_seq =
{
  .infoPtr   = &it_seq,
  .objType   = FUN,
  .ident     = "seq",
  .payload = {
    },
  };
Obj sho_unit =
{
  .infoPtr   = &it_unit,
  .objType   = CON,
  .ident     = "unit",
  .payload = {
    },
  };
Obj sho_forcelist =
{
  .infoPtr   = &it_forcelist,
  .objType   = FUN,
  .ident     = "forcelist",
  .payload = {
    },
  };
Obj sho_head =
{
  .infoPtr   = &it_head,
  .objType   = FUN,
  .ident     = "head",
  .payload = {
    },
  };
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
  .payload = {
    },
  };
Obj sho_two =
{
  .infoPtr   = &it_two,
  .objType   = CON,
  .ident     = "two",
  .payload = {
    {.argType = INT, .i = 2},
},
  };
Obj sho_list2 =
{
  .infoPtr   = &it_list2,
  .objType   = CON,
  .ident     = "list2",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_two},
    {.argType = HEAPOBJ, .op = &sho_nil},
},
  };
Obj sho_one =
{
  .infoPtr   = &it_one,
  .objType   = CON,
  .ident     = "one",
  .payload = {
    {.argType = INT, .i = 1},
},
  };
Obj sho_list12 =
{
  .infoPtr   = &it_list12,
  .objType   = CON,
  .ident     = "list12",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_one},
    {.argType = HEAPOBJ, .op = &sho_list2},
},
  };
Obj sho_list212 =
{
  .infoPtr   = &it_list212,
  .objType   = CON,
  .ident     = "list212",
  .payload = {
    {.argType = HEAPOBJ, .op = &sho_two},
    {.argType = HEAPOBJ, .op = &sho_list12},
},
  };
Obj sho_intLE =
{
  .infoPtr   = &it_intLE,
  .objType   = THUNK,
  .ident     = "intLE",
  .payload = {0}
  };
Obj sho_minInt =
{
  .infoPtr   = &it_minInt,
  .objType   = FUN,
  .ident     = "minInt",
  .payload = {
    },
  };
Obj sho_rep =
{
  .infoPtr   = &it_rep,
  .objType   = FUN,
  .ident     = "rep",
  .payload = {
    },
  };
Obj sho_repminlist =
{
  .infoPtr   = &it_repminlist,
  .objType   = FUN,
  .ident     = "repminlist",
  .payload = {
    },
  };
Obj sho_output =
{
  .infoPtr   = &it_output,
  .objType   = THUNK,
  .ident     = "output",
  .payload = {0}
  };
Obj sho_hout =
{
  .infoPtr   = &it_hout,
  .objType   = THUNK,
  .ident     = "hout",
  .payload = {0}
  };
Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  .payload = {0}
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho__ile;
  stgStatObj[stgStatObjCount++] = &sho_int;
  stgStatObj[stgStatObjCount++] = &sho__intPrimop;
  stgStatObj[stgStatObjCount++] = &sho_false;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho__intComp;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_head;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_list2;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_list12;
  stgStatObj[stgStatObjCount++] = &sho_list212;
  stgStatObj[stgStatObjCount++] = &sho_intLE;
  stgStatObj[stgStatObjCount++] = &sho_minInt;
  stgStatObj[stgStatObjCount++] = &sho_rep;
  stgStatObj[stgStatObjCount++] = &sho_repminlist;
  stgStatObj[stgStatObjCount++] = &sho_output;
  stgStatObj[stgStatObjCount++] = &sho_hout;
  stgStatObj[stgStatObjCount++] = &sho_main;
}


// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ile, self, a_h, b_h) {
  fprintf(stderr, "_ile here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i <= (b_h).i;
  fprintf(stderr, "_ile returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int_h[U]  -> Int[B] 
// (([],["i_h"]),([],[Int_h[U] ]))
DEFUN2(fun_int, self, i_h) {
  fprintf(stderr, "int here\n");
  Obj *i_0 = stgNewHeapObj( &it_i_0 );
  i_0->payload[0] = i_h; // i_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // i_0
  STGEVAL(stgCurVal);
  fprintf(stderr, "int returning\n");
  STGRETURN0();
  ENDFUN;
}

// (Int_h[U]  -> Int_h[U]  -> Int_h[U] ) -> Int[B]  -> Int[B]  -> Int[B] 
// ((["op","a","b"],[]),([Int_h[U]  -> Int_h[U]  -> Int_h[U] ,Int[B] ,Int[B] ],[]))
DEFUN4(fun__intPrimop, self, op, a, b) {
  fprintf(stderr, "_intPrimop here\n");
  Obj *ccont_alts_17 = stgAllocCont( &it_alts_17);
      // load payload with FVs b op
    ccont_alts_17->payload[0] = b; // b
    ccont_alts_17->payload[1] = op; // op
  stgCurVal = a; // a
  STGEVAL(stgCurVal);
  fprintf(stderr, "_intPrimop returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_17) {
  fprintf(stderr, "alts_17 here\n");
  Obj *ccont_alts_17 = stgPopCont();
  PtrOrLiteral b = ccont_alts_17->payload[0];
  PtrOrLiteral op = ccont_alts_17->payload[1];
  PtrOrLiteral scrut_alts_17 = stgCurVal;
  // I a_h ->
  Obj *ccont_alts_18 = stgAllocCont( &it_alts_18);
      // load payload with FVs a_h op
    ccont_alts_18->payload[0] = scrut_alts_17.op->payload[0]; // a_h
    ccont_alts_18->payload[1] = op; // op
  stgCurVal = b; // b
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_18) {
  fprintf(stderr, "alts_18 here\n");
  Obj *ccont_alts_18 = stgPopCont();
  PtrOrLiteral a_h = ccont_alts_18->payload[0];
  PtrOrLiteral op = ccont_alts_18->payload[1];
  PtrOrLiteral scrut_alts_18 = stgCurVal;
  // I b_h ->
  Obj *ccont_alts_19 = stgAllocCont( &it_alts_19);
      // no FVs
    // op a_h b_h
  STGAPPLYNN(op, a_h, scrut_alts_18.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_19) {
  fprintf(stderr, "alts_19 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_19 = stgCurVal;
  // r_h ->
  // int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_19);
  STGRETURN0();
  ENDFUN;
}


// (Int_h[U]  -> Int_h[U]  -> Int_h[U] ) -> Int[B]  -> Int[B]  -> Bool[B] 
// ((["op","a","b"],[]),([Int_h[U]  -> Int_h[U]  -> Int_h[U] ,Int[B] ,Int[B] ],[]))
DEFUN4(fun__intComp, self, op, a, b) {
  fprintf(stderr, "_intComp here\n");
  Obj *ccont_alts_20 = stgAllocCont( &it_alts_20);
      // no FVs
    // _intPrimop op a b
  STGAPPLYPPP(HOTOPL(&sho__intPrimop), op, a, b);
  fprintf(stderr, "_intComp returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_20) {
  fprintf(stderr, "alts_20 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_20 = stgCurVal;
  // I x_h ->
  Obj *ccont_alts_21 = stgAllocCont( &it_alts_21);
      // no FVs
    stgCurVal = scrut_alts_20.op->payload[0]; // x_h
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_21) {
  fprintf(stderr, "alts_21 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_21 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Bool[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  Obj *ccont_alts_2 = stgAllocCont( &it_alts_2);
      // load payload with FVs y
    ccont_alts_2->payload[0] = y; // y
  stgCurVal = x; // x
  STGEVAL(stgCurVal);
  fprintf(stderr, "eqInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  Obj *ccont_alts_2 = stgPopCont();
  PtrOrLiteral y = ccont_alts_2->payload[0];
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // I i_h ->
  Obj *ccont_alts_3 = stgAllocCont( &it_alts_3);
      // load payload with FVs i_h
    ccont_alts_3->payload[0] = scrut_alts_2.op->payload[0]; // i_h
  stgCurVal = y; // y
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  Obj *ccont_alts_3 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_3->payload[0];
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // I j_h ->
  Obj *ccont_alts_4 = stgAllocCont( &it_alts_4);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i == (scrut_alts_3.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_4) {
  fprintf(stderr, "alts_4 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t65,t66.t65 -> t66 -> t66
// ((["x","y"],[]),([t65,t66],[]))
DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  Obj *ccont_alts_55 = stgAllocCont( &it_alts_55);
      // load payload with FVs y
    ccont_alts_55->payload[0] = y; // y
  stgCurVal = x; // x
  STGEVAL(stgCurVal);
  fprintf(stderr, "seq returning\n");
  STGRETURN0();
  ENDFUN;
}

// t66
DEFUN0(alts_55) {
  fprintf(stderr, "alts_55 here\n");
  Obj *ccont_alts_55 = stgPopCont();
  PtrOrLiteral y = ccont_alts_55->payload[0];
  PtrOrLiteral scrut_alts_55 = stgCurVal;
  // z ->
  stgCurVal = y; // y
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// forall t152.List[B] t152 -> Unit[B] 
// ((["list"],[]),([List[B] t152],[]))
DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  Obj *ccont_alts_37 = stgAllocCont( &it_alts_37);
      // no FVs
    stgCurVal = list; // list
  STGEVAL(stgCurVal);
  fprintf(stderr, "forcelist returning\n");
  STGRETURN0();
  ENDFUN;
}

// Unit[B] 
DEFUN0(alts_37) {
  fprintf(stderr, "alts_37 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_37 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_unit); // unit
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *rec_2 = stgNewHeapObj( &it_rec_2 );
      rec_2->payload[0] = scrut_alts_37.op->payload[1]; // t
      // seq h rec_2
      STGAPPLYPP(HOTOPL(&sho_seq), scrut_alts_37.op->payload[0], HOTOPL((Obj *)STGHEAPAT(1,1)));
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Unit[B] 
DEFUN1(fun_rec_2, self) {
  fprintf(stderr, "rec_2 here\n");
  stgThunk(self);
  // forcelist t
  STGAPPLYP(HOTOPL(&sho_forcelist), self.op->payload[0]);
  fprintf(stderr, "rec_2 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t144.List[B] t144 -> t144
// ((["xs"],[]),([List[B] t144],[]))
DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  Obj *ccont_alts_30 = stgAllocCont( &it_alts_30);
      // no FVs
    stgCurVal = xs; // xs
  STGEVAL(stgCurVal);
  fprintf(stderr, "head returning\n");
  STGRETURN0();
  ENDFUN;
}

// t144
DEFUN0(alts_30) {
  fprintf(stderr, "alts_30 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_30 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_30.op->payload[0]; // hd
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_30_exhaust = stgNewHeapObj( &it_alts_30_exhaust );
      alts_30_exhaust->payload[0] = scrut_alts_30; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // alts_30_exhaust
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t50.t50
DEFUN1(fun_alts_30_exhaust, self) {
  fprintf(stderr, "alts_30_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  fprintf(stderr, "alts_30_exhaust returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Bool[B] 
DEFUN1(fun_intLE, self) {
  fprintf(stderr, "intLE here\n");
  stgThunk(self);
  // _intComp _ile
  STGAPPLYP(HOTOPL(&sho__intComp), HOTOPL(&sho__ile));
  fprintf(stderr, "intLE returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_minInt, self, x, y) {
  fprintf(stderr, "minInt here\n");
  Obj *ccont_alts_65 = stgAllocCont( &it_alts_65);
      // load payload with FVs x y
    ccont_alts_65->payload[0] = x; // x
    ccont_alts_65->payload[1] = y; // y
  stgCurVal = x; // x
  STGEVAL(stgCurVal);
  fprintf(stderr, "minInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_65) {
  fprintf(stderr, "alts_65 here\n");
  Obj *ccont_alts_65 = stgPopCont();
  PtrOrLiteral x = ccont_alts_65->payload[0];
  PtrOrLiteral y = ccont_alts_65->payload[1];
  PtrOrLiteral scrut_alts_65 = stgCurVal;
  // I i_h ->
  Obj *ccont_alts_66 = stgAllocCont( &it_alts_66);
      // load payload with FVs i_h x y
    ccont_alts_66->payload[0] = scrut_alts_65.op->payload[0]; // i_h
    ccont_alts_66->payload[1] = x; // x
    ccont_alts_66->payload[2] = y; // y
  stgCurVal = y; // y
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_66) {
  fprintf(stderr, "alts_66 here\n");
  Obj *ccont_alts_66 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_66->payload[0];
  PtrOrLiteral x = ccont_alts_66->payload[1];
  PtrOrLiteral y = ccont_alts_66->payload[2];
  PtrOrLiteral scrut_alts_66 = stgCurVal;
  // I j_h ->
  Obj *ccont_alts_67 = stgAllocCont( &it_alts_67);
      // load payload with FVs x y
    ccont_alts_67->payload[0] = x; // x
    ccont_alts_67->payload[1] = y; // y
  stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i <= (scrut_alts_66.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_67) {
  fprintf(stderr, "alts_67 here\n");
  Obj *ccont_alts_67 = stgPopCont();
  PtrOrLiteral x = ccont_alts_67->payload[0];
  PtrOrLiteral y = ccont_alts_67->payload[1];
  PtrOrLiteral scrut_alts_67 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = y; // y
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // q ->
    default: {
      stgCurVal = x; // x
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t155.t155 -> List[B] Int[B]  -> Pair[B] Int[B]  List[B] t155
// ((["m","xs"],[]),([t155,List[B] Int[B] ],[]))
DEFUN3(fun_rep, self, m, xs) {
  fprintf(stderr, "rep here\n");
  Obj *ccont_alts_62 = stgAllocCont( &it_alts_62);
      // load payload with FVs m
    ccont_alts_62->payload[0] = m; // m
  stgCurVal = xs; // xs
  STGEVAL(stgCurVal);
  fprintf(stderr, "rep returning\n");
  STGRETURN0();
  ENDFUN;
}

// Pair[B] Int[B]  List[B] t155
DEFUN0(alts_62) {
  fprintf(stderr, "alts_62 here\n");
  Obj *ccont_alts_62 = stgPopCont();
  PtrOrLiteral m = ccont_alts_62->payload[0];
  PtrOrLiteral scrut_alts_62 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      Obj *res_2 = stgNewHeapObj( &it_res_2 );
      res_2->payload[0] = HOTOPL(&sho_error); // error
      res_2->payload[1] = HOTOPL(&sho_nil); // nil
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // res_2
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons y ys ->
    case 1: {
      Obj *ccont_alts_63 = stgAllocCont( &it_alts_63);
          // load payload with FVs m y ys
        ccont_alts_63->payload[0] = m; // m
        ccont_alts_63->payload[1] = scrut_alts_62.op->payload[0]; // y
        ccont_alts_63->payload[2] = scrut_alts_62.op->payload[1]; // ys
      stgCurVal = scrut_alts_62.op->payload[1]; // ys
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Pair[B] Int[B]  List[B] t155
DEFUN0(alts_63) {
  fprintf(stderr, "alts_63 here\n");
  Obj *ccont_alts_63 = stgPopCont();
  PtrOrLiteral m = ccont_alts_63->payload[0];
  PtrOrLiteral y = ccont_alts_63->payload[1];
  PtrOrLiteral ys = ccont_alts_63->payload[2];
  PtrOrLiteral scrut_alts_63 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      Obj *mlist_1 = stgNewHeapObj( &it_mlist_1 );
      Obj *res_3 = stgNewHeapObj( &it_res_3 );
      mlist_1->payload[0] = m; // m
      mlist_1->payload[1] = HOTOPL(&sho_nil); // nil
      res_3->payload[0] = y; // y
      res_3->payload[1] = HOTOPL((Obj *)STGHEAPAT(4,2)); // mlist_1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // res_3
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // xxx ->
    default: {
      Obj *ccont_alts_64 = stgAllocCont( &it_alts_64);
          // load payload with FVs m y
        ccont_alts_64->payload[0] = m; // m
        ccont_alts_64->payload[1] = y; // y
      // rep m ys
      STGAPPLYPP(HOTOPL(&sho_rep), m, ys);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Pair[B] Int[B]  List[B] t155
DEFUN0(alts_64) {
  fprintf(stderr, "alts_64 here\n");
  Obj *ccont_alts_64 = stgPopCont();
  PtrOrLiteral m = ccont_alts_64->payload[0];
  PtrOrLiteral y = ccont_alts_64->payload[1];
  PtrOrLiteral scrut_alts_64 = stgCurVal;
  // Pair mp list ->
  Obj *res1_0 = stgNewHeapObj( &it_res1_0 );
  Obj *res2_0 = stgNewHeapObj( &it_res2_0 );
  Obj *res_4 = stgNewHeapObj( &it_res_4 );
  res1_0->payload[0] = scrut_alts_64.op->payload[0]; // mp
  res1_0->payload[1] = y; // y
  res2_0->payload[0] = m; // m
  res2_0->payload[1] = scrut_alts_64.op->payload[1]; // list
  res_4->payload[0] = HOTOPL((Obj *)STGHEAPAT(6,3)); // res1_0
  res_4->payload[1] = HOTOPL((Obj *)STGHEAPAT(4,2)); // res2_0
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // res_4
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_res1_0, self) {
  fprintf(stderr, "res1_0 here\n");
  stgThunk(self);
  // minInt mp y
  STGAPPLYPP(HOTOPL(&sho_minInt), self.op->payload[0], self.op->payload[1]);
  fprintf(stderr, "res1_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B]  -> List[B] Int[B] 
// ((["xs"],[]),([List[B] Int[B] ],[]))
DEFUN2(fun_repminlist, self, xs) {
  fprintf(stderr, "repminlist here\n");
  Obj *m_2 = stgNewHeapObj( &it_m_2 );
  Obj *mlist_0 = stgNewHeapObj( &it_mlist_0 );
  m_2->payload[0] = HOTOPL((Obj *)STGHEAPAT(2,1)); // mlist_0
  mlist_0->payload[0] = HOTOPL((Obj *)STGHEAPAT(3,2)); // m_2
  mlist_0->payload[1] = xs; // xs
  Obj *ccont_alts_61 = stgAllocCont( &it_alts_61);
      // no FVs
    stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // mlist_0
  STGEVAL(stgCurVal);
  fprintf(stderr, "repminlist returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_m_2, self) {
  fprintf(stderr, "m_2 here\n");
  stgThunk(self);
  Obj *ccont_alts_60 = stgAllocCont( &it_alts_60);
      // no FVs
    stgCurVal = self.op->payload[0]; // mlist_0
  STGEVAL(stgCurVal);
  fprintf(stderr, "m_2 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_60) {
  fprintf(stderr, "alts_60 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_60 = stgCurVal;
  // Pair m xxx ->
  stgCurVal = scrut_alts_60.op->payload[0]; // m
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Pair[B] Int[B]  List[B] Int[B] 
DEFUN1(fun_mlist_0, self) {
  fprintf(stderr, "mlist_0 here\n");
  stgThunk(self);
  // rep m_2 xs
  STGAPPLYPP(HOTOPL(&sho_rep), self.op->payload[0], self.op->payload[1]);
  fprintf(stderr, "mlist_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN0(alts_61) {
  fprintf(stderr, "alts_61 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_61 = stgCurVal;
  // Pair yyy list ->
  stgCurVal = scrut_alts_61.op->payload[1]; // list
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// List[B] Int[B] 
DEFUN1(fun_output, self) {
  fprintf(stderr, "output here\n");
  stgThunk(self);
  Obj *result_6 = stgNewHeapObj( &it_result_6 );
  Obj *f_0 = stgNewHeapObj( &it_f_0 );
  f_0->payload[0] = HOTOPL((Obj *)STGHEAPAT(2,2)); // result_6
  // seq f_0 result_6
  STGAPPLYPP(HOTOPL(&sho_seq), HOTOPL((Obj *)STGHEAPAT(1,1)), HOTOPL((Obj *)STGHEAPAT(2,2)));
  fprintf(stderr, "output returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_result_6, self) {
  fprintf(stderr, "result_6 here\n");
  stgThunk(self);
  // repminlist list212
  STGAPPLYP(HOTOPL(&sho_repminlist), HOTOPL(&sho_list212));
  fprintf(stderr, "result_6 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Unit[B] 
DEFUN1(fun_f_0, self) {
  fprintf(stderr, "f_0 here\n");
  stgThunk(self);
  // forcelist result_6
  STGAPPLYP(HOTOPL(&sho_forcelist), self.op->payload[0]);
  fprintf(stderr, "f_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_hout, self) {
  fprintf(stderr, "hout here\n");
  stgThunk(self);
  // head output
  STGAPPLYP(HOTOPL(&sho_head), HOTOPL(&sho_output));
  fprintf(stderr, "hout returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_main, self) {
  fprintf(stderr, "main here\n");
  stgThunk(self);
  // eqInt hout one
  STGAPPLYPP(HOTOPL(&sho_eqInt), HOTOPL(&sho_hout), HOTOPL(&sho_one));
  fprintf(stderr, "main returning\n");
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
  initStg();
  initCmm();
  initGc();
  CALL0_0(start);
  return 0;
}

