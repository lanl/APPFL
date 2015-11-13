#include "stgc.h"
#include "stgApply.h"
void registerSHOs();
FnPtr fun__idiv();
FnPtr fun__ieq();
FnPtr fun__ige();
FnPtr fun__igt();
FnPtr fun__ile();
FnPtr fun__ilt();
FnPtr fun__imax();
FnPtr fun__imin();
FnPtr fun__imod();
FnPtr fun__imul();
FnPtr fun__ine();
FnPtr fun__ineg();
FnPtr fun_int();
FnPtr fun__intPrimop();
FnPtr alts_21();
FnPtr alts_22();
FnPtr alts_23();
FnPtr fun__intComp();
FnPtr alts_24();
FnPtr alts_25();
FnPtr fun__iplus();
FnPtr fun__isub();
FnPtr fun__length();
FnPtr alts_39();
FnPtr alts_40();
FnPtr fun_all();
FnPtr alts_55();
FnPtr alts_56();
FnPtr fun_any();
FnPtr alts_57();
FnPtr alts_58();
FnPtr fun_append();
FnPtr alts_32();
FnPtr fun_rec_0();
FnPtr fun_apply();
FnPtr fun_cons();
FnPtr fun_const();
FnPtr fun_divInt();
FnPtr alts_15();
FnPtr alts_16();
FnPtr alts_17();
FnPtr fun_eqInt();
FnPtr alts_3();
FnPtr alts_4();
FnPtr alts_5();
FnPtr fun_subInt();
FnPtr alts_12();
FnPtr alts_13();
FnPtr alts_14();
FnPtr fun_drop();
FnPtr alts_44();
FnPtr alts_45();
FnPtr fun_m_1();
FnPtr fun_filter();
FnPtr alts_53();
FnPtr fun_tail_0();
FnPtr alts_54();
FnPtr fun_foldl();
FnPtr alts_36();
FnPtr fun_newAcc_0();
FnPtr fun_foldr();
FnPtr alts_37();
FnPtr fun_res_1();
FnPtr fun_seq();
FnPtr alts_0();
FnPtr fun_forcelist();
FnPtr alts_41();
FnPtr fun_rec_2();
FnPtr fun_fst();
FnPtr alts_1();
FnPtr fun_gcd_h();
FnPtr alts_27();
FnPtr alts_28();
FnPtr fun_gcd();
FnPtr alts_29();
FnPtr alts_30();
FnPtr alts_31();
FnPtr fun_head();
FnPtr alts_34();
FnPtr fun_alts_34_exhaust();
FnPtr fun_null();
FnPtr alts_50();
FnPtr fun_init();
FnPtr alts_51();
FnPtr alts_52();
FnPtr fun_l_0();
FnPtr fun_intLE();
FnPtr fun_length();
FnPtr alts_38();
FnPtr fun_map();
FnPtr alts_33();
FnPtr fun_rec_1();
FnPtr fun_x_0();
FnPtr fun_minInt();
FnPtr alts_26();
FnPtr fun_modInt();
FnPtr alts_18();
FnPtr alts_19();
FnPtr alts_20();
FnPtr fun_multInt();
FnPtr alts_6();
FnPtr alts_7();
FnPtr alts_8();
FnPtr fun_plusInt();
FnPtr alts_9();
FnPtr alts_10();
FnPtr alts_11();
FnPtr fun_repeat();
FnPtr fun_next_0();
FnPtr fun_take();
FnPtr alts_42();
FnPtr alts_43();
FnPtr fun_m_0();
FnPtr fun_rec_3();
FnPtr fun_replicate();
FnPtr fun_list_0();
FnPtr fun_snd();
FnPtr alts_2();
FnPtr fun_strictList();
FnPtr alts_48();
FnPtr alts_49();
FnPtr fun_sum();
FnPtr fun_tail();
FnPtr alts_35();
FnPtr fun_alts_35_exhaust();
FnPtr fun_tupl2();
FnPtr fun_tupl3();
FnPtr fun_zipWith();
FnPtr alts_46();
FnPtr alts_47();
FnPtr fun_newHead_0();
FnPtr fun_newTail_0();
FnPtr fun_zip();
InfoTab it__idiv __attribute__((aligned(8))) = 
  { .name                = "_idiv",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__idiv,
  };
InfoTab it__ieq __attribute__((aligned(8))) = 
  { .name                = "_ieq",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__ieq,
  };
InfoTab it__ige __attribute__((aligned(8))) = 
  { .name                = "_ige",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__ige,
  };
InfoTab it__igt __attribute__((aligned(8))) = 
  { .name                = "_igt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__igt,
  };
InfoTab it__ile __attribute__((aligned(8))) = 
  { .name                = "_ile",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__ile,
  };
InfoTab it__ilt __attribute__((aligned(8))) = 
  { .name                = "_ilt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__ilt,
  };
InfoTab it__imax __attribute__((aligned(8))) = 
  { .name                = "_imax",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__imax,
  };
InfoTab it__imin __attribute__((aligned(8))) = 
  { .name                = "_imin",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__imin,
  };
InfoTab it__imod __attribute__((aligned(8))) = 
  { .name                = "_imod",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__imod,
  };
InfoTab it__imul __attribute__((aligned(8))) = 
  { .name                = "_imul",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__imul,
  };
InfoTab it__ine __attribute__((aligned(8))) = 
  { .name                = "_ine",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__ine,
  };
InfoTab it__ineg __attribute__((aligned(8))) = 
  { .name                = "_ineg",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun__ineg,
  };
InfoTab it_int __attribute__((aligned(8))) = 
  { .name                = "int",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_int,
  };
InfoTab it_i_0 __attribute__((aligned(8))) = 
  { .name                = "i_0",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it__intPrimop __attribute__((aligned(8))) = 
  { .name                = "_intPrimop",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun__intPrimop,
  };
InfoTab it_alts_21 __attribute__((aligned(8))) = 
  { .name                = "alts_21",
    // fvs [("b",Int[B] ),("op",Int_h[U]  -> Int_h[U]  -> Int_h[U] )]
    .entryCode           = &alts_21,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_22 __attribute__((aligned(8))) = 
  { .name                = "alts_22",
    // fvs [("op",Int_h[U]  -> Int_h[U]  -> Int_h[U] ),("a_h",Int_h[U] )]
    .entryCode           = &alts_22,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_23 __attribute__((aligned(8))) = 
  { .name                = "alts_23",
    // fvs []
    .entryCode           = &alts_23,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
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
InfoTab it__intComp __attribute__((aligned(8))) = 
  { .name                = "_intComp",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun__intComp,
  };
InfoTab it_alts_24 __attribute__((aligned(8))) = 
  { .name                = "alts_24",
    // fvs []
    .entryCode           = &alts_24,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_25 __attribute__((aligned(8))) = 
  { .name                = "alts_25",
    // fvs []
    .entryCode           = &alts_25,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it__iplus __attribute__((aligned(8))) = 
  { .name                = "_iplus",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__iplus,
  };
InfoTab it__isub __attribute__((aligned(8))) = 
  { .name                = "_isub",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__isub,
  };
InfoTab it__length __attribute__((aligned(8))) = 
  { .name                = "_length",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__length,
  };
InfoTab it_alts_39 __attribute__((aligned(8))) = 
  { .name                = "alts_39",
    // fvs [("ac_h",UBInt)]
    .entryCode           = &alts_39,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_40 __attribute__((aligned(8))) = 
  { .name                = "alts_40",
    // fvs [("t",List[B] t270)]
    .entryCode           = &alts_40,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_all __attribute__((aligned(8))) = 
  { .name                = "all",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_all,
  };
InfoTab it_alts_55 __attribute__((aligned(8))) = 
  { .name                = "alts_55",
    // fvs [("p",t393 -> Bool[B] )]
    .entryCode           = &alts_55,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_56 __attribute__((aligned(8))) = 
  { .name                = "alts_56",
    // fvs [("p",t393 -> Bool[B] ),("t",List[B] t393)]
    .entryCode           = &alts_56,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_any __attribute__((aligned(8))) = 
  { .name                = "any",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_any,
  };
InfoTab it_alts_57 __attribute__((aligned(8))) = 
  { .name                = "alts_57",
    // fvs [("p",t405 -> Bool[B] )]
    .entryCode           = &alts_57,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_58 __attribute__((aligned(8))) = 
  { .name                = "alts_58",
    // fvs [("p",t405 -> Bool[B] ),("t",List[B] t405)]
    .entryCode           = &alts_58,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_append __attribute__((aligned(8))) = 
  { .name                = "append",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_append,
  };
InfoTab it_alts_32 __attribute__((aligned(8))) = 
  { .name                = "alts_32",
    // fvs [("l2",List[B] t195)]
    .entryCode           = &alts_32,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_0 __attribute__((aligned(8))) = 
  { .name                = "rec_0",
    // fvs [("l2",List[B] t195),("tl",List[B] t195)]
    .entryCode           = &fun_rec_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_4 __attribute__((aligned(8))) = 
  { .name                = "result_4",
    // fvs [("hd",t195),("rec_0",List[B] t195)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_apply __attribute__((aligned(8))) = 
  { .name                = "apply",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_apply,
  };
InfoTab it_cons __attribute__((aligned(8))) = 
  { .name                = "cons",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_cons,
  };
InfoTab it_r_0 __attribute__((aligned(8))) = 
  { .name                = "r_0",
    // fvs [("h",t62),("t",List[B] t62)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_const __attribute__((aligned(8))) = 
  { .name                = "const",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_const,
  };
InfoTab it_divInt __attribute__((aligned(8))) = 
  { .name                = "divInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_divInt,
  };
InfoTab it_alts_15 __attribute__((aligned(8))) = 
  { .name                = "alts_15",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_15,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_16 __attribute__((aligned(8))) = 
  { .name                = "alts_16",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_16,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_17 __attribute__((aligned(8))) = 
  { .name                = "alts_17",
    // fvs []
    .entryCode           = &alts_17,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_3 __attribute__((aligned(8))) = 
  { .name                = "result_3",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_eqInt __attribute__((aligned(8))) = 
  { .name                = "eqInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_eqInt,
  };
InfoTab it_alts_3 __attribute__((aligned(8))) = 
  { .name                = "alts_3",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_3,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_4 __attribute__((aligned(8))) = 
  { .name                = "alts_4",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_4,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_5 __attribute__((aligned(8))) = 
  { .name                = "alts_5",
    // fvs []
    .entryCode           = &alts_5,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_nil __attribute__((aligned(8))) = 
  { .name                = "nil",
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
    .conFields.conName   = "Nil",
  };
InfoTab it_one __attribute__((aligned(8))) = 
  { .name                = "one",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_subInt __attribute__((aligned(8))) = 
  { .name                = "subInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_subInt,
  };
InfoTab it_alts_12 __attribute__((aligned(8))) = 
  { .name                = "alts_12",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_12,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_13 __attribute__((aligned(8))) = 
  { .name                = "alts_13",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_13,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_14 __attribute__((aligned(8))) = 
  { .name                = "alts_14",
    // fvs []
    .entryCode           = &alts_14,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_2 __attribute__((aligned(8))) = 
  { .name                = "result_2",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_zero __attribute__((aligned(8))) = 
  { .name                = "zero",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_drop __attribute__((aligned(8))) = 
  { .name                = "drop",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_drop,
  };
InfoTab it_alts_44 __attribute__((aligned(8))) = 
  { .name                = "alts_44",
    // fvs [("n",Int[B] ),("xs",List[B] t443)]
    .entryCode           = &alts_44,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_45 __attribute__((aligned(8))) = 
  { .name                = "alts_45",
    // fvs [("n",Int[B] )]
    .entryCode           = &alts_45,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_m_1 __attribute__((aligned(8))) = 
  { .name                = "m_1",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_m_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_eight __attribute__((aligned(8))) = 
  { .name                = "eight",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_error  __attribute__((aligned(8)))= 
  { .name                = "error",
    // fvs []
    .entryCode           = &stgBlackhole,
    .objType             = BLACKHOLE,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_filter __attribute__((aligned(8))) = 
  { .name                = "filter",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_filter,
  };
InfoTab it_alts_53 __attribute__((aligned(8))) = 
  { .name                = "alts_53",
    // fvs [("p",t456 -> Bool[B] )]
    .entryCode           = &alts_53,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_tail_0 __attribute__((aligned(8))) = 
  { .name                = "tail_0",
    // fvs [("p",t456 -> Bool[B] ),("t",List[B] t456)]
    .entryCode           = &fun_tail_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_54 __attribute__((aligned(8))) = 
  { .name                = "alts_54",
    // fvs [("h",t456),("tail_0",List[B] t456)]
    .entryCode           = &alts_54,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_five __attribute__((aligned(8))) = 
  { .name                = "five",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_foldl __attribute__((aligned(8))) = 
  { .name                = "foldl",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun_foldl,
  };
InfoTab it_alts_36 __attribute__((aligned(8))) = 
  { .name                = "alts_36",
    // fvs [("acc",t244),("f",t244 -> t238 -> t244)]
    .entryCode           = &alts_36,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_newAcc_0 __attribute__((aligned(8))) = 
  { .name                = "newAcc_0",
    // fvs [("acc",t244),("f",t244 -> t238 -> t244),("h",t238)]
    .entryCode           = &fun_newAcc_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_foldr __attribute__((aligned(8))) = 
  { .name                = "foldr",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun_foldr,
  };
InfoTab it_alts_37 __attribute__((aligned(8))) = 
  { .name                = "alts_37",
    // fvs [("f",t252 -> t259 -> t259),("sd",t259)]
    .entryCode           = &alts_37,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_res_1 __attribute__((aligned(8))) = 
  { .name                = "res_1",
    // fvs [("f",t252 -> t259 -> t259),("sd",t259),("t",List[B] t252)]
    .entryCode           = &fun_res_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_seq __attribute__((aligned(8))) = 
  { .name                = "seq",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_seq,
  };
InfoTab it_alts_0 __attribute__((aligned(8))) = 
  { .name                = "alts_0",
    // fvs [("y",t59)]
    .entryCode           = &alts_0,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_unit __attribute__((aligned(8))) = 
  { .name                = "unit",
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
    .conFields.conName   = "Unit",
  };
InfoTab it_forcelist __attribute__((aligned(8))) = 
  { .name                = "forcelist",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_forcelist,
  };
InfoTab it_alts_41 __attribute__((aligned(8))) = 
  { .name                = "alts_41",
    // fvs []
    .entryCode           = &alts_41,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_2 __attribute__((aligned(8))) = 
  { .name                = "rec_2",
    // fvs [("t",List[B] t451)]
    .entryCode           = &fun_rec_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_four __attribute__((aligned(8))) = 
  { .name                = "four",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_fst __attribute__((aligned(8))) = 
  { .name                = "fst",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_fst,
  };
InfoTab it_alts_1 __attribute__((aligned(8))) = 
  { .name                = "alts_1",
    // fvs []
    .entryCode           = &alts_1,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_gcd_h __attribute__((aligned(8))) = 
  { .name                = "gcd_h",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_gcd_h,
  };
InfoTab it_alts_27 __attribute__((aligned(8))) = 
  { .name                = "alts_27",
    // fvs [("a_h",UBInt),("b_h",Int_h[U] )]
    .entryCode           = &alts_27,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 2,
  };
InfoTab it_alts_28 __attribute__((aligned(8))) = 
  { .name                = "alts_28",
    // fvs [("b_h",Int_h[U] )]
    .entryCode           = &alts_28,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_gcd __attribute__((aligned(8))) = 
  { .name                = "gcd",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_gcd,
  };
InfoTab it_alts_29 __attribute__((aligned(8))) = 
  { .name                = "alts_29",
    // fvs [("b",Int[B] )]
    .entryCode           = &alts_29,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_30 __attribute__((aligned(8))) = 
  { .name                = "alts_30",
    // fvs [("a_h",Int_h[U] )]
    .entryCode           = &alts_30,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_31 __attribute__((aligned(8))) = 
  { .name                = "alts_31",
    // fvs []
    .entryCode           = &alts_31,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_head __attribute__((aligned(8))) = 
  { .name                = "head",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_head,
  };
InfoTab it_alts_34 __attribute__((aligned(8))) = 
  { .name                = "alts_34",
    // fvs []
    .entryCode           = &alts_34,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_34_exhaust __attribute__((aligned(8))) = 
  { .name                = "alts_34_exhaust",
    // fvs [("x",List[B] t439)]
    .entryCode           = &fun_alts_34_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_null __attribute__((aligned(8))) = 
  { .name                = "null",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_null,
  };
InfoTab it_alts_50 __attribute__((aligned(8))) = 
  { .name                = "alts_50",
    // fvs []
    .entryCode           = &alts_50,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_init __attribute__((aligned(8))) = 
  { .name                = "init",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_init,
  };
InfoTab it_alts_51 __attribute__((aligned(8))) = 
  { .name                = "alts_51",
    // fvs []
    .entryCode           = &alts_51,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_52 __attribute__((aligned(8))) = 
  { .name                = "alts_52",
    // fvs [("h",t455),("t",List[B] t455)]
    .entryCode           = &alts_52,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_l_0 __attribute__((aligned(8))) = 
  { .name                = "l_0",
    // fvs [("t",List[B] t455)]
    .entryCode           = &fun_l_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_intLE __attribute__((aligned(8))) = 
  { .name                = "intLE",
    // fvs []
    .entryCode           = &fun_intLE,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_length __attribute__((aligned(8))) = 
  { .name                = "length",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_length,
  };
InfoTab it_alts_38 __attribute__((aligned(8))) = 
  { .name                = "alts_38",
    // fvs []
    .entryCode           = &alts_38,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_map __attribute__((aligned(8))) = 
  { .name                = "map",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_map,
  };
InfoTab it_alts_33 __attribute__((aligned(8))) = 
  { .name                = "alts_33",
    // fvs [("f",t208 -> t438)]
    .entryCode           = &alts_33,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_1 __attribute__((aligned(8))) = 
  { .name                = "rec_1",
    // fvs [("f",t208 -> t438),("t",List[B] t208)]
    .entryCode           = &fun_rec_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_x_0 __attribute__((aligned(8))) = 
  { .name                = "x_0",
    // fvs [("f",t208 -> t438),("h",t208)]
    .entryCode           = &fun_x_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_res_0 __attribute__((aligned(8))) = 
  { .name                = "res_0",
    // fvs [("rec_1",List[B] t438),("x_0",t438)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_minInt __attribute__((aligned(8))) = 
  { .name                = "minInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_minInt,
  };
InfoTab it_alts_26 __attribute__((aligned(8))) = 
  { .name                = "alts_26",
    // fvs [("a",Int[B] ),("b",Int[B] )]
    .entryCode           = &alts_26,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_modInt __attribute__((aligned(8))) = 
  { .name                = "modInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_modInt,
  };
InfoTab it_alts_18 __attribute__((aligned(8))) = 
  { .name                = "alts_18",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_18,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_19 __attribute__((aligned(8))) = 
  { .name                = "alts_19",
    // fvs [("x_h",Int_h[U] )]
    .entryCode           = &alts_19,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_20 __attribute__((aligned(8))) = 
  { .name                = "alts_20",
    // fvs []
    .entryCode           = &alts_20,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_multInt __attribute__((aligned(8))) = 
  { .name                = "multInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_multInt,
  };
InfoTab it_alts_6 __attribute__((aligned(8))) = 
  { .name                = "alts_6",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_6,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_7 __attribute__((aligned(8))) = 
  { .name                = "alts_7",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_7,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_8 __attribute__((aligned(8))) = 
  { .name                = "alts_8",
    // fvs []
    .entryCode           = &alts_8,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_0 __attribute__((aligned(8))) = 
  { .name                = "result_0",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_nine __attribute__((aligned(8))) = 
  { .name                = "nine",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_plusInt __attribute__((aligned(8))) = 
  { .name                = "plusInt",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_plusInt,
  };
InfoTab it_alts_9 __attribute__((aligned(8))) = 
  { .name                = "alts_9",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_9,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_10 __attribute__((aligned(8))) = 
  { .name                = "alts_10",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_10,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_11 __attribute__((aligned(8))) = 
  { .name                = "alts_11",
    // fvs []
    .entryCode           = &alts_11,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_1 __attribute__((aligned(8))) = 
  { .name                = "result_1",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_repeat __attribute__((aligned(8))) = 
  { .name                = "repeat",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_repeat,
  };
InfoTab it_next_0 __attribute__((aligned(8))) = 
  { .name                = "next_0",
    // fvs [("x",t459)]
    .entryCode           = &fun_next_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_take __attribute__((aligned(8))) = 
  { .name                = "take",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_take,
  };
InfoTab it_alts_42 __attribute__((aligned(8))) = 
  { .name                = "alts_42",
    // fvs [("n",Int[B] ),("xs",List[B] t442)]
    .entryCode           = &alts_42,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_43 __attribute__((aligned(8))) = 
  { .name                = "alts_43",
    // fvs [("n",Int[B] )]
    .entryCode           = &alts_43,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_m_0 __attribute__((aligned(8))) = 
  { .name                = "m_0",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_m_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_3 __attribute__((aligned(8))) = 
  { .name                = "rec_3",
    // fvs [("m_0",Int[B] ),("tl",List[B] t442)]
    .entryCode           = &fun_rec_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_5 __attribute__((aligned(8))) = 
  { .name                = "result_5",
    // fvs [("hd",t442),("rec_3",List[B] t442)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_replicate __attribute__((aligned(8))) = 
  { .name                = "replicate",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_replicate,
  };
InfoTab it_list_0 __attribute__((aligned(8))) = 
  { .name                = "list_0",
    // fvs [("x",t465)]
    .entryCode           = &fun_list_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_seven __attribute__((aligned(8))) = 
  { .name                = "seven",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_six __attribute__((aligned(8))) = 
  { .name                = "six",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_snd __attribute__((aligned(8))) = 
  { .name                = "snd",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_snd,
  };
InfoTab it_alts_2 __attribute__((aligned(8))) = 
  { .name                = "alts_2",
    // fvs []
    .entryCode           = &alts_2,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_strictList __attribute__((aligned(8))) = 
  { .name                = "strictList",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_strictList,
  };
InfoTab it_alts_48 __attribute__((aligned(8))) = 
  { .name                = "alts_48",
    // fvs []
    .entryCode           = &alts_48,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_49 __attribute__((aligned(8))) = 
  { .name                = "alts_49",
    // fvs [("h",t453)]
    .entryCode           = &alts_49,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_sum __attribute__((aligned(8))) = 
  { .name                = "sum",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_sum,
  };
InfoTab it_tail __attribute__((aligned(8))) = 
  { .name                = "tail",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_tail,
  };
InfoTab it_alts_35 __attribute__((aligned(8))) = 
  { .name                = "alts_35",
    // fvs []
    .entryCode           = &alts_35,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_35_exhaust __attribute__((aligned(8))) = 
  { .name                = "alts_35_exhaust",
    // fvs [("x",List[B] t229)]
    .entryCode           = &fun_alts_35_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_ten __attribute__((aligned(8))) = 
  { .name                = "ten",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_three __attribute__((aligned(8))) = 
  { .name                = "three",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_tupl2 __attribute__((aligned(8))) = 
  { .name                = "tupl2",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_tupl2,
  };
InfoTab it_t_0 __attribute__((aligned(8))) = 
  { .name                = "t_0",
    // fvs [("a",t71),("b",t72)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "TP2",
  };
InfoTab it_tupl3 __attribute__((aligned(8))) = 
  { .name                = "tupl3",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun_tupl3,
  };
InfoTab it_t_1 __attribute__((aligned(8))) = 
  { .name                = "t_1",
    // fvs [("a",t89),("b",t90),("c",t91)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 3,
    // argPerm = [0,1,2]
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "012",
    .conFields.arity     = 3,
    .conFields.tag       = 0,
    .conFields.conName   = "TP3",
  };
InfoTab it_two __attribute__((aligned(8))) = 
  { .name                = "two",
    // fvs []
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 1,
    // argPerm = [0]
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
    .layoutInfo.permString   = "0",
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_zipWith __attribute__((aligned(8))) = 
  { .name                = "zipWith",
    // fvs []
    .entryCode           = &stg_funcall,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun_zipWith,
  };
InfoTab it_alts_46 __attribute__((aligned(8))) = 
  { .name                = "alts_46",
    // fvs [("f",t327 -> t330 -> t445),("list2",List[B] t330)]
    .entryCode           = &alts_46,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_47 __attribute__((aligned(8))) = 
  { .name                = "alts_47",
    // fvs [("f",t327 -> t330 -> t445),("h1",t327),("t1",List[B] t327)]
    .entryCode           = &alts_47,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_newHead_0 __attribute__((aligned(8))) = 
  { .name                = "newHead_0",
    // fvs [("f",t327 -> t330 -> t445),("h1",t327),("h2",t330)]
    .entryCode           = &fun_newHead_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_newTail_0 __attribute__((aligned(8))) = 
  { .name                = "newTail_0",
    // fvs [("f",t327 -> t330 -> t445),("t1",List[B] t327),("t2",List[B] t330)]
    .entryCode           = &fun_newTail_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_6 __attribute__((aligned(8))) = 
  { .name                = "result_6",
    // fvs [("newHead_0",t445),("newTail_0",List[B] t445)]
    .entryCode           = &stg_concall,
    .objType             = CON,
    .layoutInfo.payloadSize  = 2,
    // argPerm = [0,1]
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
    .layoutInfo.permString   = "01",
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_zip __attribute__((aligned(8))) = 
  { .name                = "zip",
    // fvs []
    .entryCode           = &fun_zip,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };

extern Obj sho__idiv;
extern Obj sho__ieq;
extern Obj sho__ige;
extern Obj sho__igt;
extern Obj sho__ile;
extern Obj sho__ilt;
extern Obj sho__imax;
extern Obj sho__imin;
extern Obj sho__imod;
extern Obj sho__imul;
extern Obj sho__ine;
extern Obj sho__ineg;
extern Obj sho_int;
extern Obj sho__intPrimop;
extern Obj sho_false;
extern Obj sho_true;
extern Obj sho__intComp;
extern Obj sho__iplus;
extern Obj sho__isub;
extern Obj sho__length;
extern Obj sho_all;
extern Obj sho_any;
extern Obj sho_append;
extern Obj sho_apply;
extern Obj sho_cons;
extern Obj sho_const;
extern Obj sho_divInt;
extern Obj sho_eqInt;
extern Obj sho_nil;
extern Obj sho_one;
extern Obj sho_subInt;
extern Obj sho_zero;
extern Obj sho_drop;
extern Obj sho_eight;
extern Obj sho_error;
extern Obj sho_filter;
extern Obj sho_five;
extern Obj sho_foldl;
extern Obj sho_foldr;
extern Obj sho_seq;
extern Obj sho_unit;
extern Obj sho_forcelist;
extern Obj sho_four;
extern Obj sho_fst;
extern Obj sho_gcd_h;
extern Obj sho_gcd;
extern Obj sho_head;
extern Obj sho_null;
extern Obj sho_init;
extern Obj sho_intLE;
extern Obj sho_length;
extern Obj sho_map;
extern Obj sho_minInt;
extern Obj sho_modInt;
extern Obj sho_multInt;
extern Obj sho_nine;
extern Obj sho_plusInt;
extern Obj sho_repeat;
extern Obj sho_take;
extern Obj sho_replicate;
extern Obj sho_seven;
extern Obj sho_six;
extern Obj sho_snd;
extern Obj sho_strictList;
extern Obj sho_sum;
extern Obj sho_tail;
extern Obj sho_ten;
extern Obj sho_three;
extern Obj sho_tupl2;
extern Obj sho_tupl3;
extern Obj sho_two;
extern Obj sho_zipWith;
extern Obj sho_zip;

Obj sho__idiv =
{
  .infoPtr   = (uintptr_t)&it__idiv,
  .objType   = FUN,
  .ident     = "_idiv",
  .payload = {
    },
};

Obj sho__ieq =
{
  .infoPtr   = (uintptr_t)&it__ieq,
  .objType   = FUN,
  .ident     = "_ieq",
  .payload = {
    },
};

Obj sho__ige =
{
  .infoPtr   = (uintptr_t)&it__ige,
  .objType   = FUN,
  .ident     = "_ige",
  .payload = {
    },
};

Obj sho__igt =
{
  .infoPtr   = (uintptr_t)&it__igt,
  .objType   = FUN,
  .ident     = "_igt",
  .payload = {
    },
};

Obj sho__ile =
{
  .infoPtr   = (uintptr_t)&it__ile,
  .objType   = FUN,
  .ident     = "_ile",
  .payload = {
    },
};

Obj sho__ilt =
{
  .infoPtr   = (uintptr_t)&it__ilt,
  .objType   = FUN,
  .ident     = "_ilt",
  .payload = {
    },
};

Obj sho__imax =
{
  .infoPtr   = (uintptr_t)&it__imax,
  .objType   = FUN,
  .ident     = "_imax",
  .payload = {
    },
};

Obj sho__imin =
{
  .infoPtr   = (uintptr_t)&it__imin,
  .objType   = FUN,
  .ident     = "_imin",
  .payload = {
    },
};

Obj sho__imod =
{
  .infoPtr   = (uintptr_t)&it__imod,
  .objType   = FUN,
  .ident     = "_imod",
  .payload = {
    },
};

Obj sho__imul =
{
  .infoPtr   = (uintptr_t)&it__imul,
  .objType   = FUN,
  .ident     = "_imul",
  .payload = {
    },
};

Obj sho__ine =
{
  .infoPtr   = (uintptr_t)&it__ine,
  .objType   = FUN,
  .ident     = "_ine",
  .payload = {
    },
};

Obj sho__ineg =
{
  .infoPtr   = (uintptr_t)&it__ineg,
  .objType   = FUN,
  .ident     = "_ineg",
  .payload = {
    },
};

Obj sho_int =
{
  .infoPtr   = (uintptr_t)&it_int,
  .objType   = FUN,
  .ident     = "int",
  .payload = {
    },
};

Obj sho__intPrimop =
{
  .infoPtr   = (uintptr_t)&it__intPrimop,
  .objType   = FUN,
  .ident     = "_intPrimop",
  .payload = {
    },
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

Obj sho__intComp =
{
  .infoPtr   = (uintptr_t)&it__intComp,
  .objType   = FUN,
  .ident     = "_intComp",
  .payload = {
    },
};

Obj sho__iplus =
{
  .infoPtr   = (uintptr_t)&it__iplus,
  .objType   = FUN,
  .ident     = "_iplus",
  .payload = {
    },
};

Obj sho__isub =
{
  .infoPtr   = (uintptr_t)&it__isub,
  .objType   = FUN,
  .ident     = "_isub",
  .payload = {
    },
};

Obj sho__length =
{
  .infoPtr   = (uintptr_t)&it__length,
  .objType   = FUN,
  .ident     = "_length",
  .payload = {
    },
};

Obj sho_all =
{
  .infoPtr   = (uintptr_t)&it_all,
  .objType   = FUN,
  .ident     = "all",
  .payload = {
    },
};

Obj sho_any =
{
  .infoPtr   = (uintptr_t)&it_any,
  .objType   = FUN,
  .ident     = "any",
  .payload = {
    },
};

Obj sho_append =
{
  .infoPtr   = (uintptr_t)&it_append,
  .objType   = FUN,
  .ident     = "append",
  .payload = {
    },
};

Obj sho_apply =
{
  .infoPtr   = (uintptr_t)&it_apply,
  .objType   = FUN,
  .ident     = "apply",
  .payload = {
    },
};

Obj sho_cons =
{
  .infoPtr   = (uintptr_t)&it_cons,
  .objType   = FUN,
  .ident     = "cons",
  .payload = {
    },
};

Obj sho_const =
{
  .infoPtr   = (uintptr_t)&it_const,
  .objType   = FUN,
  .ident     = "const",
  .payload = {
    },
};

Obj sho_divInt =
{
  .infoPtr   = (uintptr_t)&it_divInt,
  .objType   = FUN,
  .ident     = "divInt",
  .payload = {
    },
};

Obj sho_eqInt =
{
  .infoPtr   = (uintptr_t)&it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  .payload = {
    },
};

Obj sho_nil =
{
  .infoPtr   = (uintptr_t)&it_nil,
  .objType   = CON,
  .ident     = "nil",
  .payload = {
    },
};

Obj sho_one =
{
  .infoPtr   = (uintptr_t)&it_one,
  .objType   = CON,
  .ident     = "one",
  .payload = {
    {.argType = INT, .i = 1},
},
};

Obj sho_subInt =
{
  .infoPtr   = (uintptr_t)&it_subInt,
  .objType   = FUN,
  .ident     = "subInt",
  .payload = {
    },
};

Obj sho_zero =
{
  .infoPtr   = (uintptr_t)&it_zero,
  .objType   = CON,
  .ident     = "zero",
  .payload = {
    {.argType = INT, .i = 0},
},
};

Obj sho_drop =
{
  .infoPtr   = (uintptr_t)&it_drop,
  .objType   = FUN,
  .ident     = "drop",
  .payload = {
    },
};

Obj sho_eight =
{
  .infoPtr   = (uintptr_t)&it_eight,
  .objType   = CON,
  .ident     = "eight",
  .payload = {
    {.argType = INT, .i = 8},
},
};

Obj sho_error =
{
  .infoPtr   = (uintptr_t)&it_error,
  .objType   = BLACKHOLE,
  .ident     = "error",
  .payload = {0}
};

Obj sho_filter =
{
  .infoPtr   = (uintptr_t)&it_filter,
  .objType   = FUN,
  .ident     = "filter",
  .payload = {
    },
};

Obj sho_five =
{
  .infoPtr   = (uintptr_t)&it_five,
  .objType   = CON,
  .ident     = "five",
  .payload = {
    {.argType = INT, .i = 5},
},
};

Obj sho_foldl =
{
  .infoPtr   = (uintptr_t)&it_foldl,
  .objType   = FUN,
  .ident     = "foldl",
  .payload = {
    },
};

Obj sho_foldr =
{
  .infoPtr   = (uintptr_t)&it_foldr,
  .objType   = FUN,
  .ident     = "foldr",
  .payload = {
    },
};

Obj sho_seq =
{
  .infoPtr   = (uintptr_t)&it_seq,
  .objType   = FUN,
  .ident     = "seq",
  .payload = {
    },
};

Obj sho_unit =
{
  .infoPtr   = (uintptr_t)&it_unit,
  .objType   = CON,
  .ident     = "unit",
  .payload = {
    },
};

Obj sho_forcelist =
{
  .infoPtr   = (uintptr_t)&it_forcelist,
  .objType   = FUN,
  .ident     = "forcelist",
  .payload = {
    },
};

Obj sho_four =
{
  .infoPtr   = (uintptr_t)&it_four,
  .objType   = CON,
  .ident     = "four",
  .payload = {
    {.argType = INT, .i = 4},
},
};

Obj sho_fst =
{
  .infoPtr   = (uintptr_t)&it_fst,
  .objType   = FUN,
  .ident     = "fst",
  .payload = {
    },
};

Obj sho_gcd_h =
{
  .infoPtr   = (uintptr_t)&it_gcd_h,
  .objType   = FUN,
  .ident     = "gcd_h",
  .payload = {
    },
};

Obj sho_gcd =
{
  .infoPtr   = (uintptr_t)&it_gcd,
  .objType   = FUN,
  .ident     = "gcd",
  .payload = {
    },
};

Obj sho_head =
{
  .infoPtr   = (uintptr_t)&it_head,
  .objType   = FUN,
  .ident     = "head",
  .payload = {
    },
};

Obj sho_null =
{
  .infoPtr   = (uintptr_t)&it_null,
  .objType   = FUN,
  .ident     = "null",
  .payload = {
    },
};

Obj sho_init =
{
  .infoPtr   = (uintptr_t)&it_init,
  .objType   = FUN,
  .ident     = "init",
  .payload = {
    },
};

Obj sho_intLE =
{
  .infoPtr   = (uintptr_t)&it_intLE,
  .objType   = THUNK,
  .ident     = "intLE",
  .payload = {0}
};

Obj sho_length =
{
  .infoPtr   = (uintptr_t)&it_length,
  .objType   = FUN,
  .ident     = "length",
  .payload = {
    },
};

Obj sho_map =
{
  .infoPtr   = (uintptr_t)&it_map,
  .objType   = FUN,
  .ident     = "map",
  .payload = {
    },
};

Obj sho_minInt =
{
  .infoPtr   = (uintptr_t)&it_minInt,
  .objType   = FUN,
  .ident     = "minInt",
  .payload = {
    },
};

Obj sho_modInt =
{
  .infoPtr   = (uintptr_t)&it_modInt,
  .objType   = FUN,
  .ident     = "modInt",
  .payload = {
    },
};

Obj sho_multInt =
{
  .infoPtr   = (uintptr_t)&it_multInt,
  .objType   = FUN,
  .ident     = "multInt",
  .payload = {
    },
};

Obj sho_nine =
{
  .infoPtr   = (uintptr_t)&it_nine,
  .objType   = CON,
  .ident     = "nine",
  .payload = {
    {.argType = INT, .i = 9},
},
};

Obj sho_plusInt =
{
  .infoPtr   = (uintptr_t)&it_plusInt,
  .objType   = FUN,
  .ident     = "plusInt",
  .payload = {
    },
};

Obj sho_repeat =
{
  .infoPtr   = (uintptr_t)&it_repeat,
  .objType   = FUN,
  .ident     = "repeat",
  .payload = {
    },
};

Obj sho_take =
{
  .infoPtr   = (uintptr_t)&it_take,
  .objType   = FUN,
  .ident     = "take",
  .payload = {
    },
};

Obj sho_replicate =
{
  .infoPtr   = (uintptr_t)&it_replicate,
  .objType   = FUN,
  .ident     = "replicate",
  .payload = {
    },
};

Obj sho_seven =
{
  .infoPtr   = (uintptr_t)&it_seven,
  .objType   = CON,
  .ident     = "seven",
  .payload = {
    {.argType = INT, .i = 7},
},
};

Obj sho_six =
{
  .infoPtr   = (uintptr_t)&it_six,
  .objType   = CON,
  .ident     = "six",
  .payload = {
    {.argType = INT, .i = 6},
},
};

Obj sho_snd =
{
  .infoPtr   = (uintptr_t)&it_snd,
  .objType   = FUN,
  .ident     = "snd",
  .payload = {
    },
};

Obj sho_strictList =
{
  .infoPtr   = (uintptr_t)&it_strictList,
  .objType   = FUN,
  .ident     = "strictList",
  .payload = {
    },
};

Obj sho_sum =
{
  .infoPtr   = (uintptr_t)&it_sum,
  .objType   = FUN,
  .ident     = "sum",
  .payload = {
    },
};

Obj sho_tail =
{
  .infoPtr   = (uintptr_t)&it_tail,
  .objType   = FUN,
  .ident     = "tail",
  .payload = {
    },
};

Obj sho_ten =
{
  .infoPtr   = (uintptr_t)&it_ten,
  .objType   = CON,
  .ident     = "ten",
  .payload = {
    {.argType = INT, .i = 10},
},
};

Obj sho_three =
{
  .infoPtr   = (uintptr_t)&it_three,
  .objType   = CON,
  .ident     = "three",
  .payload = {
    {.argType = INT, .i = 3},
},
};

Obj sho_tupl2 =
{
  .infoPtr   = (uintptr_t)&it_tupl2,
  .objType   = FUN,
  .ident     = "tupl2",
  .payload = {
    },
};

Obj sho_tupl3 =
{
  .infoPtr   = (uintptr_t)&it_tupl3,
  .objType   = FUN,
  .ident     = "tupl3",
  .payload = {
    },
};

Obj sho_two =
{
  .infoPtr   = (uintptr_t)&it_two,
  .objType   = CON,
  .ident     = "two",
  .payload = {
    {.argType = INT, .i = 2},
},
};

Obj sho_zipWith =
{
  .infoPtr   = (uintptr_t)&it_zipWith,
  .objType   = FUN,
  .ident     = "zipWith",
  .payload = {
    },
};

Obj sho_zip =
{
  .infoPtr   = (uintptr_t)&it_zip,
  .objType   = THUNK,
  .ident     = "zip",
  .payload = {0}
};

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho__idiv;
  stgStatObj[stgStatObjCount++] = &sho__ieq;
  stgStatObj[stgStatObjCount++] = &sho__ige;
  stgStatObj[stgStatObjCount++] = &sho__igt;
  stgStatObj[stgStatObjCount++] = &sho__ile;
  stgStatObj[stgStatObjCount++] = &sho__ilt;
  stgStatObj[stgStatObjCount++] = &sho__imax;
  stgStatObj[stgStatObjCount++] = &sho__imin;
  stgStatObj[stgStatObjCount++] = &sho__imod;
  stgStatObj[stgStatObjCount++] = &sho__imul;
  stgStatObj[stgStatObjCount++] = &sho__ine;
  stgStatObj[stgStatObjCount++] = &sho__ineg;
  stgStatObj[stgStatObjCount++] = &sho_int;
  stgStatObj[stgStatObjCount++] = &sho__intPrimop;
  stgStatObj[stgStatObjCount++] = &sho_false;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho__intComp;
  stgStatObj[stgStatObjCount++] = &sho__iplus;
  stgStatObj[stgStatObjCount++] = &sho__isub;
  stgStatObj[stgStatObjCount++] = &sho__length;
  stgStatObj[stgStatObjCount++] = &sho_all;
  stgStatObj[stgStatObjCount++] = &sho_any;
  stgStatObj[stgStatObjCount++] = &sho_append;
  stgStatObj[stgStatObjCount++] = &sho_apply;
  stgStatObj[stgStatObjCount++] = &sho_cons;
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_divInt;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_drop;
  stgStatObj[stgStatObjCount++] = &sho_eight;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_filter;
  stgStatObj[stgStatObjCount++] = &sho_five;
  stgStatObj[stgStatObjCount++] = &sho_foldl;
  stgStatObj[stgStatObjCount++] = &sho_foldr;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_four;
  stgStatObj[stgStatObjCount++] = &sho_fst;
  stgStatObj[stgStatObjCount++] = &sho_gcd_h;
  stgStatObj[stgStatObjCount++] = &sho_gcd;
  stgStatObj[stgStatObjCount++] = &sho_head;
  stgStatObj[stgStatObjCount++] = &sho_null;
  stgStatObj[stgStatObjCount++] = &sho_init;
  stgStatObj[stgStatObjCount++] = &sho_intLE;
  stgStatObj[stgStatObjCount++] = &sho_length;
  stgStatObj[stgStatObjCount++] = &sho_map;
  stgStatObj[stgStatObjCount++] = &sho_minInt;
  stgStatObj[stgStatObjCount++] = &sho_modInt;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_nine;
  stgStatObj[stgStatObjCount++] = &sho_plusInt;
  stgStatObj[stgStatObjCount++] = &sho_repeat;
  stgStatObj[stgStatObjCount++] = &sho_take;
  stgStatObj[stgStatObjCount++] = &sho_replicate;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_six;
  stgStatObj[stgStatObjCount++] = &sho_snd;
  stgStatObj[stgStatObjCount++] = &sho_strictList;
  stgStatObj[stgStatObjCount++] = &sho_sum;
  stgStatObj[stgStatObjCount++] = &sho_tail;
  stgStatObj[stgStatObjCount++] = &sho_ten;
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_tupl2;
  stgStatObj[stgStatObjCount++] = &sho_tupl3;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_zipWith;
  stgStatObj[stgStatObjCount++] = &sho_zip;
}


// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__idiv, self, a_h, b_h) {
  fprintf(stderr, "_idiv here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i / (b_h).i;
  fprintf(stderr, "_idiv returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ieq, self, a_h, b_h) {
  fprintf(stderr, "_ieq here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i == (b_h).i;
  fprintf(stderr, "_ieq returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ige, self, a_h, b_h) {
  fprintf(stderr, "_ige here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i >= (b_h).i;
  fprintf(stderr, "_ige returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__igt, self, a_h, b_h) {
  fprintf(stderr, "_igt here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i > (b_h).i;
  fprintf(stderr, "_igt returning\n");
  STGRETURN0();
  ENDFUN;
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

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ilt, self, a_h, b_h) {
  fprintf(stderr, "_ilt here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i < (b_h).i;
  fprintf(stderr, "_ilt returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imax, self, a_h, b_h) {
  fprintf(stderr, "_imax here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = imax((a_h).i, (b_h).i);
  fprintf(stderr, "_imax returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imin, self, a_h, b_h) {
  fprintf(stderr, "_imin here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = imin((a_h).i, (b_h).i);
  fprintf(stderr, "_imin returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imod, self, a_h, b_h) {
  fprintf(stderr, "_imod here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i % (b_h).i;
  fprintf(stderr, "_imod returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imul, self, a_h, b_h) {
  fprintf(stderr, "_imul here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i * (b_h).i;
  fprintf(stderr, "_imul returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ine, self, a_h, b_h) {
  fprintf(stderr, "_ine here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i != (b_h).i;
  fprintf(stderr, "_ine returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt
// (([],["a_h"]),([],[UBInt]))
DEFUN2(fun__ineg, self, a_h) {
  fprintf(stderr, "_ineg here\n");
  stgCurVal.argType = INT;
  stgCurVal.i =  -(a_h).i;
  fprintf(stderr, "_ineg returning\n");
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
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "int returning\n");
  STGRETURN0();
  ENDFUN;
}

// (Int_h[U]  -> Int_h[U]  -> Int_h[U] ) -> Int[B]  -> Int[B]  -> Int[B] 
// ((["op","a","b"],[]),([Int_h[U]  -> Int_h[U]  -> Int_h[U] ,Int[B] ,Int[B] ],[]))
DEFUN4(fun__intPrimop, self, op, a, b) {
  fprintf(stderr, "_intPrimop here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_21 = stgAllocCont( &it_alts_21);
      // load payload with FVs b op
    ccont_alts_21->payload[0] = b; // b
    ccont_alts_21->payload[1] = op; // op
  stgCurVal = a; // a
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "_intPrimop returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_21) {
  fprintf(stderr, "alts_21 here\n");
  Obj *ccont_alts_21 = stgPopCont();
  PtrOrLiteral b = ccont_alts_21->payload[0];
  PtrOrLiteral op = ccont_alts_21->payload[1];
  PtrOrLiteral scrut_alts_21 = stgCurVal;
  // I a_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_22 = stgAllocCont( &it_alts_22);
      // load payload with FVs op a_h
    ccont_alts_22->payload[0] = op; // op
    ccont_alts_22->payload[1] = scrut_alts_21.op->payload[0]; // a_h
  stgCurVal = b; // b
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_22) {
  fprintf(stderr, "alts_22 here\n");
  Obj *ccont_alts_22 = stgPopCont();
  PtrOrLiteral op = ccont_alts_22->payload[0];
  PtrOrLiteral a_h = ccont_alts_22->payload[1];
  PtrOrLiteral scrut_alts_22 = stgCurVal;
  // I b_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_23 = stgAllocCont( &it_alts_23);
      // no FVs
    // INDIRECT TAIL CALL op a_h b_h
  STGAPPLYNN(op, a_h, scrut_alts_22.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_23) {
  fprintf(stderr, "alts_23 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_23 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_23);
  STGRETURN0();
  ENDFUN;
}


// (Int_h[U]  -> Int_h[U]  -> Int_h[U] ) -> Int[B]  -> Int[B]  -> Bool[B] 
// ((["op","a","b"],[]),([Int_h[U]  -> Int_h[U]  -> Int_h[U] ,Int[B] ,Int[B] ],[]))
DEFUN4(fun__intComp, self, op, a, b) {
  fprintf(stderr, "_intComp here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_24 = stgAllocCont( &it_alts_24);
      // no FVs
    // INDIRECT TAIL CALL _intPrimop op a b
  STGAPPLYPPP(HOTOPL(&sho__intPrimop), op, a, b);
  fprintf(stderr, "_intComp returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_24) {
  fprintf(stderr, "alts_24 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_24 = stgCurVal;
  // I x_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_25 = stgAllocCont( &it_alts_25);
      // no FVs
    stgCurVal = scrut_alts_24.op->payload[0]; // x_h
  // unboxed EAtom
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_25) {
  fprintf(stderr, "alts_25 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_25 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__iplus, self, a_h, b_h) {
  fprintf(stderr, "_iplus here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i + (b_h).i;
  fprintf(stderr, "_iplus returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__isub, self, a_h, b_h) {
  fprintf(stderr, "_isub here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i - (b_h).i;
  fprintf(stderr, "_isub returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t270.List[B] t270 -> UBInt -> UBInt
// ((["xs"],["ac_h"]),([List[B] t270],[UBInt]))
DEFUN3(fun__length, self, xs, ac_h) {
  fprintf(stderr, "_length here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_39 = stgAllocCont( &it_alts_39);
      // load payload with FVs ac_h
    ccont_alts_39->payload[0] = ac_h; // ac_h
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "_length returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt
DEFUN0(alts_39) {
  fprintf(stderr, "alts_39 here\n");
  Obj *ccont_alts_39 = stgPopCont();
  PtrOrLiteral ac_h = ccont_alts_39->payload[0];
  PtrOrLiteral scrut_alts_39 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = ac_h; // ac_h
      // unboxed EAtom
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_40 = stgAllocCont( &it_alts_40);
          // load payload with FVs t
        ccont_alts_40->payload[0] = scrut_alts_39.op->payload[1]; // t
      stgCurVal.argType = INT;
      stgCurVal.i = (ac_h).i + 1;
      STGRETURN0();
    }
  }
  ENDFUN;
}


// UBInt
DEFUN0(alts_40) {
  fprintf(stderr, "alts_40 here\n");
  Obj *ccont_alts_40 = stgPopCont();
  PtrOrLiteral t = ccont_alts_40->payload[0];
  PtrOrLiteral scrut_alts_40 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL _length t r_h
  STGAPPLYPN(HOTOPL(&sho__length), t, scrut_alts_40);
  STGRETURN0();
  ENDFUN;
}


// forall t393.(t393 -> Bool[B] ) -> List[B] t393 -> Bool[B] 
// ((["p","xs"],[]),([t393 -> Bool[B] ,List[B] t393],[]))
DEFUN3(fun_all, self, p, xs) {
  fprintf(stderr, "all here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_55 = stgAllocCont( &it_alts_55);
      // load payload with FVs p
    ccont_alts_55->payload[0] = p; // p
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "all returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_55) {
  fprintf(stderr, "alts_55 here\n");
  Obj *ccont_alts_55 = stgPopCont();
  PtrOrLiteral p = ccont_alts_55->payload[0];
  PtrOrLiteral scrut_alts_55 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_56 = stgAllocCont( &it_alts_56);
          // load payload with FVs p t
        ccont_alts_56->payload[0] = p; // p
        ccont_alts_56->payload[1] = scrut_alts_55.op->payload[1]; // t
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_55.op->payload[0]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_56) {
  fprintf(stderr, "alts_56 here\n");
  Obj *ccont_alts_56 = stgPopCont();
  PtrOrLiteral p = ccont_alts_56->payload[0];
  PtrOrLiteral t = ccont_alts_56->payload[1];
  PtrOrLiteral scrut_alts_56 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // False  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // True  ->
    case 1: {
      // INDIRECT TAIL CALL all p t
      STGAPPLYPP(HOTOPL(&sho_all), p, t);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t405.(t405 -> Bool[B] ) -> List[B] t405 -> Bool[B] 
// ((["p","xs"],[]),([t405 -> Bool[B] ,List[B] t405],[]))
DEFUN3(fun_any, self, p, xs) {
  fprintf(stderr, "any here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_57 = stgAllocCont( &it_alts_57);
      // load payload with FVs p
    ccont_alts_57->payload[0] = p; // p
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "any returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_57) {
  fprintf(stderr, "alts_57 here\n");
  Obj *ccont_alts_57 = stgPopCont();
  PtrOrLiteral p = ccont_alts_57->payload[0];
  PtrOrLiteral scrut_alts_57 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_58 = stgAllocCont( &it_alts_58);
          // load payload with FVs p t
        ccont_alts_58->payload[0] = p; // p
        ccont_alts_58->payload[1] = scrut_alts_57.op->payload[1]; // t
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_57.op->payload[0]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_58) {
  fprintf(stderr, "alts_58 here\n");
  Obj *ccont_alts_58 = stgPopCont();
  PtrOrLiteral p = ccont_alts_58->payload[0];
  PtrOrLiteral t = ccont_alts_58->payload[1];
  PtrOrLiteral scrut_alts_58 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      // INDIRECT TAIL CALL any p t
      STGAPPLYPP(HOTOPL(&sho_any), p, t);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t195.List[B] t195 -> List[B] t195 -> List[B] t195
// ((["l1","l2"],[]),([List[B] t195,List[B] t195],[]))
DEFUN3(fun_append, self, l1, l2) {
  fprintf(stderr, "append here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_32 = stgAllocCont( &it_alts_32);
      // load payload with FVs l2
    ccont_alts_32->payload[0] = l2; // l2
  stgCurVal = l1; // l1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "append returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t195
DEFUN0(alts_32) {
  fprintf(stderr, "alts_32 here\n");
  Obj *ccont_alts_32 = stgPopCont();
  PtrOrLiteral l2 = ccont_alts_32->payload[0];
  PtrOrLiteral scrut_alts_32 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = l2; // l2
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons hd tl ->
    case 1: {
      Obj *rec_0 = stgNewHeapObj( &it_rec_0 );
      Obj *result_4 = stgNewHeapObj( &it_result_4 );
      rec_0->payload[1] = l2; // l2
      rec_0->payload[2] = scrut_alts_32.op->payload[1]; // tl
      result_4->payload[0] = scrut_alts_32.op->payload[0]; // hd
      result_4->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // rec_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_4
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t195
DEFUN1(fun_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL append tl l2
  STGAPPLYPP(HOTOPL(&sho_append), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "rec_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t423,t424.(t424 -> t423) -> t424 -> t423
// ((["f","x"],[]),([t424 -> t423,t424],[]))
DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  // INDIRECT TAIL CALL f x
  STGAPPLYP(f, x);
  fprintf(stderr, "apply returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t62.t62 -> List[B] t62 -> List[B] t62
// ((["h","t"],[]),([t62,List[B] t62],[]))
DEFUN3(fun_cons, self, h, t) {
  fprintf(stderr, "cons here\n");
  Obj *r_0 = stgNewHeapObj( &it_r_0 );
  r_0->payload[0] = h; // h
  r_0->payload[1] = t; // t
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // r_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cons returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t419,t420.t420 -> t419 -> t420
// ((["x","y"],[]),([t420,t419],[]))
DEFUN3(fun_const, self, x, y) {
  fprintf(stderr, "const here\n");
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "const returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_divInt, self, x, y) {
  fprintf(stderr, "divInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_15 = stgAllocCont( &it_alts_15);
      // load payload with FVs y
    ccont_alts_15->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "divInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_15) {
  fprintf(stderr, "alts_15 here\n");
  Obj *ccont_alts_15 = stgPopCont();
  PtrOrLiteral y = ccont_alts_15->payload[0];
  PtrOrLiteral scrut_alts_15 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_16 = stgAllocCont( &it_alts_16);
      // load payload with FVs i_h
    ccont_alts_16->payload[0] = scrut_alts_15.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_16) {
  fprintf(stderr, "alts_16 here\n");
  Obj *ccont_alts_16 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_16->payload[0];
  PtrOrLiteral scrut_alts_16 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_17 = stgAllocCont( &it_alts_17);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i / (scrut_alts_16.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_17) {
  fprintf(stderr, "alts_17 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_17 = stgCurVal;
  // x_h ->
  Obj *result_3 = stgNewHeapObj( &it_result_3 );
  result_3->payload[0] = scrut_alts_17; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_3
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Bool[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_3 = stgAllocCont( &it_alts_3);
      // load payload with FVs y
    ccont_alts_3->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "eqInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_3) {
  fprintf(stderr, "alts_3 here\n");
  Obj *ccont_alts_3 = stgPopCont();
  PtrOrLiteral y = ccont_alts_3->payload[0];
  PtrOrLiteral scrut_alts_3 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_4 = stgAllocCont( &it_alts_4);
      // load payload with FVs i_h
    ccont_alts_4->payload[0] = scrut_alts_3.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_4) {
  fprintf(stderr, "alts_4 here\n");
  Obj *ccont_alts_4 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_4->payload[0];
  PtrOrLiteral scrut_alts_4 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_5 = stgAllocCont( &it_alts_5);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i == (scrut_alts_4.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_12 = stgAllocCont( &it_alts_12);
      // load payload with FVs y
    ccont_alts_12->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "subInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_12) {
  fprintf(stderr, "alts_12 here\n");
  Obj *ccont_alts_12 = stgPopCont();
  PtrOrLiteral y = ccont_alts_12->payload[0];
  PtrOrLiteral scrut_alts_12 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_13 = stgAllocCont( &it_alts_13);
      // load payload with FVs i_h
    ccont_alts_13->payload[0] = scrut_alts_12.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_13) {
  fprintf(stderr, "alts_13 here\n");
  Obj *ccont_alts_13 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_13->payload[0];
  PtrOrLiteral scrut_alts_13 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_14 = stgAllocCont( &it_alts_14);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i - (scrut_alts_13.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_14) {
  fprintf(stderr, "alts_14 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_14 = stgCurVal;
  // x_h ->
  Obj *result_2 = stgNewHeapObj( &it_result_2 );
  result_2->payload[0] = scrut_alts_14; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// forall t443.Int[B]  -> List[B] t443 -> List[B] t443
// ((["n","xs"],[]),([Int[B] ,List[B] t443],[]))
DEFUN3(fun_drop, self, n, xs) {
  fprintf(stderr, "drop here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_44 = stgAllocCont( &it_alts_44);
      // load payload with FVs n xs
    ccont_alts_44->payload[0] = n; // n
    ccont_alts_44->payload[1] = xs; // xs
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  fprintf(stderr, "drop returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t443
DEFUN0(alts_44) {
  fprintf(stderr, "alts_44 here\n");
  Obj *ccont_alts_44 = stgPopCont();
  PtrOrLiteral n = ccont_alts_44->payload[0];
  PtrOrLiteral xs = ccont_alts_44->payload[1];
  PtrOrLiteral scrut_alts_44 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = xs; // xs
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      // scrutinee may heap alloc
      Obj *ccont_alts_45 = stgAllocCont( &it_alts_45);
          // load payload with FVs n
        ccont_alts_45->payload[0] = n; // n
      stgCurVal = xs; // xs
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t443
DEFUN0(alts_45) {
  fprintf(stderr, "alts_45 here\n");
  Obj *ccont_alts_45 = stgPopCont();
  PtrOrLiteral n = ccont_alts_45->payload[0];
  PtrOrLiteral scrut_alts_45 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *m_1 = stgNewHeapObj( &it_m_1 );
      m_1->payload[1] = n; // n
      // INDIRECT TAIL CALL drop m_1 t
      STGAPPLYPP(HOTOPL(&sho_drop), HOTOPL((Obj *)STGHEAPAT(2,1)), scrut_alts_45.op->payload[1]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_m_1, self) {
  fprintf(stderr, "m_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "m_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t456.(t456 -> Bool[B] ) -> List[B] t456 -> List[B] t456
// ((["p","xs"],[]),([t456 -> Bool[B] ,List[B] t456],[]))
DEFUN3(fun_filter, self, p, xs) {
  fprintf(stderr, "filter here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_53 = stgAllocCont( &it_alts_53);
      // load payload with FVs p
    ccont_alts_53->payload[0] = p; // p
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "filter returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t456
DEFUN0(alts_53) {
  fprintf(stderr, "alts_53 here\n");
  Obj *ccont_alts_53 = stgPopCont();
  PtrOrLiteral p = ccont_alts_53->payload[0];
  PtrOrLiteral scrut_alts_53 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *tail_0 = stgNewHeapObj( &it_tail_0 );
      tail_0->payload[1] = p; // p
      tail_0->payload[2] = scrut_alts_53.op->payload[1]; // t
      // scrutinee may heap alloc
      Obj *ccont_alts_54 = stgAllocCont( &it_alts_54);
          // load payload with FVs h tail_0
        ccont_alts_54->payload[0] = scrut_alts_53.op->payload[0]; // h
        ccont_alts_54->payload[1] = HOTOPL((Obj *)STGHEAPAT(3,1)); // tail_0
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_53.op->payload[0]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t456
DEFUN1(fun_tail_0, self) {
  fprintf(stderr, "tail_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL filter p t
  STGAPPLYPP(HOTOPL(&sho_filter), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "tail_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t456
DEFUN0(alts_54) {
  fprintf(stderr, "alts_54 here\n");
  Obj *ccont_alts_54 = stgPopCont();
  PtrOrLiteral h = ccont_alts_54->payload[0];
  PtrOrLiteral tail_0 = ccont_alts_54->payload[1];
  PtrOrLiteral scrut_alts_54 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      // INDIRECT TAIL CALL cons h tail_0
      STGAPPLYPP(HOTOPL(&sho_cons), h, tail_0);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      stgCurVal = tail_0; // tail_0
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t238,t244.(t244 -> t238 -> t244) -> t244 -> List[B] t238 -> t244
// ((["f","acc","list"],[]),([t244 -> t238 -> t244,t244,List[B] t238],[]))
DEFUN4(fun_foldl, self, f, acc, list) {
  fprintf(stderr, "foldl here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_36 = stgAllocCont( &it_alts_36);
      // load payload with FVs acc f
    ccont_alts_36->payload[0] = acc; // acc
    ccont_alts_36->payload[1] = f; // f
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "foldl returning\n");
  STGRETURN0();
  ENDFUN;
}

// t244
DEFUN0(alts_36) {
  fprintf(stderr, "alts_36 here\n");
  Obj *ccont_alts_36 = stgPopCont();
  PtrOrLiteral acc = ccont_alts_36->payload[0];
  PtrOrLiteral f = ccont_alts_36->payload[1];
  PtrOrLiteral scrut_alts_36 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = acc; // acc
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *newAcc_0 = stgNewHeapObj( &it_newAcc_0 );
      newAcc_0->payload[1] = acc; // acc
      newAcc_0->payload[2] = f; // f
      newAcc_0->payload[3] = scrut_alts_36.op->payload[0]; // h
      // INDIRECT TAIL CALL foldl f newAcc_0 t
      STGAPPLYPPP(HOTOPL(&sho_foldl), f, HOTOPL((Obj *)STGHEAPAT(4,1)), scrut_alts_36.op->payload[1]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// t244
DEFUN1(fun_newAcc_0, self) {
  fprintf(stderr, "newAcc_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL f acc h
  STGAPPLYPP(self.op->payload[2], self.op->payload[1], self.op->payload[3]);
  fprintf(stderr, "newAcc_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t252,t259.(t252 -> t259 -> t259) -> t259 -> List[B] t252 -> t259
// ((["f","sd","list"],[]),([t252 -> t259 -> t259,t259,List[B] t252],[]))
DEFUN4(fun_foldr, self, f, sd, list) {
  fprintf(stderr, "foldr here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_37 = stgAllocCont( &it_alts_37);
      // load payload with FVs f sd
    ccont_alts_37->payload[0] = f; // f
    ccont_alts_37->payload[1] = sd; // sd
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "foldr returning\n");
  STGRETURN0();
  ENDFUN;
}

// t259
DEFUN0(alts_37) {
  fprintf(stderr, "alts_37 here\n");
  Obj *ccont_alts_37 = stgPopCont();
  PtrOrLiteral f = ccont_alts_37->payload[0];
  PtrOrLiteral sd = ccont_alts_37->payload[1];
  PtrOrLiteral scrut_alts_37 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = sd; // sd
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *res_1 = stgNewHeapObj( &it_res_1 );
      res_1->payload[1] = f; // f
      res_1->payload[2] = sd; // sd
      res_1->payload[3] = scrut_alts_37.op->payload[1]; // t
      // INDIRECT TAIL CALL f h res_1
      STGAPPLYPP(f, scrut_alts_37.op->payload[0], HOTOPL((Obj *)STGHEAPAT(4,1)));
      STGRETURN0();
    }
  }
  ENDFUN;
}


// t259
DEFUN1(fun_res_1, self) {
  fprintf(stderr, "res_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL foldr f sd t
  STGAPPLYPPP(HOTOPL(&sho_foldr), self.op->payload[1], self.op->payload[2], self.op->payload[3]);
  fprintf(stderr, "res_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t58,t59.t58 -> t59 -> t59
// ((["x","y"],[]),([t58,t59],[]))
DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_0 = stgAllocCont( &it_alts_0);
      // load payload with FVs y
    ccont_alts_0->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "seq returning\n");
  STGRETURN0();
  ENDFUN;
}

// t59
DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  Obj *ccont_alts_0 = stgPopCont();
  PtrOrLiteral y = ccont_alts_0->payload[0];
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  // z ->
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// forall t451.List[B] t451 -> Unit[B] 
// ((["list"],[]),([List[B] t451],[]))
DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_41 = stgAllocCont( &it_alts_41);
      // no FVs
    stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "forcelist returning\n");
  STGRETURN0();
  ENDFUN;
}

// Unit[B] 
DEFUN0(alts_41) {
  fprintf(stderr, "alts_41 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_41 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_unit); // unit
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *rec_2 = stgNewHeapObj( &it_rec_2 );
      rec_2->payload[1] = scrut_alts_41.op->payload[1]; // t
      // INDIRECT TAIL CALL seq h rec_2
      STGAPPLYPP(HOTOPL(&sho_seq), scrut_alts_41.op->payload[0], HOTOPL((Obj *)STGHEAPAT(2,1)));
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Unit[B] 
DEFUN1(fun_rec_2, self) {
  fprintf(stderr, "rec_2 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL forcelist t
  STGAPPLYP(HOTOPL(&sho_forcelist), self.op->payload[1]);
  fprintf(stderr, "rec_2 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t79,t80.Tupl2[B] t79 t80 -> t79
// ((["t2"],[]),([Tupl2[B] t79 t80],[]))
DEFUN2(fun_fst, self, t2) {
  fprintf(stderr, "fst here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_1 = stgAllocCont( &it_alts_1);
      // no FVs
    stgCurVal = t2; // t2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "fst returning\n");
  STGRETURN0();
  ENDFUN;
}

// t79
DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // TP2 a b ->
  stgCurVal = scrut_alts_1.op->payload[0]; // a
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// UBInt -> Int_h[U]  -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,Int_h[U] ]))
DEFUN3(fun_gcd_h, self, a_h, b_h) {
  fprintf(stderr, "gcd_h here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_27 = stgAllocCont( &it_alts_27);
      // load payload with FVs a_h b_h
    ccont_alts_27->payload[0] = a_h; // a_h
    ccont_alts_27->payload[1] = b_h; // b_h
  stgCurVal = b_h; // b_h
  // unboxed EAtom
  fprintf(stderr, "gcd_h returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt
DEFUN0(alts_27) {
  fprintf(stderr, "alts_27 here\n");
  Obj *ccont_alts_27 = stgPopCont();
  PtrOrLiteral a_h = ccont_alts_27->payload[0];
  PtrOrLiteral b_h = ccont_alts_27->payload[1];
  PtrOrLiteral scrut_alts_27 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = a_h; // a_h
      // unboxed EAtom
      STGRETURN0();
    }
    // x ->
    default: {
      // scrutinee may heap alloc
      Obj *ccont_alts_28 = stgAllocCont( &it_alts_28);
          // load payload with FVs b_h
        ccont_alts_28->payload[0] = b_h; // b_h
      stgCurVal.argType = INT;
      stgCurVal.i = (a_h).i % (b_h).i;
      STGRETURN0();
    }
  }
  ENDFUN;
}


// UBInt
DEFUN0(alts_28) {
  fprintf(stderr, "alts_28 here\n");
  Obj *ccont_alts_28 = stgPopCont();
  PtrOrLiteral b_h = ccont_alts_28->payload[0];
  PtrOrLiteral scrut_alts_28 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL gcd_h b_h r_h
  STGAPPLYNN(HOTOPL(&sho_gcd_h), b_h, scrut_alts_28);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["a","b"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_gcd, self, a, b) {
  fprintf(stderr, "gcd here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_29 = stgAllocCont( &it_alts_29);
      // load payload with FVs b
    ccont_alts_29->payload[0] = b; // b
  stgCurVal = a; // a
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "gcd returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_29) {
  fprintf(stderr, "alts_29 here\n");
  Obj *ccont_alts_29 = stgPopCont();
  PtrOrLiteral b = ccont_alts_29->payload[0];
  PtrOrLiteral scrut_alts_29 = stgCurVal;
  // I a_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_30 = stgAllocCont( &it_alts_30);
      // load payload with FVs a_h
    ccont_alts_30->payload[0] = scrut_alts_29.op->payload[0]; // a_h
  stgCurVal = b; // b
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_30) {
  fprintf(stderr, "alts_30 here\n");
  Obj *ccont_alts_30 = stgPopCont();
  PtrOrLiteral a_h = ccont_alts_30->payload[0];
  PtrOrLiteral scrut_alts_30 = stgCurVal;
  // I b_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_31 = stgAllocCont( &it_alts_31);
      // no FVs
    // INDIRECT TAIL CALL gcd_h a_h b_h
  STGAPPLYNN(HOTOPL(&sho_gcd_h), a_h, scrut_alts_30.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_31) {
  fprintf(stderr, "alts_31 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_31 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_31);
  STGRETURN0();
  ENDFUN;
}


// forall t439.List[B] t439 -> t439
// ((["xs"],[]),([List[B] t439],[]))
DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_34 = stgAllocCont( &it_alts_34);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "head returning\n");
  STGRETURN0();
  ENDFUN;
}

// t439
DEFUN0(alts_34) {
  fprintf(stderr, "alts_34 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_34 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_34.op->payload[0]; // hd
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_34_exhaust = stgNewHeapObj( &it_alts_34_exhaust );
      alts_34_exhaust->payload[1] = scrut_alts_34; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_34_exhaust
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t223.t223
DEFUN1(fun_alts_34_exhaust, self) {
  fprintf(stderr, "alts_34_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_34_exhaust returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t359.List[B] t359 -> Bool[B] 
// ((["xs"],[]),([List[B] t359],[]))
DEFUN2(fun_null, self, xs) {
  fprintf(stderr, "null here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_50 = stgAllocCont( &it_alts_50);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "null returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_50) {
  fprintf(stderr, "alts_50 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_50 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t455.List[B] t455 -> List[B] t455
// ((["xs"],[]),([List[B] t455],[]))
DEFUN2(fun_init, self, xs) {
  fprintf(stderr, "init here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_51 = stgAllocCont( &it_alts_51);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "init returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t455
DEFUN0(alts_51) {
  fprintf(stderr, "alts_51 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_51 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_52 = stgAllocCont( &it_alts_52);
          // load payload with FVs h t
        ccont_alts_52->payload[0] = scrut_alts_51.op->payload[0]; // h
        ccont_alts_52->payload[1] = scrut_alts_51.op->payload[1]; // t
      // INDIRECT TAIL CALL null t
      STGAPPLYP(HOTOPL(&sho_null), scrut_alts_51.op->payload[1]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t455
DEFUN0(alts_52) {
  fprintf(stderr, "alts_52 here\n");
  Obj *ccont_alts_52 = stgPopCont();
  PtrOrLiteral h = ccont_alts_52->payload[0];
  PtrOrLiteral t = ccont_alts_52->payload[1];
  PtrOrLiteral scrut_alts_52 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      Obj *l_0 = stgNewHeapObj( &it_l_0 );
      l_0->payload[1] = t; // t
      // INDIRECT TAIL CALL cons h l_0
      STGAPPLYPP(HOTOPL(&sho_cons), h, HOTOPL((Obj *)STGHEAPAT(2,1)));
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t455
DEFUN1(fun_l_0, self) {
  fprintf(stderr, "l_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL init t
  STGAPPLYP(HOTOPL(&sho_init), self.op->payload[1]);
  fprintf(stderr, "l_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Bool[B] 
DEFUN1(fun_intLE, self) {
  fprintf(stderr, "intLE here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL _intComp _ile
  STGAPPLYP(HOTOPL(&sho__intComp), HOTOPL(&sho__ile));
  fprintf(stderr, "intLE returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t450.List[B] t450 -> Int[B] 
// ((["xs"],[]),([List[B] t450],[]))
DEFUN2(fun_length, self, xs) {
  fprintf(stderr, "length here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_38 = stgAllocCont( &it_alts_38);
      // no FVs
    // INDIRECT TAIL CALL _length xs 0
  STGAPPLYPN(HOTOPL(&sho__length), xs, ((PtrOrLiteral){.argType = INT,    .i = 0 }));
  fprintf(stderr, "length returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_38) {
  fprintf(stderr, "alts_38 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_38 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_38);
  STGRETURN0();
  ENDFUN;
}


// forall t208,t438.(t208 -> t438) -> List[B] t208 -> List[B] t438
// ((["f","list"],[]),([t208 -> t438,List[B] t208],[]))
DEFUN3(fun_map, self, f, list) {
  fprintf(stderr, "map here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_33 = stgAllocCont( &it_alts_33);
      // load payload with FVs f
    ccont_alts_33->payload[0] = f; // f
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "map returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t438
DEFUN0(alts_33) {
  fprintf(stderr, "alts_33 here\n");
  Obj *ccont_alts_33 = stgPopCont();
  PtrOrLiteral f = ccont_alts_33->payload[0];
  PtrOrLiteral scrut_alts_33 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *rec_1 = stgNewHeapObj( &it_rec_1 );
      Obj *x_0 = stgNewHeapObj( &it_x_0 );
      Obj *res_0 = stgNewHeapObj( &it_res_0 );
      rec_1->payload[1] = f; // f
      rec_1->payload[2] = scrut_alts_33.op->payload[1]; // t
      x_0->payload[1] = f; // f
      x_0->payload[2] = scrut_alts_33.op->payload[0]; // h
      res_0->payload[0] = HOTOPL((Obj *)STGHEAPAT(5,2)); // x_0
      res_0->payload[1] = HOTOPL((Obj *)STGHEAPAT(8,3)); // rec_1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // res_0
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t438
DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL map f t
  STGAPPLYPP(HOTOPL(&sho_map), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "rec_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// t438
DEFUN1(fun_x_0, self) {
  fprintf(stderr, "x_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL f h
  STGAPPLYP(self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "x_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Int[B] 
// ((["a","b"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_minInt, self, a, b) {
  fprintf(stderr, "minInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_26 = stgAllocCont( &it_alts_26);
      // load payload with FVs a b
    ccont_alts_26->payload[0] = a; // a
    ccont_alts_26->payload[1] = b; // b
  // INDIRECT TAIL CALL intLE a b
  STGAPPLYPP(HOTOPL(&sho_intLE), a, b);
  fprintf(stderr, "minInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_26) {
  fprintf(stderr, "alts_26 here\n");
  Obj *ccont_alts_26 = stgPopCont();
  PtrOrLiteral a = ccont_alts_26->payload[0];
  PtrOrLiteral b = ccont_alts_26->payload[1];
  PtrOrLiteral scrut_alts_26 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = a; // a
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      stgCurVal = b; // b
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_modInt, self, x, y) {
  fprintf(stderr, "modInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_18 = stgAllocCont( &it_alts_18);
      // load payload with FVs y
    ccont_alts_18->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "modInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_18) {
  fprintf(stderr, "alts_18 here\n");
  Obj *ccont_alts_18 = stgPopCont();
  PtrOrLiteral y = ccont_alts_18->payload[0];
  PtrOrLiteral scrut_alts_18 = stgCurVal;
  // I x_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_19 = stgAllocCont( &it_alts_19);
      // load payload with FVs x_h
    ccont_alts_19->payload[0] = scrut_alts_18.op->payload[0]; // x_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_19) {
  fprintf(stderr, "alts_19 here\n");
  Obj *ccont_alts_19 = stgPopCont();
  PtrOrLiteral x_h = ccont_alts_19->payload[0];
  PtrOrLiteral scrut_alts_19 = stgCurVal;
  // I y_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_20 = stgAllocCont( &it_alts_20);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (x_h).i % (scrut_alts_19.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_20) {
  fprintf(stderr, "alts_20 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_20 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_20);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_6 = stgAllocCont( &it_alts_6);
      // load payload with FVs y
    ccont_alts_6->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "multInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_6) {
  fprintf(stderr, "alts_6 here\n");
  Obj *ccont_alts_6 = stgPopCont();
  PtrOrLiteral y = ccont_alts_6->payload[0];
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_7 = stgAllocCont( &it_alts_7);
      // load payload with FVs i_h
    ccont_alts_7->payload[0] = scrut_alts_6.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_7) {
  fprintf(stderr, "alts_7 here\n");
  Obj *ccont_alts_7 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_7->payload[0];
  PtrOrLiteral scrut_alts_7 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_8 = stgAllocCont( &it_alts_8);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i * (scrut_alts_7.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_8) {
  fprintf(stderr, "alts_8 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_8 = stgCurVal;
  // x_h ->
  Obj *result_0 = stgNewHeapObj( &it_result_0 );
  result_0->payload[0] = scrut_alts_8; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_plusInt, self, x, y) {
  fprintf(stderr, "plusInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_9 = stgAllocCont( &it_alts_9);
      // load payload with FVs y
    ccont_alts_9->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "plusInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_9) {
  fprintf(stderr, "alts_9 here\n");
  Obj *ccont_alts_9 = stgPopCont();
  PtrOrLiteral y = ccont_alts_9->payload[0];
  PtrOrLiteral scrut_alts_9 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_10 = stgAllocCont( &it_alts_10);
      // load payload with FVs i_h
    ccont_alts_10->payload[0] = scrut_alts_9.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_10) {
  fprintf(stderr, "alts_10 here\n");
  Obj *ccont_alts_10 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_10->payload[0];
  PtrOrLiteral scrut_alts_10 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_11 = stgAllocCont( &it_alts_11);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i + (scrut_alts_10.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_11) {
  fprintf(stderr, "alts_11 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_11 = stgCurVal;
  // x_h ->
  Obj *result_1 = stgNewHeapObj( &it_result_1 );
  result_1->payload[0] = scrut_alts_11; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// forall t459.t459 -> List[B] t459
// ((["x"],[]),([t459],[]))
DEFUN2(fun_repeat, self, x) {
  fprintf(stderr, "repeat here\n");
  Obj *next_0 = stgNewHeapObj( &it_next_0 );
  next_0->payload[1] = x; // x
  // INDIRECT TAIL CALL cons x next_0
  STGAPPLYPP(HOTOPL(&sho_cons), x, HOTOPL((Obj *)STGHEAPAT(2,1)));
  fprintf(stderr, "repeat returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t459
DEFUN1(fun_next_0, self) {
  fprintf(stderr, "next_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL repeat x
  STGAPPLYP(HOTOPL(&sho_repeat), self.op->payload[1]);
  fprintf(stderr, "next_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t442.Int[B]  -> List[B] t442 -> List[B] t442
// ((["n","xs"],[]),([Int[B] ,List[B] t442],[]))
DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_42 = stgAllocCont( &it_alts_42);
      // load payload with FVs n xs
    ccont_alts_42->payload[0] = n; // n
    ccont_alts_42->payload[1] = xs; // xs
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  fprintf(stderr, "take returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t442
DEFUN0(alts_42) {
  fprintf(stderr, "alts_42 here\n");
  Obj *ccont_alts_42 = stgPopCont();
  PtrOrLiteral n = ccont_alts_42->payload[0];
  PtrOrLiteral xs = ccont_alts_42->payload[1];
  PtrOrLiteral scrut_alts_42 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      // scrutinee may heap alloc
      Obj *ccont_alts_43 = stgAllocCont( &it_alts_43);
          // load payload with FVs n
        ccont_alts_43->payload[0] = n; // n
      stgCurVal = xs; // xs
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t442
DEFUN0(alts_43) {
  fprintf(stderr, "alts_43 here\n");
  Obj *ccont_alts_43 = stgPopCont();
  PtrOrLiteral n = ccont_alts_43->payload[0];
  PtrOrLiteral scrut_alts_43 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons hd tl ->
    case 1: {
      Obj *m_0 = stgNewHeapObj( &it_m_0 );
      Obj *rec_3 = stgNewHeapObj( &it_rec_3 );
      Obj *result_5 = stgNewHeapObj( &it_result_5 );
      m_0->payload[1] = n; // n
      rec_3->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // m_0
      rec_3->payload[2] = scrut_alts_43.op->payload[1]; // tl
      result_5->payload[0] = scrut_alts_43.op->payload[0]; // hd
      result_5->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // rec_3
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_5
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_m_0, self) {
  fprintf(stderr, "m_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "m_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t442
DEFUN1(fun_rec_3, self) {
  fprintf(stderr, "rec_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL take m_0 tl
  STGAPPLYPP(HOTOPL(&sho_take), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "rec_3 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t465.Int[B]  -> t465 -> List[B] t465
// ((["n","x"],[]),([Int[B] ,t465],[]))
DEFUN3(fun_replicate, self, n, x) {
  fprintf(stderr, "replicate here\n");
  Obj *list_0 = stgNewHeapObj( &it_list_0 );
  list_0->payload[1] = x; // x
  // INDIRECT TAIL CALL take n list_0
  STGAPPLYPP(HOTOPL(&sho_take), n, HOTOPL((Obj *)STGHEAPAT(2,1)));
  fprintf(stderr, "replicate returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t465
DEFUN1(fun_list_0, self) {
  fprintf(stderr, "list_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL repeat x
  STGAPPLYP(HOTOPL(&sho_repeat), self.op->payload[1]);
  fprintf(stderr, "list_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t84,t85.Tupl2[B] t84 t85 -> t85
// ((["t2"],[]),([Tupl2[B] t84 t85],[]))
DEFUN2(fun_snd, self, t2) {
  fprintf(stderr, "snd here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_2 = stgAllocCont( &it_alts_2);
      // no FVs
    stgCurVal = t2; // t2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "snd returning\n");
  STGRETURN0();
  ENDFUN;
}

// t85
DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // TP2 a b ->
  stgCurVal = scrut_alts_2.op->payload[1]; // b
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// forall t453.List[B] t453 -> List[B] t453
// ((["xs"],[]),([List[B] t453],[]))
DEFUN2(fun_strictList, self, xs) {
  fprintf(stderr, "strictList here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_48 = stgAllocCont( &it_alts_48);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "strictList returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t453
DEFUN0(alts_48) {
  fprintf(stderr, "alts_48 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_48 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_49 = stgAllocCont( &it_alts_49);
          // load payload with FVs h
        ccont_alts_49->payload[0] = scrut_alts_48.op->payload[0]; // h
      // INDIRECT TAIL CALL strictList t
      STGAPPLYP(HOTOPL(&sho_strictList), scrut_alts_48.op->payload[1]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t453
DEFUN0(alts_49) {
  fprintf(stderr, "alts_49 here\n");
  Obj *ccont_alts_49 = stgPopCont();
  PtrOrLiteral h = ccont_alts_49->payload[0];
  PtrOrLiteral scrut_alts_49 = stgCurVal;
  // x ->
  // INDIRECT TAIL CALL cons h x
  STGAPPLYPP(HOTOPL(&sho_cons), h, scrut_alts_49);
  STGRETURN0();
  ENDFUN;
}


// List[B] Int[B]  -> Int[B] 
// ((["list"],[]),([List[B] Int[B] ],[]))
DEFUN2(fun_sum, self, list) {
  fprintf(stderr, "sum here\n");
  // INDIRECT TAIL CALL foldl plusInt zero list
  STGAPPLYPPP(HOTOPL(&sho_foldl), HOTOPL(&sho_plusInt), HOTOPL(&sho_zero), list);
  fprintf(stderr, "sum returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t229.List[B] t229 -> List[B] t229
// ((["xs"],[]),([List[B] t229],[]))
DEFUN2(fun_tail, self, xs) {
  fprintf(stderr, "tail here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_35 = stgAllocCont( &it_alts_35);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "tail returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t229
DEFUN0(alts_35) {
  fprintf(stderr, "alts_35 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_35 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_35.op->payload[1]; // tl
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_35_exhaust = stgNewHeapObj( &it_alts_35_exhaust );
      alts_35_exhaust->payload[1] = scrut_alts_35; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_35_exhaust
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// forall t230.t230
DEFUN1(fun_alts_35_exhaust, self) {
  fprintf(stderr, "alts_35_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_35_exhaust returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t71,t72.t71 -> t72 -> Tupl2[B] t71 t72
// ((["a","b"],[]),([t71,t72],[]))
DEFUN3(fun_tupl2, self, a, b) {
  fprintf(stderr, "tupl2 here\n");
  Obj *t_0 = stgNewHeapObj( &it_t_0 );
  t_0->payload[0] = a; // a
  t_0->payload[1] = b; // b
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // t_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "tupl2 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t89,t90,t91.t89 -> t90 -> t91 -> Tupl3[B] t89 t90 t91
// ((["a","b","c"],[]),([t89,t90,t91],[]))
DEFUN4(fun_tupl3, self, a, b, c) {
  fprintf(stderr, "tupl3 here\n");
  Obj *t_1 = stgNewHeapObj( &it_t_1 );
  t_1->payload[0] = a; // a
  t_1->payload[1] = b; // b
  t_1->payload[2] = c; // c
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // t_1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "tupl3 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t327,t330,t445.(t327 -> t330 -> t445) -> List[B] t327 -> List[B] t330 -> List[B] t445
// ((["f","list1","list2"],[]),([t327 -> t330 -> t445,List[B] t327,List[B] t330],[]))
DEFUN4(fun_zipWith, self, f, list1, list2) {
  fprintf(stderr, "zipWith here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_46 = stgAllocCont( &it_alts_46);
      // load payload with FVs f list2
    ccont_alts_46->payload[0] = f; // f
    ccont_alts_46->payload[1] = list2; // list2
  stgCurVal = list1; // list1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "zipWith returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t445
DEFUN0(alts_46) {
  fprintf(stderr, "alts_46 here\n");
  Obj *ccont_alts_46 = stgPopCont();
  PtrOrLiteral f = ccont_alts_46->payload[0];
  PtrOrLiteral list2 = ccont_alts_46->payload[1];
  PtrOrLiteral scrut_alts_46 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h1 t1 ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_47 = stgAllocCont( &it_alts_47);
          // load payload with FVs f h1 t1
        ccont_alts_47->payload[0] = f; // f
        ccont_alts_47->payload[1] = scrut_alts_46.op->payload[0]; // h1
        ccont_alts_47->payload[2] = scrut_alts_46.op->payload[1]; // t1
      stgCurVal = list2; // list2
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t445
DEFUN0(alts_47) {
  fprintf(stderr, "alts_47 here\n");
  Obj *ccont_alts_47 = stgPopCont();
  PtrOrLiteral f = ccont_alts_47->payload[0];
  PtrOrLiteral h1 = ccont_alts_47->payload[1];
  PtrOrLiteral t1 = ccont_alts_47->payload[2];
  PtrOrLiteral scrut_alts_47 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h2 t2 ->
    case 1: {
      Obj *newHead_0 = stgNewHeapObj( &it_newHead_0 );
      Obj *newTail_0 = stgNewHeapObj( &it_newTail_0 );
      Obj *result_6 = stgNewHeapObj( &it_result_6 );
      newHead_0->payload[1] = f; // f
      newHead_0->payload[2] = h1; // h1
      newHead_0->payload[3] = scrut_alts_47.op->payload[0]; // h2
      newTail_0->payload[1] = f; // f
      newTail_0->payload[2] = t1; // t1
      newTail_0->payload[3] = scrut_alts_47.op->payload[1]; // t2
      result_6->payload[0] = HOTOPL((Obj *)STGHEAPAT(10,3)); // newHead_0
      result_6->payload[1] = HOTOPL((Obj *)STGHEAPAT(6,2)); // newTail_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_6
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// t445
DEFUN1(fun_newHead_0, self) {
  fprintf(stderr, "newHead_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL f h1 h2
  STGAPPLYPP(self.op->payload[1], self.op->payload[2], self.op->payload[3]);
  fprintf(stderr, "newHead_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t445
DEFUN1(fun_newTail_0, self) {
  fprintf(stderr, "newTail_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL zipWith f t1 t2
  STGAPPLYPPP(HOTOPL(&sho_zipWith), self.op->payload[1], self.op->payload[2], self.op->payload[3]);
  fprintf(stderr, "newTail_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t462,t463.List[B] t462 -> List[B] t463 -> List[B] Tupl2[B] t462 t463
DEFUN1(fun_zip, self) {
  fprintf(stderr, "zip here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL zipWith tupl2
  STGAPPLYP(HOTOPL(&sho_zipWith), HOTOPL(&sho_tupl2));
  fprintf(stderr, "zip returning\n");
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

