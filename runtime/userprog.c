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
<<<<<<< HEAD
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
=======
FnPtr alts_17();
FnPtr alts_18();
FnPtr alts_19();
FnPtr fun__intComp();
FnPtr alts_20();
FnPtr alts_21();
FnPtr fun__iplus();
FnPtr fun__isub();
FnPtr fun__length();
FnPtr alts_35();
FnPtr alts_36();
FnPtr fun_all();
FnPtr alts_51();
FnPtr alts_52();
FnPtr fun_any();
FnPtr alts_53();
FnPtr alts_54();
FnPtr fun_append();
FnPtr alts_28();
FnPtr fun_rec_0();
FnPtr fun_apply();
FnPtr fun_eqInt();
FnPtr alts_2();
FnPtr alts_3();
FnPtr alts_4();
FnPtr fun_subInt();
FnPtr alts_11();
FnPtr alts_12();
FnPtr alts_13();
FnPtr fun_createArray();
FnPtr fun_a_6();
FnPtr alts_73();
FnPtr fun_d_3();
FnPtr fun_c_4();
FnPtr fun_cArr();
FnPtr fun_a_7();
FnPtr fun_multInt();
FnPtr alts_5();
FnPtr alts_6();
FnPtr alts_7();
FnPtr fun_createEvenArray();
FnPtr fun_a_10();
FnPtr alts_75();
FnPtr fun_e_2();
FnPtr fun_d_5();
FnPtr fun_c_6();
FnPtr fun_cEArr();
FnPtr fun_b_8();
FnPtr fun_a_11();
FnPtr fun_plusInt();
FnPtr alts_8();
FnPtr alts_9();
FnPtr alts_10();
FnPtr fun_createEvenBackArray();
FnPtr fun_a_12();
FnPtr alts_76();
FnPtr fun_d_6();
FnPtr fun_e_3();
FnPtr fun_c_7();
FnPtr fun_cEBArr();
FnPtr fun_a_13();
FnPtr fun_createNormArray();
FnPtr fun_a_0();
FnPtr alts_63();
FnPtr fun_d_0();
FnPtr fun_c_0();
FnPtr fun_cNArr();
FnPtr fun_b_2();
FnPtr fun_a_1();
FnPtr fun_createNormBackArray();
FnPtr fun_a_4();
FnPtr alts_68();
FnPtr fun_d_2();
FnPtr fun_e_0();
FnPtr fun_c_3();
FnPtr fun_cNBArr();
FnPtr fun_a_5();
FnPtr fun_createOddArray();
FnPtr fun_a_14();
FnPtr alts_77();
FnPtr fun_e_4();
FnPtr fun_f_0();
FnPtr fun_d_7();
FnPtr fun_c_8();
FnPtr fun_cOArr();
FnPtr fun_a_15();
FnPtr fun_createOddBackArray();
FnPtr fun_a_8();
FnPtr alts_74();
FnPtr fun_d_4();
FnPtr fun_e_1();
FnPtr fun_c_5();
FnPtr fun_cOBArr();
FnPtr fun_a_9();
FnPtr fun_compose();
FnPtr fun_r_1();
FnPtr fun_cons();
FnPtr fun_const();
FnPtr fun_divInt();
FnPtr alts_60();
FnPtr alts_61();
FnPtr alts_62();
FnPtr fun_drop();
FnPtr alts_40();
FnPtr alts_41();
FnPtr fun_m_1();
FnPtr fun_eqList();
FnPtr alts_69();
FnPtr alts_70();
FnPtr alts_71();
FnPtr fun_expr_0();
FnPtr alts_72();
FnPtr fun_final_0();
FnPtr fun_not();
FnPtr alts_59();
FnPtr fun_odd_h();
FnPtr alts_56();
FnPtr fun_odd();
FnPtr alts_58();
FnPtr fun_even();
FnPtr fun_even_h();
FnPtr alts_57();
FnPtr fun_filter();
FnPtr alts_49();
FnPtr fun_tail_0();
FnPtr alts_50();
FnPtr fun_foldl();
FnPtr alts_32();
FnPtr fun_newAcc_0();
FnPtr fun_foldr();
FnPtr alts_33();
FnPtr fun_res_1();
FnPtr fun_seq();
FnPtr alts_55();
FnPtr fun_forcelist();
FnPtr alts_37();
FnPtr fun_rec_2();
FnPtr fun_fst();
FnPtr alts_0();
FnPtr fun_gcd_h();
FnPtr alts_23();
FnPtr alts_24();
FnPtr fun_gcd();
FnPtr alts_25();
FnPtr alts_26();
FnPtr alts_27();
FnPtr fun_head();
FnPtr alts_30();
FnPtr fun_alts_30_exhaust();
FnPtr fun_index();
FnPtr alts_64();
FnPtr fun_a_2();
FnPtr alts_65();
FnPtr fun_c_1();
FnPtr fun_d_1();
FnPtr fun_alts_64_exhaust();
FnPtr fun_null();
FnPtr alts_46();
FnPtr fun_init();
FnPtr alts_47();
FnPtr alts_48();
FnPtr fun_l_0();
FnPtr fun_intLE();
FnPtr fun_length();
FnPtr alts_34();
FnPtr fun_map();
FnPtr alts_29();
FnPtr fun_rec_1();
FnPtr fun_x_0();
FnPtr fun_minInt();
FnPtr alts_22();
FnPtr fun_modInt();
FnPtr alts_14();
FnPtr alts_15();
FnPtr alts_16();
FnPtr fun_remove();
FnPtr alts_66();
FnPtr fun_a_3();
FnPtr alts_67();
FnPtr fun_b_3();
FnPtr fun_repeat();
FnPtr fun_next_0();
FnPtr fun_take();
FnPtr alts_38();
FnPtr alts_39();
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
FnPtr fun_m_0();
FnPtr fun_rec_3();
FnPtr fun_replicate();
FnPtr fun_list_0();
FnPtr fun_snd();
<<<<<<< HEAD
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
=======
FnPtr alts_1();
FnPtr fun_strictList();
FnPtr alts_44();
FnPtr alts_45();
FnPtr fun_sum();
FnPtr fun_tail();
FnPtr alts_31();
FnPtr fun_alts_31_exhaust();
FnPtr fun_tupl2();
FnPtr fun_tupl3();
FnPtr fun_zipWith();
FnPtr alts_42();
FnPtr alts_43();
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
FnPtr fun_newHead_0();
FnPtr fun_newTail_0();
FnPtr fun_zip();
InfoTab it__idiv __attribute__((aligned(8))) = 
  { .name                = "_idiv",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun__idiv,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__idiv,
  };
InfoTab it__ieq __attribute__((aligned(8))) = 
  { .name                = "_ieq",
<<<<<<< HEAD
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
=======
    // fvs []
    .entryCode           = &fun__ieq,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__ige __attribute__((aligned(8))) = 
  { .name                = "_ige",
    // fvs []
    .entryCode           = &fun__ige,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__igt __attribute__((aligned(8))) = 
  { .name                = "_igt",
    // fvs []
    .entryCode           = &fun__igt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__ile __attribute__((aligned(8))) = 
  { .name                = "_ile",
    // fvs []
    .entryCode           = &fun__ile,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__ilt __attribute__((aligned(8))) = 
  { .name                = "_ilt",
    // fvs []
    .entryCode           = &fun__ilt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__imax __attribute__((aligned(8))) = 
  { .name                = "_imax",
    // fvs []
    .entryCode           = &fun__imax,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__imin __attribute__((aligned(8))) = 
  { .name                = "_imin",
    // fvs []
    .entryCode           = &fun__imin,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__imod __attribute__((aligned(8))) = 
  { .name                = "_imod",
    // fvs []
    .entryCode           = &fun__imod,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__imul __attribute__((aligned(8))) = 
  { .name                = "_imul",
    // fvs []
    .entryCode           = &fun__imul,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it__ine __attribute__((aligned(8))) = 
  { .name                = "_ine",
    // fvs []
    .entryCode           = &fun__ine,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
InfoTab it__ineg __attribute__((aligned(8))) = 
  { .name                = "_ineg",
    // fvs []
    .entryCode           = &fun__ineg,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_int __attribute__((aligned(8))) = 
  { .name                = "int",
    // fvs []
    .entryCode           = &fun_int,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_i_0 __attribute__((aligned(8))) = 
  { .name                = "i_0",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun__intPrimop,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 3,
  };
InfoTab it_alts_17 __attribute__((aligned(8))) = 
  { .name                = "alts_17",
    // fvs [("b",Int[B] ),("op",Int_h[U]  -> Int_h[U]  -> Int_h[U] )]
    .entryCode           = &alts_17,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_18 __attribute__((aligned(8))) = 
  { .name                = "alts_18",
    // fvs [("op",Int_h[U]  -> Int_h[U]  -> Int_h[U] ),("a_h",Int_h[U] )]
    .entryCode           = &alts_18,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_19 __attribute__((aligned(8))) = 
  { .name                = "alts_19",
    // fvs []
    .entryCode           = &alts_19,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_false __attribute__((aligned(8))) = 
  { .name                = "false",
    // fvs []
    .entryCode           = &stg_constructorcall,
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
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun__intComp,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 3,
    .funFields.trueEntryCode = fun__intComp,
  };
InfoTab it_alts_24 __attribute__((aligned(8))) = 
  { .name                = "alts_24",
    // fvs []
    .entryCode           = &alts_24,
=======
    .funFields.arity     = 3,
  };
InfoTab it_alts_20 __attribute__((aligned(8))) = 
  { .name                = "alts_20",
    // fvs []
    .entryCode           = &alts_20,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_alts_25 __attribute__((aligned(8))) = 
  { .name                = "alts_25",
    // fvs []
    .entryCode           = &alts_25,
=======
InfoTab it_alts_21 __attribute__((aligned(8))) = 
  { .name                = "alts_21",
    // fvs []
    .entryCode           = &alts_21,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it__iplus __attribute__((aligned(8))) = 
  { .name                = "_iplus",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun__iplus,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__iplus,
=======
    .funFields.arity     = 2,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  };
InfoTab it__isub __attribute__((aligned(8))) = 
  { .name                = "_isub",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun__isub,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__isub,
=======
    .funFields.arity     = 2,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  };
InfoTab it__length __attribute__((aligned(8))) = 
  { .name                = "_length",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun__length,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun__length,
  };
InfoTab it_alts_39 __attribute__((aligned(8))) = 
  { .name                = "alts_39",
    // fvs [("ac_h",UBInt)]
    .entryCode           = &alts_39,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_35 __attribute__((aligned(8))) = 
  { .name                = "alts_35",
    // fvs [("ac_h",UBInt)]
    .entryCode           = &alts_35,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
<<<<<<< HEAD
InfoTab it_alts_40 __attribute__((aligned(8))) = 
  { .name                = "alts_40",
    // fvs [("t",List[B] t270)]
    .entryCode           = &alts_40,
=======
InfoTab it_alts_36 __attribute__((aligned(8))) = 
  { .name                = "alts_36",
    // fvs [("t",List[B] t258)]
    .entryCode           = &alts_36,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_all __attribute__((aligned(8))) = 
  { .name                = "all",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun_all,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_all,
  };
InfoTab it_alts_55 __attribute__((aligned(8))) = 
  { .name                = "alts_55",
    // fvs [("p",t393 -> Bool[B] )]
    .entryCode           = &alts_55,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_51 __attribute__((aligned(8))) = 
  { .name                = "alts_51",
    // fvs [("p",t381 -> Bool[B] )]
    .entryCode           = &alts_51,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_alts_56 __attribute__((aligned(8))) = 
  { .name                = "alts_56",
    // fvs [("p",t393 -> Bool[B] ),("t",List[B] t393)]
    .entryCode           = &alts_56,
=======
InfoTab it_alts_52 __attribute__((aligned(8))) = 
  { .name                = "alts_52",
    // fvs [("p",t381 -> Bool[B] ),("t",List[B] t381)]
    .entryCode           = &alts_52,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_any __attribute__((aligned(8))) = 
  { .name                = "any",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun_any,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_any,
  };
InfoTab it_alts_57 __attribute__((aligned(8))) = 
  { .name                = "alts_57",
    // fvs [("p",t405 -> Bool[B] )]
    .entryCode           = &alts_57,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_53 __attribute__((aligned(8))) = 
  { .name                = "alts_53",
    // fvs [("p",t393 -> Bool[B] )]
    .entryCode           = &alts_53,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_alts_58 __attribute__((aligned(8))) = 
  { .name                = "alts_58",
    // fvs [("p",t405 -> Bool[B] ),("t",List[B] t405)]
    .entryCode           = &alts_58,
=======
InfoTab it_alts_54 __attribute__((aligned(8))) = 
  { .name                = "alts_54",
    // fvs [("p",t393 -> Bool[B] ),("t",List[B] t393)]
    .entryCode           = &alts_54,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_append __attribute__((aligned(8))) = 
  { .name                = "append",
    // fvs []
<<<<<<< HEAD
    .entryCode           = &stg_funcall,
=======
    .entryCode           = &fun_append,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_append,
  };
InfoTab it_alts_32 __attribute__((aligned(8))) = 
  { .name                = "alts_32",
    // fvs [("l2",List[B] t195)]
    .entryCode           = &alts_32,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_28 __attribute__((aligned(8))) = 
  { .name                = "alts_28",
    // fvs [("l2",List[B] t183)]
    .entryCode           = &alts_28,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_0 __attribute__((aligned(8))) = 
  { .name                = "rec_0",
<<<<<<< HEAD
    // fvs [("l2",List[B] t195),("tl",List[B] t195)]
=======
    // fvs [("l2",List[B] t183),("tl",List[B] t183)]
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .entryCode           = &fun_rec_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_result_4 __attribute__((aligned(8))) = 
  { .name                = "result_4",
    // fvs [("hd",t195),("rec_0",List[B] t195)]
    .entryCode           = &stg_concall,
=======
InfoTab it_result_3 __attribute__((aligned(8))) = 
  { .name                = "result_3",
    // fvs [("hd",t183),("rec_0",List[B] t183)]
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
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
=======
    .entryCode           = &fun_apply,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_const,
  };
InfoTab it_divInt __attribute__((aligned(8))) = 
  { .name                = "divInt",
    // fvs []
    .entryCode           = &stg_funcall,
=======
    .funFields.arity     = 2,
  };
InfoTab it_eqInt __attribute__((aligned(8))) = 
  { .name                = "eqInt",
    // fvs []
    .entryCode           = &fun_eqInt,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_divInt,
  };
InfoTab it_alts_15 __attribute__((aligned(8))) = 
  { .name                = "alts_15",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_15,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_2 __attribute__((aligned(8))) = 
  { .name                = "alts_2",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_2,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_alts_16 __attribute__((aligned(8))) = 
  { .name                = "alts_16",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_16,
=======
InfoTab it_alts_3 __attribute__((aligned(8))) = 
  { .name                = "alts_3",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_3,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
<<<<<<< HEAD
InfoTab it_alts_17 __attribute__((aligned(8))) = 
  { .name                = "alts_17",
=======
InfoTab it_alts_4 __attribute__((aligned(8))) = 
  { .name                = "alts_4",
    // fvs []
    .entryCode           = &alts_4,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_one __attribute__((aligned(8))) = 
  { .name                = "one",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_eqInt __attribute__((aligned(8))) = 
  { .name                = "eqInt",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_subInt __attribute__((aligned(8))) = 
  { .name                = "subInt",
    // fvs []
    .entryCode           = &fun_subInt,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_11 __attribute__((aligned(8))) = 
  { .name                = "alts_11",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_11,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_12 __attribute__((aligned(8))) = 
  { .name                = "alts_12",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_12,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_13 __attribute__((aligned(8))) = 
  { .name                = "alts_13",
    // fvs []
    .entryCode           = &alts_13,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
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
<<<<<<< HEAD
InfoTab it_nil __attribute__((aligned(8))) = 
  { .name                = "nil",
    // fvs []
    .entryCode           = &stg_concall,
=======
InfoTab it_result_2 __attribute__((aligned(8))) = 
  { .name                = "result_2",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &stg_constructorcall,
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
InfoTab it_createArray __attribute__((aligned(8))) = 
  { .name                = "createArray",
    // fvs []
    .entryCode           = &fun_createArray,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_a_6 __attribute__((aligned(8))) = 
  { .name                = "a_6",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_6,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_73 __attribute__((aligned(8))) = 
  { .name                = "alts_73",
    // fvs [("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_73,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_5 __attribute__((aligned(8))) = 
  { .name                = "b_5",
    // fvs [("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_d_3 __attribute__((aligned(8))) = 
  { .name                = "d_3",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_4 __attribute__((aligned(8))) = 
  { .name                = "c_4",
    // fvs [("b_5",List[B] Int[B] ),("d_3",Int[B] )]
    .entryCode           = &fun_c_4,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_nil __attribute__((aligned(8))) = 
  { .name                = "nil",
    // fvs []
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
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
=======
InfoTab it_cArr __attribute__((aligned(8))) = 
  { .name                = "cArr",
    // fvs []
    .entryCode           = &fun_cArr,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_a_7 __attribute__((aligned(8))) = 
  { .name                = "a_7",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_7,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_multInt __attribute__((aligned(8))) = 
  { .name                = "multInt",
    // fvs []
    .entryCode           = &fun_multInt,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_subInt,
  };
InfoTab it_alts_12 __attribute__((aligned(8))) = 
  { .name                = "alts_12",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_12,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_5 __attribute__((aligned(8))) = 
  { .name                = "alts_5",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_5,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_alts_13 __attribute__((aligned(8))) = 
  { .name                = "alts_13",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_13,
=======
InfoTab it_alts_6 __attribute__((aligned(8))) = 
  { .name                = "alts_6",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_6,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
<<<<<<< HEAD
InfoTab it_alts_14 __attribute__((aligned(8))) = 
  { .name                = "alts_14",
    // fvs []
    .entryCode           = &alts_14,
=======
InfoTab it_alts_7 __attribute__((aligned(8))) = 
  { .name                = "alts_7",
    // fvs []
    .entryCode           = &alts_7,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_result_2 __attribute__((aligned(8))) = 
  { .name                = "result_2",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_concall,
=======
InfoTab it_result_0 __attribute__((aligned(8))) = 
  { .name                = "result_0",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_zero __attribute__((aligned(8))) = 
  { .name                = "zero",
    // fvs []
    .entryCode           = &stg_concall,
=======
InfoTab it_two __attribute__((aligned(8))) = 
  { .name                = "two",
    // fvs []
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_drop __attribute__((aligned(8))) = 
  { .name                = "drop",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_createEvenArray __attribute__((aligned(8))) = 
  { .name                = "createEvenArray",
    // fvs []
    .entryCode           = &fun_createEvenArray,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_drop,
  };
InfoTab it_alts_44 __attribute__((aligned(8))) = 
  { .name                = "alts_44",
    // fvs [("n",Int[B] ),("xs",List[B] t443)]
    .entryCode           = &alts_44,
=======
    .funFields.arity     = 2,
  };
InfoTab it_a_10 __attribute__((aligned(8))) = 
  { .name                = "a_10",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_10,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_75 __attribute__((aligned(8))) = 
  { .name                = "alts_75",
    // fvs [("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_75,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_g_0 __attribute__((aligned(8))) = 
  { .name                = "g_0",
    // fvs [("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_e_2 __attribute__((aligned(8))) = 
  { .name                = "e_2",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_e_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_7 __attribute__((aligned(8))) = 
  { .name                = "b_7",
    // fvs [("e_2",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_d_5 __attribute__((aligned(8))) = 
  { .name                = "d_5",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_5,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_c_6 __attribute__((aligned(8))) = 
  { .name                = "c_6",
    // fvs [("b_7",List[B] Int[B] ),("d_5",Int[B] )]
    .entryCode           = &fun_c_6,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_cEArr __attribute__((aligned(8))) = 
  { .name                = "cEArr",
    // fvs []
    .entryCode           = &fun_cEArr,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_b_8 __attribute__((aligned(8))) = 
  { .name                = "b_8",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_b_8,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_a_11 __attribute__((aligned(8))) = 
  { .name                = "a_11",
    // fvs [("b_8",Int[B] )]
    .entryCode           = &fun_a_11,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_plusInt __attribute__((aligned(8))) = 
  { .name                = "plusInt",
    // fvs []
    .entryCode           = &fun_plusInt,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_filter,
  };
InfoTab it_alts_53 __attribute__((aligned(8))) = 
  { .name                = "alts_53",
    // fvs [("p",t456 -> Bool[B] )]
    .entryCode           = &alts_53,
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_8 __attribute__((aligned(8))) = 
  { .name                = "alts_8",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_8,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_alts_9 __attribute__((aligned(8))) = 
  { .name                = "alts_9",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_9,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_10 __attribute__((aligned(8))) = 
  { .name                = "alts_10",
    // fvs []
    .entryCode           = &alts_10,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_1 __attribute__((aligned(8))) = 
  { .name                = "result_1",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_foldl __attribute__((aligned(8))) = 
  { .name                = "foldl",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_createEvenBackArray __attribute__((aligned(8))) = 
  { .name                = "createEvenBackArray",
    // fvs []
    .entryCode           = &fun_createEvenBackArray,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 3,
  };
InfoTab it_a_12 __attribute__((aligned(8))) = 
  { .name                = "a_12",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_12,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_76 __attribute__((aligned(8))) = 
  { .name                = "alts_76",
    // fvs [("m",Int[B] ),("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_76,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_9 __attribute__((aligned(8))) = 
  { .name                = "b_9",
    // fvs [("m",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_d_6 __attribute__((aligned(8))) = 
  { .name                = "d_6",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_6,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_e_3 __attribute__((aligned(8))) = 
  { .name                = "e_3",
    // fvs [("m",Int[B] )]
    .entryCode           = &fun_e_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_7 __attribute__((aligned(8))) = 
  { .name                = "c_7",
    // fvs [("b_9",List[B] Int[B] ),("d_6",Int[B] ),("e_3",Int[B] )]
    .entryCode           = &fun_c_7,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_seq __attribute__((aligned(8))) = 
  { .name                = "seq",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_cEBArr __attribute__((aligned(8))) = 
  { .name                = "cEBArr",
    // fvs []
    .entryCode           = &fun_cEBArr,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 1,
  };
InfoTab it_a_13 __attribute__((aligned(8))) = 
  { .name                = "a_13",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_13,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_createNormArray __attribute__((aligned(8))) = 
  { .name                = "createNormArray",
    // fvs []
    .entryCode           = &fun_createNormArray,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_a_0 __attribute__((aligned(8))) = 
  { .name                = "a_0",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_0,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_alts_63 __attribute__((aligned(8))) = 
  { .name                = "alts_63",
    // fvs [("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_63,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_0 __attribute__((aligned(8))) = 
  { .name                = "b_0",
    // fvs [("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_b_1 __attribute__((aligned(8))) = 
  { .name                = "b_1",
    // fvs [("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_d_0 __attribute__((aligned(8))) = 
  { .name                = "d_0",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_0 __attribute__((aligned(8))) = 
  { .name                = "c_0",
    // fvs [("b_1",List[B] Int[B] ),("d_0",Int[B] )]
    .entryCode           = &fun_c_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_cNArr __attribute__((aligned(8))) = 
  { .name                = "cNArr",
    // fvs []
    .entryCode           = &fun_cNArr,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 1,
  };
InfoTab it_b_2 __attribute__((aligned(8))) = 
  { .name                = "b_2",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_b_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_a_1 __attribute__((aligned(8))) = 
  { .name                = "a_1",
    // fvs [("b_2",Int[B] )]
    .entryCode           = &fun_a_1,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_null __attribute__((aligned(8))) = 
  { .name                = "null",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_createNormBackArray __attribute__((aligned(8))) = 
  { .name                = "createNormBackArray",
    // fvs []
    .entryCode           = &fun_createNormBackArray,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 3,
  };
InfoTab it_a_4 __attribute__((aligned(8))) = 
  { .name                = "a_4",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_4,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_68 __attribute__((aligned(8))) = 
  { .name                = "alts_68",
    // fvs [("m",Int[B] ),("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_68,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_4 __attribute__((aligned(8))) = 
  { .name                = "b_4",
    // fvs [("m",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_d_2 __attribute__((aligned(8))) = 
  { .name                = "d_2",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_e_0 __attribute__((aligned(8))) = 
  { .name                = "e_0",
    // fvs [("m",Int[B] )]
    .entryCode           = &fun_e_0,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_c_3 __attribute__((aligned(8))) = 
  { .name                = "c_3",
    // fvs [("b_4",List[B] Int[B] ),("d_2",Int[B] ),("e_0",Int[B] )]
    .entryCode           = &fun_c_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_cNBArr __attribute__((aligned(8))) = 
  { .name                = "cNBArr",
    // fvs []
    .entryCode           = &fun_cNBArr,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 1,
  };
InfoTab it_a_5 __attribute__((aligned(8))) = 
  { .name                = "a_5",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_5,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_createOddArray __attribute__((aligned(8))) = 
  { .name                = "createOddArray",
    // fvs []
    .entryCode           = &fun_createOddArray,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_a_14 __attribute__((aligned(8))) = 
  { .name                = "a_14",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_14,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_77 __attribute__((aligned(8))) = 
  { .name                = "alts_77",
    // fvs [("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_77,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_e_4 __attribute__((aligned(8))) = 
  { .name                = "e_4",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_e_4,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_f_0 __attribute__((aligned(8))) = 
  { .name                = "f_0",
    // fvs [("e_4",Int[B] )]
    .entryCode           = &fun_f_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_10 __attribute__((aligned(8))) = 
  { .name                = "b_10",
    // fvs [("f_0",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_minInt __attribute__((aligned(8))) = 
  { .name                = "minInt",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_d_7 __attribute__((aligned(8))) = 
  { .name                = "d_7",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_7,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_8 __attribute__((aligned(8))) = 
  { .name                = "c_8",
    // fvs [("b_10",List[B] Int[B] ),("d_7",Int[B] )]
    .entryCode           = &fun_c_8,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_cOArr __attribute__((aligned(8))) = 
  { .name                = "cOArr",
    // fvs []
    .entryCode           = &fun_cOArr,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 1,
  };
InfoTab it_a_15 __attribute__((aligned(8))) = 
  { .name                = "a_15",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_15,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_createOddBackArray __attribute__((aligned(8))) = 
  { .name                = "createOddBackArray",
    // fvs []
    .entryCode           = &fun_createOddBackArray,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 3,
  };
InfoTab it_a_8 __attribute__((aligned(8))) = 
  { .name                = "a_8",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_8,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_74 __attribute__((aligned(8))) = 
  { .name                = "alts_74",
    // fvs [("m",Int[B] ),("n",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &alts_74,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_6 __attribute__((aligned(8))) = 
  { .name                = "b_6",
    // fvs [("m",Int[B] ),("xs",List[B] Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_d_4 __attribute__((aligned(8))) = 
  { .name                = "d_4",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_d_4,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_e_1 __attribute__((aligned(8))) = 
  { .name                = "e_1",
    // fvs [("m",Int[B] )]
    .entryCode           = &fun_e_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_5 __attribute__((aligned(8))) = 
  { .name                = "c_5",
    // fvs [("b_6",List[B] Int[B] ),("d_4",Int[B] ),("e_1",Int[B] )]
    .entryCode           = &fun_c_5,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_cOBArr __attribute__((aligned(8))) = 
  { .name                = "cOBArr",
    // fvs []
    .entryCode           = &fun_cOBArr,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_a_9 __attribute__((aligned(8))) = 
  { .name                = "a_9",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_9,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_compose __attribute__((aligned(8))) = 
  { .name                = "compose",
    // fvs []
    .entryCode           = &fun_compose,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 3,
  };
InfoTab it_r_1 __attribute__((aligned(8))) = 
  { .name                = "r_1",
    // fvs [("g",t454 -> t456),("x",t454)]
    .entryCode           = &fun_r_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_cons __attribute__((aligned(8))) = 
  { .name                = "cons",
    // fvs []
    .entryCode           = &fun_cons,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 2,
    .funFields.trueEntryCode = fun_multInt,
  };
InfoTab it_alts_6 __attribute__((aligned(8))) = 
  { .name                = "alts_6",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_6,
=======
    .funFields.arity     = 2,
  };
InfoTab it_r_0 __attribute__((aligned(8))) = 
  { .name                = "r_0",
    // fvs [("h",t58),("t",List[B] t58)]
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_const,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_divInt __attribute__((aligned(8))) = 
  { .name                = "divInt",
    // fvs []
    .entryCode           = &fun_divInt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_60 __attribute__((aligned(8))) = 
  { .name                = "alts_60",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_60,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_alts_7 __attribute__((aligned(8))) = 
  { .name                = "alts_7",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_7,
=======
InfoTab it_alts_61 __attribute__((aligned(8))) = 
  { .name                = "alts_61",
    // fvs [("i_h",Int_h[U] )]
    .entryCode           = &alts_61,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
<<<<<<< HEAD
InfoTab it_alts_8 __attribute__((aligned(8))) = 
  { .name                = "alts_8",
    // fvs []
    .entryCode           = &alts_8,
=======
InfoTab it_alts_62 __attribute__((aligned(8))) = 
  { .name                = "alts_62",
    // fvs []
    .entryCode           = &alts_62,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_result_6 __attribute__((aligned(8))) = 
  { .name                = "result_6",
    // fvs [("x_h",UBInt)]
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_plusInt __attribute__((aligned(8))) = 
  { .name                = "plusInt",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_drop __attribute__((aligned(8))) = 
  { .name                = "drop",
    // fvs []
    .entryCode           = &fun_drop,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_40 __attribute__((aligned(8))) = 
  { .name                = "alts_40",
    // fvs [("n",Int[B] ),("xs",List[B] t723)]
    .entryCode           = &alts_40,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_41 __attribute__((aligned(8))) = 
  { .name                = "alts_41",
    // fvs [("n",Int[B] )]
    .entryCode           = &alts_41,
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
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_repeat __attribute__((aligned(8))) = 
  { .name                = "repeat",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_eqList __attribute__((aligned(8))) = 
  { .name                = "eqList",
    // fvs []
    .entryCode           = &fun_eqList,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_69 __attribute__((aligned(8))) = 
  { .name                = "alts_69",
    // fvs [("ys",List[B] Int[B] )]
    .entryCode           = &alts_69,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_70 __attribute__((aligned(8))) = 
  { .name                = "alts_70",
    // fvs []
    .entryCode           = &alts_70,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_71 __attribute__((aligned(8))) = 
  { .name                = "alts_71",
    // fvs [("h1",Int[B] ),("t1",List[B] Int[B] )]
    .entryCode           = &alts_71,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_expr_0 __attribute__((aligned(8))) = 
  { .name                = "expr_0",
    // fvs [("h1",Int[B] ),("h2",Int[B] )]
    .entryCode           = &fun_expr_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_72 __attribute__((aligned(8))) = 
  { .name                = "alts_72",
    // fvs [("t1",List[B] Int[B] ),("t2",List[B] Int[B] )]
    .entryCode           = &alts_72,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_final_0 __attribute__((aligned(8))) = 
  { .name                = "final_0",
    // fvs [("t1",List[B] Int[B] ),("t2",List[B] Int[B] )]
    .entryCode           = &fun_final_0,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_error  __attribute__((aligned(8)))= 
  { .name                = "error",
    // fvs []
    .entryCode           = &stg_error,
    .objType             = BLACKHOLE,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_not __attribute__((aligned(8))) = 
  { .name                = "not",
    // fvs []
    .entryCode           = &fun_not,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_59 __attribute__((aligned(8))) = 
  { .name                = "alts_59",
    // fvs []
    .entryCode           = &alts_59,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_odd_h __attribute__((aligned(8))) = 
  { .name                = "odd_h",
    // fvs []
    .entryCode           = &fun_odd_h,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_snd,
  };
InfoTab it_alts_2 __attribute__((aligned(8))) = 
  { .name                = "alts_2",
    // fvs []
    .entryCode           = &alts_2,
=======
    .funFields.arity     = 1,
  };
InfoTab it_alts_56 __attribute__((aligned(8))) = 
  { .name                = "alts_56",
    // fvs []
    .entryCode           = &alts_56,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
InfoTab it_strictList __attribute__((aligned(8))) = 
  { .name                = "strictList",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_odd __attribute__((aligned(8))) = 
  { .name                = "odd",
    // fvs []
    .entryCode           = &fun_odd,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_strictList,
  };
InfoTab it_alts_48 __attribute__((aligned(8))) = 
  { .name                = "alts_48",
    // fvs []
    .entryCode           = &alts_48,
=======
    .funFields.arity     = 1,
  };
InfoTab it_alts_58 __attribute__((aligned(8))) = 
  { .name                = "alts_58",
    // fvs []
    .entryCode           = &alts_58,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
<<<<<<< HEAD
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
=======
InfoTab it_even __attribute__((aligned(8))) = 
  { .name                = "even",
    // fvs []
    .entryCode           = &fun_even,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_even_h __attribute__((aligned(8))) = 
  { .name                = "even_h",
    // fvs []
    .entryCode           = &fun_even_h,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
    .funFields.arity         = 1,
    .funFields.trueEntryCode = fun_sum,
  };
InfoTab it_tail __attribute__((aligned(8))) = 
  { .name                = "tail",
    // fvs []
    .entryCode           = &stg_funcall,
=======
    .funFields.arity     = 1,
  };
InfoTab it_alts_57 __attribute__((aligned(8))) = 
  { .name                = "alts_57",
    // fvs []
    .entryCode           = &alts_57,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_filter __attribute__((aligned(8))) = 
  { .name                = "filter",
    // fvs []
    .entryCode           = &fun_filter,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_49 __attribute__((aligned(8))) = 
  { .name                = "alts_49",
    // fvs [("p",t745 -> Bool[B] )]
    .entryCode           = &alts_49,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_tail_0 __attribute__((aligned(8))) = 
  { .name                = "tail_0",
    // fvs [("p",t745 -> Bool[B] ),("t",List[B] t745)]
    .entryCode           = &fun_tail_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_50 __attribute__((aligned(8))) = 
  { .name                = "alts_50",
    // fvs [("h",t745),("tail_0",List[B] t745)]
    .entryCode           = &alts_50,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_five __attribute__((aligned(8))) = 
  { .name                = "five",
    // fvs []
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
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
=======
InfoTab it_foldl __attribute__((aligned(8))) = 
  { .name                = "foldl",
    // fvs []
    .entryCode           = &fun_foldl,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 3,
  };
InfoTab it_alts_32 __attribute__((aligned(8))) = 
  { .name                = "alts_32",
    // fvs [("acc",t232),("f",t232 -> t226 -> t232)]
    .entryCode           = &alts_32,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_newAcc_0 __attribute__((aligned(8))) = 
  { .name                = "newAcc_0",
    // fvs [("acc",t232),("f",t232 -> t226 -> t232),("h",t226)]
    .entryCode           = &fun_newAcc_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_foldr __attribute__((aligned(8))) = 
  { .name                = "foldr",
    // fvs []
    .entryCode           = &fun_foldr,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 3,
  };
InfoTab it_alts_33 __attribute__((aligned(8))) = 
  { .name                = "alts_33",
    // fvs [("f",t240 -> t247 -> t247),("sd",t247)]
    .entryCode           = &alts_33,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_res_1 __attribute__((aligned(8))) = 
  { .name                = "res_1",
    // fvs [("f",t240 -> t247 -> t247),("sd",t247),("t",List[B] t240)]
    .entryCode           = &fun_res_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_seq __attribute__((aligned(8))) = 
  { .name                = "seq",
    // fvs []
    .entryCode           = &fun_seq,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 2,
  };
InfoTab it_alts_55 __attribute__((aligned(8))) = 
  { .name                = "alts_55",
    // fvs [("y",t416)]
    .entryCode           = &alts_55,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_unit __attribute__((aligned(8))) = 
  { .name                = "unit",
    // fvs []
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_forcelist,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_37 __attribute__((aligned(8))) = 
  { .name                = "alts_37",
    // fvs []
    .entryCode           = &alts_37,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_2 __attribute__((aligned(8))) = 
  { .name                = "rec_2",
    // fvs [("t",List[B] t740)]
    .entryCode           = &fun_rec_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_four __attribute__((aligned(8))) = 
  { .name                = "four",
    // fvs []
    .entryCode           = &stg_constructorcall,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
InfoTab it_zipWith __attribute__((aligned(8))) = 
  { .name                = "zipWith",
    // fvs []
    .entryCode           = &stg_funcall,
=======
InfoTab it_fst __attribute__((aligned(8))) = 
  { .name                = "fst",
    // fvs []
    .entryCode           = &fun_fst,
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
<<<<<<< HEAD
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
=======
    .funFields.arity     = 1,
  };
InfoTab it_alts_0 __attribute__((aligned(8))) = 
  { .name                = "alts_0",
    // fvs []
    .entryCode           = &alts_0,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_gcd_h __attribute__((aligned(8))) = 
  { .name                = "gcd_h",
    // fvs []
    .entryCode           = &fun_gcd_h,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_23 __attribute__((aligned(8))) = 
  { .name                = "alts_23",
    // fvs [("a_h",UBInt),("b_h",Int_h[U] )]
    .entryCode           = &alts_23,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 2,
  };
InfoTab it_alts_24 __attribute__((aligned(8))) = 
  { .name                = "alts_24",
    // fvs [("b_h",Int_h[U] )]
    .entryCode           = &alts_24,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_gcd __attribute__((aligned(8))) = 
  { .name                = "gcd",
    // fvs []
    .entryCode           = &fun_gcd,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_25 __attribute__((aligned(8))) = 
  { .name                = "alts_25",
    // fvs [("b",Int[B] )]
    .entryCode           = &alts_25,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_26 __attribute__((aligned(8))) = 
  { .name                = "alts_26",
    // fvs [("a_h",Int_h[U] )]
    .entryCode           = &alts_26,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_27 __attribute__((aligned(8))) = 
  { .name                = "alts_27",
    // fvs []
    .entryCode           = &alts_27,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_head __attribute__((aligned(8))) = 
  { .name                = "head",
    // fvs []
    .entryCode           = &fun_head,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_30 __attribute__((aligned(8))) = 
  { .name                = "alts_30",
    // fvs []
    .entryCode           = &alts_30,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_30_exhaust __attribute__((aligned(8))) = 
  { .name                = "alts_30_exhaust",
    // fvs [("x",List[B] t719)]
    .entryCode           = &fun_alts_30_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_index __attribute__((aligned(8))) = 
  { .name                = "index",
    // fvs []
    .entryCode           = &fun_index,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_64 __attribute__((aligned(8))) = 
  { .name                = "alts_64",
    // fvs [("n",Int[B] )]
    .entryCode           = &alts_64,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_a_2 __attribute__((aligned(8))) = 
  { .name                = "a_2",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_a_2,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_65 __attribute__((aligned(8))) = 
  { .name                = "alts_65",
    // fvs [("h1",t731),("n",Int[B] ),("t1",List[B] t731)]
    .entryCode           = &alts_65,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_1 __attribute__((aligned(8))) = 
  { .name                = "c_1",
    // fvs [("n",Int[B] )]
    .entryCode           = &fun_c_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_d_1 __attribute__((aligned(8))) = 
  { .name                = "d_1",
    // fvs [("c_1",Int[B] ),("t1",List[B] t731)]
    .entryCode           = &fun_d_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_64_exhaust __attribute__((aligned(8))) = 
  { .name                = "alts_64_exhaust",
    // fvs [("x",List[B] t731)]
    .entryCode           = &fun_alts_64_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_null __attribute__((aligned(8))) = 
  { .name                = "null",
    // fvs []
    .entryCode           = &fun_null,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_46 __attribute__((aligned(8))) = 
  { .name                = "alts_46",
    // fvs []
    .entryCode           = &alts_46,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_init __attribute__((aligned(8))) = 
  { .name                = "init",
    // fvs []
    .entryCode           = &fun_init,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_47 __attribute__((aligned(8))) = 
  { .name                = "alts_47",
    // fvs []
    .entryCode           = &alts_47,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_48 __attribute__((aligned(8))) = 
  { .name                = "alts_48",
    // fvs [("h",t744),("t",List[B] t744)]
    .entryCode           = &alts_48,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_l_0 __attribute__((aligned(8))) = 
  { .name                = "l_0",
    // fvs [("t",List[B] t744)]
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
    .entryCode           = &fun_length,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
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
InfoTab it_map __attribute__((aligned(8))) = 
  { .name                = "map",
    // fvs []
    .entryCode           = &fun_map,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_29 __attribute__((aligned(8))) = 
  { .name                = "alts_29",
    // fvs [("f",t196 -> t718)]
    .entryCode           = &alts_29,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_rec_1 __attribute__((aligned(8))) = 
  { .name                = "rec_1",
    // fvs [("f",t196 -> t718),("t",List[B] t196)]
    .entryCode           = &fun_rec_1,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_x_0 __attribute__((aligned(8))) = 
  { .name                = "x_0",
    // fvs [("f",t196 -> t718),("h",t196)]
    .entryCode           = &fun_x_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_res_0 __attribute__((aligned(8))) = 
  { .name                = "res_0",
    // fvs [("rec_1",List[B] t718),("x_0",t718)]
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_minInt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_22 __attribute__((aligned(8))) = 
  { .name                = "alts_22",
    // fvs [("a",Int[B] ),("b",Int[B] )]
    .entryCode           = &alts_22,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_modInt __attribute__((aligned(8))) = 
  { .name                = "modInt",
    // fvs []
    .entryCode           = &fun_modInt,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_14 __attribute__((aligned(8))) = 
  { .name                = "alts_14",
    // fvs [("y",Int[B] )]
    .entryCode           = &alts_14,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_15 __attribute__((aligned(8))) = 
  { .name                = "alts_15",
    // fvs [("x_h",Int_h[U] )]
    .entryCode           = &alts_15,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 1,
  };
InfoTab it_alts_16 __attribute__((aligned(8))) = 
  { .name                = "alts_16",
    // fvs []
    .entryCode           = &alts_16,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_nine __attribute__((aligned(8))) = 
  { .name                = "nine",
    // fvs []
    .entryCode           = &stg_constructorcall,
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
InfoTab it_remove __attribute__((aligned(8))) = 
  { .name                = "remove",
    // fvs []
    .entryCode           = &fun_remove,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_66 __attribute__((aligned(8))) = 
  { .name                = "alts_66",
    // fvs [("n",Int[B] )]
    .entryCode           = &alts_66,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_a_3 __attribute__((aligned(8))) = 
  { .name                = "a_3",
    // fvs [("h1",Int[B] ),("n",Int[B] )]
    .entryCode           = &fun_a_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_67 __attribute__((aligned(8))) = 
  { .name                = "alts_67",
    // fvs [("h1",Int[B] ),("n",Int[B] ),("t1",List[B] Int[B] )]
    .entryCode           = &alts_67,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_b_3 __attribute__((aligned(8))) = 
  { .name                = "b_3",
    // fvs [("n",Int[B] ),("t1",List[B] Int[B] )]
    .entryCode           = &fun_b_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_c_2 __attribute__((aligned(8))) = 
  { .name                = "c_2",
    // fvs [("b_3",List[B] Int[B] ),("h1",Int[B] )]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_repeat __attribute__((aligned(8))) = 
  { .name                = "repeat",
    // fvs []
    .entryCode           = &fun_repeat,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_next_0 __attribute__((aligned(8))) = 
  { .name                = "next_0",
    // fvs [("x",t748)]
    .entryCode           = &fun_next_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_take __attribute__((aligned(8))) = 
  { .name                = "take",
    // fvs []
    .entryCode           = &fun_take,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_alts_38 __attribute__((aligned(8))) = 
  { .name                = "alts_38",
    // fvs [("n",Int[B] ),("xs",List[B] t722)]
    .entryCode           = &alts_38,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_39 __attribute__((aligned(8))) = 
  { .name                = "alts_39",
    // fvs [("n",Int[B] )]
    .entryCode           = &alts_39,
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
    // fvs [("m_0",Int[B] ),("tl",List[B] t722)]
    .entryCode           = &fun_rec_3,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_4 __attribute__((aligned(8))) = 
  { .name                = "result_4",
    // fvs [("hd",t722),("rec_3",List[B] t722)]
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_replicate,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_list_0 __attribute__((aligned(8))) = 
  { .name                = "list_0",
    // fvs [("x",t757)]
    .entryCode           = &fun_list_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_seven __attribute__((aligned(8))) = 
  { .name                = "seven",
    // fvs []
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_snd,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
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
InfoTab it_strictList __attribute__((aligned(8))) = 
  { .name                = "strictList",
    // fvs []
    .entryCode           = &fun_strictList,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_alts_44 __attribute__((aligned(8))) = 
  { .name                = "alts_44",
    // fvs []
    .entryCode           = &alts_44,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_45 __attribute__((aligned(8))) = 
  { .name                = "alts_45",
    // fvs [("h",t742)]
    .entryCode           = &alts_45,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 1,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_sum __attribute__((aligned(8))) = 
  { .name                = "sum",
    // fvs []
    .entryCode           = &fun_sum,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
  };
InfoTab it_tail __attribute__((aligned(8))) = 
  { .name                = "tail",
    // fvs []
    .entryCode           = &fun_tail,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 1,
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
InfoTab it_alts_31_exhaust __attribute__((aligned(8))) = 
  { .name                = "alts_31_exhaust",
    // fvs [("x",List[B] t217)]
    .entryCode           = &fun_alts_31_exhaust,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 1,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_ten __attribute__((aligned(8))) = 
  { .name                = "ten",
    // fvs []
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_tupl2,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 2,
  };
InfoTab it_t_0 __attribute__((aligned(8))) = 
  { .name                = "t_0",
    // fvs [("a",t67),("b",t68)]
    .entryCode           = &stg_constructorcall,
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
    .entryCode           = &fun_tupl3,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 3,
  };
InfoTab it_t_1 __attribute__((aligned(8))) = 
  { .name                = "t_1",
    // fvs [("a",t85),("b",t86),("c",t87)]
    .entryCode           = &stg_constructorcall,
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
InfoTab it_zipWith __attribute__((aligned(8))) = 
  { .name                = "zipWith",
    // fvs []
    .entryCode           = &fun_zipWith,
    .objType             = FUN,
    .layoutInfo.payloadSize  = 0,
    .layoutInfo.boxedCount   = 0,
    .layoutInfo.unboxedCount = 0,
    .funFields.arity     = 3,
  };
InfoTab it_alts_42 __attribute__((aligned(8))) = 
  { .name                = "alts_42",
    // fvs [("f",t315 -> t318 -> t725),("list2",List[B] t318)]
    .entryCode           = &alts_42,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 2,
    .layoutInfo.boxedCount   = 2,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_alts_43 __attribute__((aligned(8))) = 
  { .name                = "alts_43",
    // fvs [("f",t315 -> t318 -> t725),("h1",t315),("t1",List[B] t315)]
    .entryCode           = &alts_43,
    .objType             = CASECONT,
    .layoutInfo.payloadSize = 3,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_newHead_0 __attribute__((aligned(8))) = 
  { .name                = "newHead_0",
    // fvs [("f",t315 -> t318 -> t725),("h1",t315),("h2",t318)]
    .entryCode           = &fun_newHead_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_newTail_0 __attribute__((aligned(8))) = 
  { .name                = "newTail_0",
    // fvs [("f",t315 -> t318 -> t725),("t1",List[B] t315),("t2",List[B] t318)]
    .entryCode           = &fun_newTail_0,
    .objType             = THUNK,
    .layoutInfo.payloadSize = 4,
    .layoutInfo.boxedCount   = 3,
    .layoutInfo.unboxedCount = 0,
  };
InfoTab it_result_5 __attribute__((aligned(8))) = 
  { .name                = "result_5",
    // fvs [("newHead_0",t725),("newTail_0",List[B] t725)]
    .entryCode           = &stg_constructorcall,
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
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
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
=======
extern Obj sho_eqInt;
extern Obj sho_one;
extern Obj sho_subInt;
extern Obj sho_zero;
extern Obj sho_createArray;
extern Obj sho_nil;
extern Obj sho_cArr;
extern Obj sho_multInt;
extern Obj sho_two;
extern Obj sho_createEvenArray;
extern Obj sho_cEArr;
extern Obj sho_plusInt;
extern Obj sho_createEvenBackArray;
extern Obj sho_cEBArr;
extern Obj sho_createNormArray;
extern Obj sho_cNArr;
extern Obj sho_createNormBackArray;
extern Obj sho_cNBArr;
extern Obj sho_createOddArray;
extern Obj sho_cOArr;
extern Obj sho_createOddBackArray;
extern Obj sho_cOBArr;
extern Obj sho_compose;
extern Obj sho_cons;
extern Obj sho_const;
extern Obj sho_divInt;
extern Obj sho_drop;
extern Obj sho_eight;
extern Obj sho_eqList;
extern Obj sho_error;
extern Obj sho_not;
extern Obj sho_odd_h;
extern Obj sho_odd;
extern Obj sho_even;
extern Obj sho_even_h;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
=======
extern Obj sho_index;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
extern Obj sho_null;
extern Obj sho_init;
extern Obj sho_intLE;
extern Obj sho_length;
extern Obj sho_map;
extern Obj sho_minInt;
extern Obj sho_modInt;
<<<<<<< HEAD
extern Obj sho_multInt;
extern Obj sho_nine;
extern Obj sho_plusInt;
=======
extern Obj sho_nine;
extern Obj sho_remove;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
extern Obj sho_two;
extern Obj sho_zipWith;
extern Obj sho_zip;

Obj sho__idiv =
{
  .infoPtr   = (uintptr_t)&it__idiv,
  .objType   = FUN,
  .ident     = "_idiv",
=======
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

Obj sho_eqInt =
{
  .infoPtr   = (uintptr_t)&it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
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

Obj sho_createArray =
{
  .infoPtr   = (uintptr_t)&it_createArray,
  .objType   = FUN,
  .ident     = "createArray",
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

Obj sho_cArr =
{
  .infoPtr   = (uintptr_t)&it_cArr,
  .objType   = FUN,
  .ident     = "cArr",
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

Obj sho_two =
{
  .infoPtr   = (uintptr_t)&it_two,
  .objType   = CON,
  .ident     = "two",
  .payload = {
    {.argType = INT, .i = 2},
},
};

Obj sho_createEvenArray =
{
  .infoPtr   = (uintptr_t)&it_createEvenArray,
  .objType   = FUN,
  .ident     = "createEvenArray",
  .payload = {
    },
};

Obj sho_cEArr =
{
  .infoPtr   = (uintptr_t)&it_cEArr,
  .objType   = FUN,
  .ident     = "cEArr",
  .payload = {
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

Obj sho_createEvenBackArray =
{
  .infoPtr   = (uintptr_t)&it_createEvenBackArray,
  .objType   = FUN,
  .ident     = "createEvenBackArray",
  .payload = {
    },
};

Obj sho_cEBArr =
{
  .infoPtr   = (uintptr_t)&it_cEBArr,
  .objType   = FUN,
  .ident     = "cEBArr",
  .payload = {
    },
};

Obj sho_createNormArray =
{
  .infoPtr   = (uintptr_t)&it_createNormArray,
  .objType   = FUN,
  .ident     = "createNormArray",
  .payload = {
    },
};

Obj sho_cNArr =
{
  .infoPtr   = (uintptr_t)&it_cNArr,
  .objType   = FUN,
  .ident     = "cNArr",
  .payload = {
    },
};

Obj sho_createNormBackArray =
{
  .infoPtr   = (uintptr_t)&it_createNormBackArray,
  .objType   = FUN,
  .ident     = "createNormBackArray",
  .payload = {
    },
};

Obj sho_cNBArr =
{
  .infoPtr   = (uintptr_t)&it_cNBArr,
  .objType   = FUN,
  .ident     = "cNBArr",
  .payload = {
    },
};

Obj sho_createOddArray =
{
  .infoPtr   = (uintptr_t)&it_createOddArray,
  .objType   = FUN,
  .ident     = "createOddArray",
  .payload = {
    },
};

Obj sho_cOArr =
{
  .infoPtr   = (uintptr_t)&it_cOArr,
  .objType   = FUN,
  .ident     = "cOArr",
  .payload = {
    },
};

Obj sho_createOddBackArray =
{
  .infoPtr   = (uintptr_t)&it_createOddBackArray,
  .objType   = FUN,
  .ident     = "createOddBackArray",
  .payload = {
    },
};

Obj sho_cOBArr =
{
  .infoPtr   = (uintptr_t)&it_cOBArr,
  .objType   = FUN,
  .ident     = "cOBArr",
  .payload = {
    },
};

Obj sho_compose =
{
  .infoPtr   = (uintptr_t)&it_compose,
  .objType   = FUN,
  .ident     = "compose",
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

Obj sho_eqList =
{
  .infoPtr   = (uintptr_t)&it_eqList,
  .objType   = FUN,
  .ident     = "eqList",
  .payload = {
    },
};

Obj sho_error =
{
  .infoPtr   = (uintptr_t)&it_error,
  .objType   = BLACKHOLE,
  .ident     = "error",
  .payload = {0}
};

Obj sho_not =
{
  .infoPtr   = (uintptr_t)&it_not,
  .objType   = FUN,
  .ident     = "not",
  .payload = {
    },
};

Obj sho_odd_h =
{
  .infoPtr   = (uintptr_t)&it_odd_h,
  .objType   = FUN,
  .ident     = "odd_h",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__ieq =
{
  .infoPtr   = (uintptr_t)&it__ieq,
  .objType   = FUN,
  .ident     = "_ieq",
=======
Obj sho_odd =
{
  .infoPtr   = (uintptr_t)&it_odd,
  .objType   = FUN,
  .ident     = "odd",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__ige =
{
  .infoPtr   = (uintptr_t)&it__ige,
  .objType   = FUN,
  .ident     = "_ige",
=======
Obj sho_even =
{
  .infoPtr   = (uintptr_t)&it_even,
  .objType   = THUNK,
  .ident     = "even",
  .payload = {0}
};

Obj sho_even_h =
{
  .infoPtr   = (uintptr_t)&it_even_h,
  .objType   = FUN,
  .ident     = "even_h",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__igt =
{
  .infoPtr   = (uintptr_t)&it__igt,
  .objType   = FUN,
  .ident     = "_igt",
=======
Obj sho_filter =
{
  .infoPtr   = (uintptr_t)&it_filter,
  .objType   = FUN,
  .ident     = "filter",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__ile =
{
  .infoPtr   = (uintptr_t)&it__ile,
  .objType   = FUN,
  .ident     = "_ile",
=======
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
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__ilt =
{
  .infoPtr   = (uintptr_t)&it__ilt,
  .objType   = FUN,
  .ident     = "_ilt",
=======
Obj sho_foldr =
{
  .infoPtr   = (uintptr_t)&it_foldr,
  .objType   = FUN,
  .ident     = "foldr",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__imax =
{
  .infoPtr   = (uintptr_t)&it__imax,
  .objType   = FUN,
  .ident     = "_imax",
=======
Obj sho_seq =
{
  .infoPtr   = (uintptr_t)&it_seq,
  .objType   = FUN,
  .ident     = "seq",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__imin =
{
  .infoPtr   = (uintptr_t)&it__imin,
  .objType   = FUN,
  .ident     = "_imin",
=======
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
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__imod =
{
  .infoPtr   = (uintptr_t)&it__imod,
  .objType   = FUN,
  .ident     = "_imod",
=======
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
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__imul =
{
  .infoPtr   = (uintptr_t)&it__imul,
  .objType   = FUN,
  .ident     = "_imul",
=======
Obj sho_gcd_h =
{
  .infoPtr   = (uintptr_t)&it_gcd_h,
  .objType   = FUN,
  .ident     = "gcd_h",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__ine =
{
  .infoPtr   = (uintptr_t)&it__ine,
  .objType   = FUN,
  .ident     = "_ine",
=======
Obj sho_gcd =
{
  .infoPtr   = (uintptr_t)&it_gcd,
  .objType   = FUN,
  .ident     = "gcd",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__ineg =
{
  .infoPtr   = (uintptr_t)&it__ineg,
  .objType   = FUN,
  .ident     = "_ineg",
=======
Obj sho_head =
{
  .infoPtr   = (uintptr_t)&it_head,
  .objType   = FUN,
  .ident     = "head",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho_int =
{
  .infoPtr   = (uintptr_t)&it_int,
  .objType   = FUN,
  .ident     = "int",
=======
Obj sho_index =
{
  .infoPtr   = (uintptr_t)&it_index,
  .objType   = FUN,
  .ident     = "index",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__intPrimop =
{
  .infoPtr   = (uintptr_t)&it__intPrimop,
  .objType   = FUN,
  .ident     = "_intPrimop",
=======
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

Obj sho_nine =
{
  .infoPtr   = (uintptr_t)&it_nine,
  .objType   = CON,
  .ident     = "nine",
  .payload = {
    {.argType = INT, .i = 9},
},
};

Obj sho_remove =
{
  .infoPtr   = (uintptr_t)&it_remove,
  .objType   = FUN,
  .ident     = "remove",
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
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
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
=======
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
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__iplus =
{
  .infoPtr   = (uintptr_t)&it__iplus,
  .objType   = FUN,
  .ident     = "_iplus",
=======
Obj sho_tupl3 =
{
  .infoPtr   = (uintptr_t)&it_tupl3,
  .objType   = FUN,
  .ident     = "tupl3",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
Obj sho__isub =
{
  .infoPtr   = (uintptr_t)&it__isub,
  .objType   = FUN,
  .ident     = "_isub",
=======
Obj sho_zipWith =
{
  .infoPtr   = (uintptr_t)&it_zipWith,
  .objType   = FUN,
  .ident     = "zipWith",
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  .payload = {
    },
};

<<<<<<< HEAD
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
=======
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
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_createArray;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_cArr;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_createEvenArray;
  stgStatObj[stgStatObjCount++] = &sho_cEArr;
  stgStatObj[stgStatObjCount++] = &sho_plusInt;
  stgStatObj[stgStatObjCount++] = &sho_createEvenBackArray;
  stgStatObj[stgStatObjCount++] = &sho_cEBArr;
  stgStatObj[stgStatObjCount++] = &sho_createNormArray;
  stgStatObj[stgStatObjCount++] = &sho_cNArr;
  stgStatObj[stgStatObjCount++] = &sho_createNormBackArray;
  stgStatObj[stgStatObjCount++] = &sho_cNBArr;
  stgStatObj[stgStatObjCount++] = &sho_createOddArray;
  stgStatObj[stgStatObjCount++] = &sho_cOArr;
  stgStatObj[stgStatObjCount++] = &sho_createOddBackArray;
  stgStatObj[stgStatObjCount++] = &sho_cOBArr;
  stgStatObj[stgStatObjCount++] = &sho_compose;
  stgStatObj[stgStatObjCount++] = &sho_cons;
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_divInt;
  stgStatObj[stgStatObjCount++] = &sho_drop;
  stgStatObj[stgStatObjCount++] = &sho_eight;
  stgStatObj[stgStatObjCount++] = &sho_eqList;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_not;
  stgStatObj[stgStatObjCount++] = &sho_odd_h;
  stgStatObj[stgStatObjCount++] = &sho_odd;
  stgStatObj[stgStatObjCount++] = &sho_even;
  stgStatObj[stgStatObjCount++] = &sho_even_h;
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
  stgStatObj[stgStatObjCount++] = &sho_index;
  stgStatObj[stgStatObjCount++] = &sho_null;
  stgStatObj[stgStatObjCount++] = &sho_init;
  stgStatObj[stgStatObjCount++] = &sho_intLE;
  stgStatObj[stgStatObjCount++] = &sho_length;
  stgStatObj[stgStatObjCount++] = &sho_map;
  stgStatObj[stgStatObjCount++] = &sho_minInt;
  stgStatObj[stgStatObjCount++] = &sho_modInt;
  stgStatObj[stgStatObjCount++] = &sho_nine;
  stgStatObj[stgStatObjCount++] = &sho_remove;
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
  Obj *ccont_alts_17 = stgAllocCont( &it_alts_17);
      // load payload with FVs b op
    ccont_alts_17->payload[0] = b; // b
    ccont_alts_17->payload[1] = op; // op
  stgCurVal = a; // a
  // boxed EAtom 
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
  // scrutinee may heap alloc
  Obj *ccont_alts_18 = stgAllocCont( &it_alts_18);
      // load payload with FVs op a_h
    ccont_alts_18->payload[0] = op; // op
    ccont_alts_18->payload[1] = scrut_alts_17.op->payload[0]; // a_h
  stgCurVal = b; // b
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_18) {
  fprintf(stderr, "alts_18 here\n");
  Obj *ccont_alts_18 = stgPopCont();
  PtrOrLiteral op = ccont_alts_18->payload[0];
  PtrOrLiteral a_h = ccont_alts_18->payload[1];
  PtrOrLiteral scrut_alts_18 = stgCurVal;
  // I b_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_19 = stgAllocCont( &it_alts_19);
      // no FVs
    // INDIRECT TAIL CALL op a_h b_h
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
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_19);
  STGRETURN0();
  ENDFUN;
}


// (Int_h[U]  -> Int_h[U]  -> Int_h[U] ) -> Int[B]  -> Int[B]  -> Bool[B] 
// ((["op","a","b"],[]),([Int_h[U]  -> Int_h[U]  -> Int_h[U] ,Int[B] ,Int[B] ],[]))
DEFUN4(fun__intComp, self, op, a, b) {
  fprintf(stderr, "_intComp here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_20 = stgAllocCont( &it_alts_20);
      // no FVs
    // INDIRECT TAIL CALL _intPrimop op a b
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
  // scrutinee may heap alloc
  Obj *ccont_alts_21 = stgAllocCont( &it_alts_21);
      // no FVs
    stgCurVal = scrut_alts_20.op->payload[0]; // x_h
  // unboxed EAtom
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

// forall t258.List[B] t258 -> UBInt -> UBInt
// ((["xs"],["ac_h"]),([List[B] t258],[UBInt]))
DEFUN3(fun__length, self, xs, ac_h) {
  fprintf(stderr, "_length here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_35 = stgAllocCont( &it_alts_35);
      // load payload with FVs ac_h
    ccont_alts_35->payload[0] = ac_h; // ac_h
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "_length returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt
DEFUN0(alts_35) {
  fprintf(stderr, "alts_35 here\n");
  Obj *ccont_alts_35 = stgPopCont();
  PtrOrLiteral ac_h = ccont_alts_35->payload[0];
  PtrOrLiteral scrut_alts_35 = stgCurVal;
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
      Obj *ccont_alts_36 = stgAllocCont( &it_alts_36);
          // load payload with FVs t
        ccont_alts_36->payload[0] = scrut_alts_35.op->payload[1]; // t
      stgCurVal.argType = INT;
      stgCurVal.i = (ac_h).i + 1;
      STGRETURN0();
    }
  }
  ENDFUN;
}


// UBInt
DEFUN0(alts_36) {
  fprintf(stderr, "alts_36 here\n");
  Obj *ccont_alts_36 = stgPopCont();
  PtrOrLiteral t = ccont_alts_36->payload[0];
  PtrOrLiteral scrut_alts_36 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL _length t r_h
  STGAPPLYPN(HOTOPL(&sho__length), t, scrut_alts_36);
  STGRETURN0();
  ENDFUN;
}


// forall t381.(t381 -> Bool[B] ) -> List[B] t381 -> Bool[B] 
// ((["p","xs"],[]),([t381 -> Bool[B] ,List[B] t381],[]))
DEFUN3(fun_all, self, p, xs) {
  fprintf(stderr, "all here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_51 = stgAllocCont( &it_alts_51);
      // load payload with FVs p
    ccont_alts_51->payload[0] = p; // p
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "all returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_51) {
  fprintf(stderr, "alts_51 here\n");
  Obj *ccont_alts_51 = stgPopCont();
  PtrOrLiteral p = ccont_alts_51->payload[0];
  PtrOrLiteral scrut_alts_51 = stgCurVal;
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
      Obj *ccont_alts_52 = stgAllocCont( &it_alts_52);
          // load payload with FVs p t
        ccont_alts_52->payload[0] = p; // p
        ccont_alts_52->payload[1] = scrut_alts_51.op->payload[1]; // t
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_51.op->payload[0]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_52) {
  fprintf(stderr, "alts_52 here\n");
  Obj *ccont_alts_52 = stgPopCont();
  PtrOrLiteral p = ccont_alts_52->payload[0];
  PtrOrLiteral t = ccont_alts_52->payload[1];
  PtrOrLiteral scrut_alts_52 = stgCurVal;
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


// forall t393.(t393 -> Bool[B] ) -> List[B] t393 -> Bool[B] 
// ((["p","xs"],[]),([t393 -> Bool[B] ,List[B] t393],[]))
DEFUN3(fun_any, self, p, xs) {
  fprintf(stderr, "any here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_53 = stgAllocCont( &it_alts_53);
      // load payload with FVs p
    ccont_alts_53->payload[0] = p; // p
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "any returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_53) {
  fprintf(stderr, "alts_53 here\n");
  Obj *ccont_alts_53 = stgPopCont();
  PtrOrLiteral p = ccont_alts_53->payload[0];
  PtrOrLiteral scrut_alts_53 = stgCurVal;
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
      Obj *ccont_alts_54 = stgAllocCont( &it_alts_54);
          // load payload with FVs p t
        ccont_alts_54->payload[0] = p; // p
        ccont_alts_54->payload[1] = scrut_alts_53.op->payload[1]; // t
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_53.op->payload[0]);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Bool[B] 
DEFUN0(alts_54) {
  fprintf(stderr, "alts_54 here\n");
  Obj *ccont_alts_54 = stgPopCont();
  PtrOrLiteral p = ccont_alts_54->payload[0];
  PtrOrLiteral t = ccont_alts_54->payload[1];
  PtrOrLiteral scrut_alts_54 = stgCurVal;
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


// forall t183.List[B] t183 -> List[B] t183 -> List[B] t183
// ((["l1","l2"],[]),([List[B] t183,List[B] t183],[]))
DEFUN3(fun_append, self, l1, l2) {
  fprintf(stderr, "append here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_28 = stgAllocCont( &it_alts_28);
      // load payload with FVs l2
    ccont_alts_28->payload[0] = l2; // l2
  stgCurVal = l1; // l1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "append returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t183
DEFUN0(alts_28) {
  fprintf(stderr, "alts_28 here\n");
  Obj *ccont_alts_28 = stgPopCont();
  PtrOrLiteral l2 = ccont_alts_28->payload[0];
  PtrOrLiteral scrut_alts_28 = stgCurVal;
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
      Obj *result_3 = stgNewHeapObj( &it_result_3 );
      rec_0->payload[1] = l2; // l2
      rec_0->payload[2] = scrut_alts_28.op->payload[1]; // tl
      result_3->payload[0] = scrut_alts_28.op->payload[0]; // hd
      result_3->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // rec_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_3
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t183
DEFUN1(fun_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL append tl l2
  STGAPPLYPP(HOTOPL(&sho_append), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "rec_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t411,t412.(t412 -> t411) -> t412 -> t411
// ((["f","x"],[]),([t412 -> t411,t412],[]))
DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  // INDIRECT TAIL CALL f x
  STGAPPLYP(f, x);
  fprintf(stderr, "apply returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> Bool[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_2 = stgAllocCont( &it_alts_2);
      // load payload with FVs y
    ccont_alts_2->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
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
  // scrutinee may heap alloc
  Obj *ccont_alts_3 = stgAllocCont( &it_alts_3);
      // load payload with FVs i_h
    ccont_alts_3->payload[0] = scrut_alts_2.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
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
  // scrutinee may heap alloc
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
  Obj *ccont_alts_11 = stgAllocCont( &it_alts_11);
      // load payload with FVs y
    ccont_alts_11->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "subInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_11) {
  fprintf(stderr, "alts_11 here\n");
  Obj *ccont_alts_11 = stgPopCont();
  PtrOrLiteral y = ccont_alts_11->payload[0];
  PtrOrLiteral scrut_alts_11 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_12 = stgAllocCont( &it_alts_12);
      // load payload with FVs i_h
    ccont_alts_12->payload[0] = scrut_alts_11.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_12) {
  fprintf(stderr, "alts_12 here\n");
  Obj *ccont_alts_12 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_12->payload[0];
  PtrOrLiteral scrut_alts_12 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_13 = stgAllocCont( &it_alts_13);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i - (scrut_alts_12.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_13) {
  fprintf(stderr, "alts_13 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_13 = stgCurVal;
  // x_h ->
  Obj *result_2 = stgNewHeapObj( &it_result_2 );
  result_2->payload[0] = scrut_alts_13; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","xs"],[]),([Int[B] ,List[B] Int[B] ],[]))
DEFUN3(fun_createArray, self, n, xs) {
  fprintf(stderr, "createArray here\n");
  Obj *a_6 = stgNewHeapObj( &it_a_6 );
  a_6->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_73 = stgAllocCont( &it_alts_73);
      // load payload with FVs n xs
    ccont_alts_73->payload[0] = n; // n
    ccont_alts_73->payload[1] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_6
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createArray returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_a_6, self) {
  fprintf(stderr, "a_6 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_6 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN0(alts_73) {
  fprintf(stderr, "alts_73 here\n");
  Obj *ccont_alts_73 = stgPopCont();
  PtrOrLiteral n = ccont_alts_73->payload[0];
  PtrOrLiteral xs = ccont_alts_73->payload[1];
  PtrOrLiteral scrut_alts_73 = stgCurVal;
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
      Obj *b_5 = stgNewHeapObj( &it_b_5 );
      Obj *d_3 = stgNewHeapObj( &it_d_3 );
      Obj *c_4 = stgNewHeapObj( &it_c_4 );
      b_5->payload[0] = HOTOPL(&sho_zero); // zero
      b_5->payload[1] = xs; // xs
      d_3->payload[1] = n; // n
      c_4->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // b_5
      c_4->payload[2] = HOTOPL((Obj *)STGHEAPAT(5,2)); // d_3
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // c_4
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_d_3, self) {
  fprintf(stderr, "d_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_3 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_c_4, self) {
  fprintf(stderr, "c_4 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createArray d_3 b_5
  STGAPPLYPP(HOTOPL(&sho_createArray), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "c_4 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cArr, self, n) {
  fprintf(stderr, "cArr here\n");
  Obj *a_7 = stgNewHeapObj( &it_a_7 );
  a_7->payload[1] = n; // n
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_7
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cArr returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_a_7, self) {
  fprintf(stderr, "a_7 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createArray n nil
  STGAPPLYPP(HOTOPL(&sho_createArray), self.op->payload[1], HOTOPL(&sho_nil));
  fprintf(stderr, "a_7 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ieq, self, a_h, b_h) {
  fprintf(stderr, "_ieq here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i == (b_h).i;
  fprintf(stderr, "_ieq returning\n");
=======
// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_5 = stgAllocCont( &it_alts_5);
      // load payload with FVs y
    ccont_alts_5->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "multInt returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ige, self, a_h, b_h) {
  fprintf(stderr, "_ige here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i >= (b_h).i;
  fprintf(stderr, "_ige returning\n");
=======
// Int[B] 
DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  Obj *ccont_alts_5 = stgPopCont();
  PtrOrLiteral y = ccont_alts_5->payload[0];
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_6 = stgAllocCont( &it_alts_6);
      // load payload with FVs i_h
    ccont_alts_6->payload[0] = scrut_alts_5.op->payload[0]; // i_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__igt, self, a_h, b_h) {
  fprintf(stderr, "_igt here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i > (b_h).i;
  fprintf(stderr, "_igt returning\n");
=======

// Int[B] 
DEFUN0(alts_6) {
  fprintf(stderr, "alts_6 here\n");
  Obj *ccont_alts_6 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_6->payload[0];
  PtrOrLiteral scrut_alts_6 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_7 = stgAllocCont( &it_alts_7);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i * (scrut_alts_6.op->payload[0]).i;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ile, self, a_h, b_h) {
  fprintf(stderr, "_ile here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i <= (b_h).i;
  fprintf(stderr, "_ile returning\n");
=======

// Int[B] 
DEFUN0(alts_7) {
  fprintf(stderr, "alts_7 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_7 = stgCurVal;
  // x_h ->
  Obj *result_0 = stgNewHeapObj( &it_result_0 );
  result_0->payload[0] = scrut_alts_7; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ilt, self, a_h, b_h) {
  fprintf(stderr, "_ilt here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i < (b_h).i;
  fprintf(stderr, "_ilt returning\n");
=======

// Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","xs"],[]),([Int[B] ,List[B] Int[B] ],[]))
DEFUN3(fun_createEvenArray, self, n, xs) {
  fprintf(stderr, "createEvenArray here\n");
  Obj *a_10 = stgNewHeapObj( &it_a_10 );
  a_10->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_75 = stgAllocCont( &it_alts_75);
      // load payload with FVs n xs
    ccont_alts_75->payload[0] = n; // n
    ccont_alts_75->payload[1] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_10
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createEvenArray returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imax, self, a_h, b_h) {
  fprintf(stderr, "_imax here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = imax((a_h).i, (b_h).i);
  fprintf(stderr, "_imax returning\n");
=======
// Bool[B] 
DEFUN1(fun_a_10, self) {
  fprintf(stderr, "a_10 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_10 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imin, self, a_h, b_h) {
  fprintf(stderr, "_imin here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = imin((a_h).i, (b_h).i);
  fprintf(stderr, "_imin returning\n");
=======
// List[B] Int[B] 
DEFUN0(alts_75) {
  fprintf(stderr, "alts_75 here\n");
  Obj *ccont_alts_75 = stgPopCont();
  PtrOrLiteral n = ccont_alts_75->payload[0];
  PtrOrLiteral xs = ccont_alts_75->payload[1];
  PtrOrLiteral scrut_alts_75 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      Obj *g_0 = stgNewHeapObj( &it_g_0 );
      g_0->payload[0] = HOTOPL(&sho_zero); // zero
      g_0->payload[1] = xs; // xs
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // g_0
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      Obj *e_2 = stgNewHeapObj( &it_e_2 );
      Obj *b_7 = stgNewHeapObj( &it_b_7 );
      Obj *d_5 = stgNewHeapObj( &it_d_5 );
      Obj *c_6 = stgNewHeapObj( &it_c_6 );
      e_2->payload[1] = n; // n
      b_7->payload[0] = HOTOPL((Obj *)STGHEAPAT(9,4)); // e_2
      b_7->payload[1] = xs; // xs
      d_5->payload[1] = n; // n
      c_6->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // b_7
      c_6->payload[2] = HOTOPL((Obj *)STGHEAPAT(5,2)); // d_5
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // c_6
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_e_2, self) {
  fprintf(stderr, "e_2 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL multInt n two
  STGAPPLYPP(HOTOPL(&sho_multInt), self.op->payload[1], HOTOPL(&sho_two));
  fprintf(stderr, "e_2 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imod, self, a_h, b_h) {
  fprintf(stderr, "_imod here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i % (b_h).i;
  fprintf(stderr, "_imod returning\n");
=======
// Int[B] 
DEFUN1(fun_d_5, self) {
  fprintf(stderr, "d_5 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_5 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__imul, self, a_h, b_h) {
  fprintf(stderr, "_imul here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i * (b_h).i;
  fprintf(stderr, "_imul returning\n");
=======
// List[B] Int[B] 
DEFUN1(fun_c_6, self) {
  fprintf(stderr, "c_6 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createEvenArray d_5 b_7
  STGAPPLYPP(HOTOPL(&sho_createEvenArray), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "c_6 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__ine, self, a_h, b_h) {
  fprintf(stderr, "_ine here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i != (b_h).i;
  fprintf(stderr, "_ine returning\n");
=======
// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cEArr, self, n) {
  fprintf(stderr, "cEArr here\n");
  Obj *b_8 = stgNewHeapObj( &it_b_8 );
  Obj *a_11 = stgNewHeapObj( &it_a_11 );
  b_8->payload[1] = n; // n
  a_11->payload[1] = HOTOPL((Obj *)STGHEAPAT(4,2)); // b_8
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_11
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cEArr returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt
// (([],["a_h"]),([],[UBInt]))
DEFUN2(fun__ineg, self, a_h) {
  fprintf(stderr, "_ineg here\n");
  stgCurVal.argType = INT;
  stgCurVal.i =  -(a_h).i;
  fprintf(stderr, "_ineg returning\n");
=======
// Int[B] 
DEFUN1(fun_b_8, self) {
  fprintf(stderr, "b_8 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "b_8 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// List[B] Int[B] 
DEFUN1(fun_a_11, self) {
  fprintf(stderr, "a_11 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createEvenArray b_8 nil
  STGAPPLYPP(HOTOPL(&sho_createEvenArray), self.op->payload[1], HOTOPL(&sho_nil));
  fprintf(stderr, "a_11 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// Int[B]  -> Int[B]  -> Int[B] 
// ((["x","y"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_plusInt, self, x, y) {
  fprintf(stderr, "plusInt here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_8 = stgAllocCont( &it_alts_8);
      // load payload with FVs y
    ccont_alts_8->payload[0] = y; // y
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "plusInt returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
<<<<<<< HEAD
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
=======
DEFUN0(alts_8) {
  fprintf(stderr, "alts_8 here\n");
  Obj *ccont_alts_8 = stgPopCont();
  PtrOrLiteral y = ccont_alts_8->payload[0];
  PtrOrLiteral scrut_alts_8 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_9 = stgAllocCont( &it_alts_9);
      // load payload with FVs i_h
    ccont_alts_9->payload[0] = scrut_alts_8.op->payload[0]; // i_h
  stgCurVal = y; // y
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
<<<<<<< HEAD
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
=======
DEFUN0(alts_9) {
  fprintf(stderr, "alts_9 here\n");
  Obj *ccont_alts_9 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_9->payload[0];
  PtrOrLiteral scrut_alts_9 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_10 = stgAllocCont( &it_alts_10);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i + (scrut_alts_9.op->payload[0]).i;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
<<<<<<< HEAD
DEFUN0(alts_23) {
  fprintf(stderr, "alts_23 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_23 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_23);
=======
DEFUN0(alts_10) {
  fprintf(stderr, "alts_10 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_10 = stgCurVal;
  // x_h ->
  Obj *result_1 = stgNewHeapObj( &it_result_1 );
  result_1->payload[0] = scrut_alts_10; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_1
  // boxed EAtom 
  STGEVAL(stgCurVal);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Int[B]  -> Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","m","xs"],[]),([Int[B] ,Int[B] ,List[B] Int[B] ],[]))
DEFUN4(fun_createEvenBackArray, self, n, m, xs) {
  fprintf(stderr, "createEvenBackArray here\n");
  Obj *a_12 = stgNewHeapObj( &it_a_12 );
  a_12->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_76 = stgAllocCont( &it_alts_76);
      // load payload with FVs m n xs
    ccont_alts_76->payload[0] = m; // m
    ccont_alts_76->payload[1] = n; // n
    ccont_alts_76->payload[2] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_12
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createEvenBackArray returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
<<<<<<< HEAD
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
=======
DEFUN1(fun_a_12, self) {
  fprintf(stderr, "a_12 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_12 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN0(alts_76) {
  fprintf(stderr, "alts_76 here\n");
  Obj *ccont_alts_76 = stgPopCont();
  PtrOrLiteral m = ccont_alts_76->payload[0];
  PtrOrLiteral n = ccont_alts_76->payload[1];
  PtrOrLiteral xs = ccont_alts_76->payload[2];
  PtrOrLiteral scrut_alts_76 = stgCurVal;
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
      Obj *b_9 = stgNewHeapObj( &it_b_9 );
      Obj *d_6 = stgNewHeapObj( &it_d_6 );
      Obj *e_3 = stgNewHeapObj( &it_e_3 );
      Obj *c_7 = stgNewHeapObj( &it_c_7 );
      b_9->payload[0] = m; // m
      b_9->payload[1] = xs; // xs
      d_6->payload[1] = n; // n
      e_3->payload[1] = m; // m
      c_7->payload[1] = HOTOPL((Obj *)STGHEAPAT(10,4)); // b_9
      c_7->payload[2] = HOTOPL((Obj *)STGHEAPAT(8,3)); // d_6
      c_7->payload[3] = HOTOPL((Obj *)STGHEAPAT(6,2)); // e_3
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(4,1)); // c_7
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Int[B] 
DEFUN1(fun_d_6, self) {
  fprintf(stderr, "d_6 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_6 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_e_3, self) {
  fprintf(stderr, "e_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL plusInt two m
  STGAPPLYPP(HOTOPL(&sho_plusInt), HOTOPL(&sho_two), self.op->payload[1]);
  fprintf(stderr, "e_3 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_c_7, self) {
  fprintf(stderr, "c_7 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createEvenBackArray d_6 e_3 b_9
  STGAPPLYPPP(HOTOPL(&sho_createEvenBackArray), self.op->payload[2], self.op->payload[3], self.op->payload[1]);
  fprintf(stderr, "c_7 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cEBArr, self, n) {
  fprintf(stderr, "cEBArr here\n");
  Obj *a_13 = stgNewHeapObj( &it_a_13 );
  a_13->payload[1] = n; // n
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_13
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cEBArr returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_a_13, self) {
  fprintf(stderr, "a_13 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createEvenBackArray n zero nil
  STGAPPLYPPP(HOTOPL(&sho_createEvenBackArray), self.op->payload[1], HOTOPL(&sho_zero), HOTOPL(&sho_nil));
  fprintf(stderr, "a_13 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","xs"],[]),([Int[B] ,List[B] Int[B] ],[]))
DEFUN3(fun_createNormArray, self, n, xs) {
  fprintf(stderr, "createNormArray here\n");
  Obj *a_0 = stgNewHeapObj( &it_a_0 );
  a_0->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_63 = stgAllocCont( &it_alts_63);
      // load payload with FVs n xs
    ccont_alts_63->payload[0] = n; // n
    ccont_alts_63->payload[1] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_0
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createNormArray returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_a_0, self) {
  fprintf(stderr, "a_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_0 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

// Bool[B] 
DEFUN0(alts_25) {
  fprintf(stderr, "alts_25 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_25 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
=======
// List[B] Int[B] 
DEFUN0(alts_63) {
  fprintf(stderr, "alts_63 here\n");
  Obj *ccont_alts_63 = stgPopCont();
  PtrOrLiteral n = ccont_alts_63->payload[0];
  PtrOrLiteral xs = ccont_alts_63->payload[1];
  PtrOrLiteral scrut_alts_63 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      Obj *b_0 = stgNewHeapObj( &it_b_0 );
      b_0->payload[0] = n; // n
      b_0->payload[1] = xs; // xs
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // b_0
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
=======
    // False  ->
    case 0: {
      Obj *b_1 = stgNewHeapObj( &it_b_1 );
      Obj *d_0 = stgNewHeapObj( &it_d_0 );
      Obj *c_0 = stgNewHeapObj( &it_c_0 );
      b_1->payload[0] = n; // n
      b_1->payload[1] = xs; // xs
      d_0->payload[1] = n; // n
      c_0->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // b_1
      c_0->payload[2] = HOTOPL((Obj *)STGHEAPAT(5,2)); // d_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // c_0
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__iplus, self, a_h, b_h) {
  fprintf(stderr, "_iplus here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i + (b_h).i;
  fprintf(stderr, "_iplus returning\n");
=======
// Int[B] 
DEFUN1(fun_d_0, self) {
  fprintf(stderr, "d_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_0 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// UBInt -> UBInt -> UBInt
// (([],["a_h","b_h"]),([],[UBInt,UBInt]))
DEFUN3(fun__isub, self, a_h, b_h) {
  fprintf(stderr, "_isub here\n");
  stgCurVal.argType = INT;
  stgCurVal.i = (a_h).i - (b_h).i;
  fprintf(stderr, "_isub returning\n");
=======
// List[B] Int[B] 
DEFUN1(fun_c_0, self) {
  fprintf(stderr, "c_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createNormArray d_0 b_1
  STGAPPLYPP(HOTOPL(&sho_createNormArray), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "c_0 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cNArr, self, n) {
  fprintf(stderr, "cNArr here\n");
  Obj *b_2 = stgNewHeapObj( &it_b_2 );
  Obj *a_1 = stgNewHeapObj( &it_a_1 );
  b_2->payload[1] = n; // n
  a_1->payload[1] = HOTOPL((Obj *)STGHEAPAT(4,2)); // b_2
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cNArr returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// Int[B] 
DEFUN1(fun_b_2, self) {
  fprintf(stderr, "b_2 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "b_2 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_a_1, self) {
  fprintf(stderr, "a_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createNormArray b_2 nil
  STGAPPLYPP(HOTOPL(&sho_createNormArray), self.op->payload[1], HOTOPL(&sho_nil));
  fprintf(stderr, "a_1 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

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
=======
// Int[B]  -> Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","m","xs"],[]),([Int[B] ,Int[B] ,List[B] Int[B] ],[]))
DEFUN4(fun_createNormBackArray, self, n, m, xs) {
  fprintf(stderr, "createNormBackArray here\n");
  Obj *a_4 = stgNewHeapObj( &it_a_4 );
  a_4->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_68 = stgAllocCont( &it_alts_68);
      // load payload with FVs m n xs
    ccont_alts_68->payload[0] = m; // m
    ccont_alts_68->payload[1] = n; // n
    ccont_alts_68->payload[2] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_4
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createNormBackArray returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
<<<<<<< HEAD
DEFUN0(alts_55) {
  fprintf(stderr, "alts_55 here\n");
  Obj *ccont_alts_55 = stgPopCont();
  PtrOrLiteral p = ccont_alts_55->payload[0];
  PtrOrLiteral scrut_alts_55 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_true); // true
=======
DEFUN1(fun_a_4, self) {
  fprintf(stderr, "a_4 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_4 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN0(alts_68) {
  fprintf(stderr, "alts_68 here\n");
  Obj *ccont_alts_68 = stgPopCont();
  PtrOrLiteral m = ccont_alts_68->payload[0];
  PtrOrLiteral n = ccont_alts_68->payload[1];
  PtrOrLiteral xs = ccont_alts_68->payload[2];
  PtrOrLiteral scrut_alts_68 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = xs; // xs
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
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
=======
    // False  ->
    case 0: {
      Obj *b_4 = stgNewHeapObj( &it_b_4 );
      Obj *d_2 = stgNewHeapObj( &it_d_2 );
      Obj *e_0 = stgNewHeapObj( &it_e_0 );
      Obj *c_3 = stgNewHeapObj( &it_c_3 );
      b_4->payload[0] = m; // m
      b_4->payload[1] = xs; // xs
      d_2->payload[1] = n; // n
      e_0->payload[1] = m; // m
      c_3->payload[1] = HOTOPL((Obj *)STGHEAPAT(10,4)); // b_4
      c_3->payload[2] = HOTOPL((Obj *)STGHEAPAT(8,3)); // d_2
      c_3->payload[3] = HOTOPL((Obj *)STGHEAPAT(6,2)); // e_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(4,1)); // c_3
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
    // True  ->
    case 1: {
      // INDIRECT TAIL CALL all p t
      STGAPPLYPP(HOTOPL(&sho_all), p, t);
      STGRETURN0();
    }
=======
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  }
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Int[B] 
DEFUN1(fun_d_2, self) {
  fprintf(stderr, "d_2 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_2 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_e_0, self) {
  fprintf(stderr, "e_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL plusInt m one
  STGAPPLYPP(HOTOPL(&sho_plusInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "e_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_c_3, self) {
  fprintf(stderr, "c_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createNormBackArray d_2 e_0 b_4
  STGAPPLYPPP(HOTOPL(&sho_createNormBackArray), self.op->payload[2], self.op->payload[3], self.op->payload[1]);
  fprintf(stderr, "c_3 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cNBArr, self, n) {
  fprintf(stderr, "cNBArr here\n");
  Obj *a_5 = stgNewHeapObj( &it_a_5 );
  a_5->payload[1] = n; // n
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_5
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cNBArr returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// List[B] Int[B] 
DEFUN1(fun_a_5, self) {
  fprintf(stderr, "a_5 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createNormBackArray n zero nil
  STGAPPLYPPP(HOTOPL(&sho_createNormBackArray), self.op->payload[1], HOTOPL(&sho_zero), HOTOPL(&sho_nil));
  fprintf(stderr, "a_5 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","xs"],[]),([Int[B] ,List[B] Int[B] ],[]))
DEFUN3(fun_createOddArray, self, n, xs) {
  fprintf(stderr, "createOddArray here\n");
  Obj *a_14 = stgNewHeapObj( &it_a_14 );
  a_14->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_77 = stgAllocCont( &it_alts_77);
      // load payload with FVs n xs
    ccont_alts_77->payload[0] = n; // n
    ccont_alts_77->payload[1] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_14
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createOddArray returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN1(fun_a_14, self) {
  fprintf(stderr, "a_14 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_14 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN0(alts_77) {
  fprintf(stderr, "alts_77 here\n");
  Obj *ccont_alts_77 = stgPopCont();
  PtrOrLiteral n = ccont_alts_77->payload[0];
  PtrOrLiteral xs = ccont_alts_77->payload[1];
  PtrOrLiteral scrut_alts_77 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = xs; // xs
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
<<<<<<< HEAD
      // INDIRECT TAIL CALL any p t
      STGAPPLYPP(HOTOPL(&sho_any), p, t);
=======
      Obj *e_4 = stgNewHeapObj( &it_e_4 );
      Obj *f_0 = stgNewHeapObj( &it_f_0 );
      Obj *b_10 = stgNewHeapObj( &it_b_10 );
      Obj *d_7 = stgNewHeapObj( &it_d_7 );
      Obj *c_8 = stgNewHeapObj( &it_c_8 );
      e_4->payload[1] = n; // n
      f_0->payload[1] = HOTOPL((Obj *)STGHEAPAT(11,5)); // e_4
      b_10->payload[0] = HOTOPL((Obj *)STGHEAPAT(9,4)); // f_0
      b_10->payload[1] = xs; // xs
      d_7->payload[1] = n; // n
      c_8->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // b_10
      c_8->payload[2] = HOTOPL((Obj *)STGHEAPAT(5,2)); // d_7
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // c_8
      // boxed EAtom 
      STGEVAL(stgCurVal);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Int[B] 
DEFUN1(fun_e_4, self) {
  fprintf(stderr, "e_4 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL multInt n two
  STGAPPLYPP(HOTOPL(&sho_multInt), self.op->payload[1], HOTOPL(&sho_two));
  fprintf(stderr, "e_4 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_f_0, self) {
  fprintf(stderr, "f_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt e_4 one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "f_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN1(fun_d_7, self) {
  fprintf(stderr, "d_7 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_7 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_c_8, self) {
  fprintf(stderr, "c_8 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createOddArray d_7 b_10
  STGAPPLYPP(HOTOPL(&sho_createOddArray), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "c_8 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cOArr, self, n) {
  fprintf(stderr, "cOArr here\n");
  Obj *a_15 = stgNewHeapObj( &it_a_15 );
  a_15->payload[1] = n; // n
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_15
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cOArr returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_a_15, self) {
  fprintf(stderr, "a_15 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createOddArray n nil
  STGAPPLYPP(HOTOPL(&sho_createOddArray), self.op->payload[1], HOTOPL(&sho_nil));
  fprintf(stderr, "a_15 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","m","xs"],[]),([Int[B] ,Int[B] ,List[B] Int[B] ],[]))
DEFUN4(fun_createOddBackArray, self, n, m, xs) {
  fprintf(stderr, "createOddBackArray here\n");
  Obj *a_8 = stgNewHeapObj( &it_a_8 );
  a_8->payload[1] = n; // n
  // scrutinee may heap alloc
  Obj *ccont_alts_74 = stgAllocCont( &it_alts_74);
      // load payload with FVs m n xs
    ccont_alts_74->payload[0] = m; // m
    ccont_alts_74->payload[1] = n; // n
    ccont_alts_74->payload[2] = xs; // xs
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_8
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "createOddBackArray returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN1(fun_a_8, self) {
  fprintf(stderr, "a_8 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_8 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN0(alts_74) {
  fprintf(stderr, "alts_74 here\n");
  Obj *ccont_alts_74 = stgPopCont();
  PtrOrLiteral m = ccont_alts_74->payload[0];
  PtrOrLiteral n = ccont_alts_74->payload[1];
  PtrOrLiteral xs = ccont_alts_74->payload[2];
  PtrOrLiteral scrut_alts_74 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = xs; // xs
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
    // Cons hd tl ->
    case 1: {
      Obj *rec_0 = stgNewHeapObj( &it_rec_0 );
      Obj *result_4 = stgNewHeapObj( &it_result_4 );
      rec_0->payload[1] = l2; // l2
      rec_0->payload[2] = scrut_alts_32.op->payload[1]; // tl
      result_4->payload[0] = scrut_alts_32.op->payload[0]; // hd
      result_4->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // rec_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_4
=======
    // False  ->
    case 0: {
      Obj *b_6 = stgNewHeapObj( &it_b_6 );
      Obj *d_4 = stgNewHeapObj( &it_d_4 );
      Obj *e_1 = stgNewHeapObj( &it_e_1 );
      Obj *c_5 = stgNewHeapObj( &it_c_5 );
      b_6->payload[0] = m; // m
      b_6->payload[1] = xs; // xs
      d_4->payload[1] = n; // n
      e_1->payload[1] = m; // m
      c_5->payload[1] = HOTOPL((Obj *)STGHEAPAT(10,4)); // b_6
      c_5->payload[2] = HOTOPL((Obj *)STGHEAPAT(8,3)); // d_4
      c_5->payload[3] = HOTOPL((Obj *)STGHEAPAT(6,2)); // e_1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(4,1)); // c_5
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// List[B] t195
DEFUN1(fun_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL append tl l2
  STGAPPLYPP(HOTOPL(&sho_append), self.op->payload[2], self.op->payload[1]);
  fprintf(stderr, "rec_0 returning\n");
=======
// Int[B] 
DEFUN1(fun_d_4, self) {
  fprintf(stderr, "d_4 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "d_4 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t423,t424.(t424 -> t423) -> t424 -> t423
// ((["f","x"],[]),([t424 -> t423,t424],[]))
DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  // INDIRECT TAIL CALL f x
  STGAPPLYP(f, x);
  fprintf(stderr, "apply returning\n");
=======
// Int[B] 
DEFUN1(fun_e_1, self) {
  fprintf(stderr, "e_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL plusInt two m
  STGAPPLYPP(HOTOPL(&sho_plusInt), HOTOPL(&sho_two), self.op->payload[1]);
  fprintf(stderr, "e_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_c_5, self) {
  fprintf(stderr, "c_5 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createOddBackArray d_4 e_1 b_6
  STGAPPLYPPP(HOTOPL(&sho_createOddBackArray), self.op->payload[2], self.op->payload[3], self.op->payload[1]);
  fprintf(stderr, "c_5 returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B]  -> List[B] Int[B] 
// ((["n"],[]),([Int[B] ],[]))
DEFUN2(fun_cOBArr, self, n) {
  fprintf(stderr, "cOBArr here\n");
  Obj *a_9 = stgNewHeapObj( &it_a_9 );
  a_9->payload[1] = n; // n
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_9
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "cOBArr returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] Int[B] 
DEFUN1(fun_a_9, self) {
  fprintf(stderr, "a_9 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL createOddBackArray n one nil
  STGAPPLYPPP(HOTOPL(&sho_createOddBackArray), self.op->payload[1], HOTOPL(&sho_one), HOTOPL(&sho_nil));
  fprintf(stderr, "a_9 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t454,t455,t456.(t456 -> t455) -> (t454 -> t456) -> t454 -> t455
// ((["f","g","x"],[]),([t456 -> t455,t454 -> t456,t454],[]))
DEFUN4(fun_compose, self, f, g, x) {
  fprintf(stderr, "compose here\n");
  Obj *r_1 = stgNewHeapObj( &it_r_1 );
  r_1->payload[1] = g; // g
  r_1->payload[2] = x; // x
  // INDIRECT TAIL CALL f r_1
  STGAPPLYP(f, HOTOPL((Obj *)STGHEAPAT(3,1)));
  fprintf(stderr, "compose returning\n");
  STGRETURN0();
  ENDFUN;
}

// t456
DEFUN1(fun_r_1, self) {
  fprintf(stderr, "r_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL g x
  STGAPPLYP(self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "r_1 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t62.t62 -> List[B] t62 -> List[B] t62
// ((["h","t"],[]),([t62,List[B] t62],[]))
=======
// forall t58.t58 -> List[B] t58 -> List[B] t58
// ((["h","t"],[]),([t58,List[B] t58],[]))
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// forall t419,t420.t420 -> t419 -> t420
// ((["x","y"],[]),([t420,t419],[]))
=======
// forall t407,t408.t408 -> t407 -> t408
// ((["x","y"],[]),([t408,t407],[]))
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
  Obj *ccont_alts_15 = stgAllocCont( &it_alts_15);
      // load payload with FVs y
    ccont_alts_15->payload[0] = y; // y
=======
  Obj *ccont_alts_60 = stgAllocCont( &it_alts_60);
      // load payload with FVs y
    ccont_alts_60->payload[0] = y; // y
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "divInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
<<<<<<< HEAD
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
=======
DEFUN0(alts_60) {
  fprintf(stderr, "alts_60 here\n");
  Obj *ccont_alts_60 = stgPopCont();
  PtrOrLiteral y = ccont_alts_60->payload[0];
  PtrOrLiteral scrut_alts_60 = stgCurVal;
  // I i_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_61 = stgAllocCont( &it_alts_61);
      // load payload with FVs i_h
    ccont_alts_61->payload[0] = scrut_alts_60.op->payload[0]; // i_h
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
<<<<<<< HEAD
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
=======
DEFUN0(alts_61) {
  fprintf(stderr, "alts_61 here\n");
  Obj *ccont_alts_61 = stgPopCont();
  PtrOrLiteral i_h = ccont_alts_61->payload[0];
  PtrOrLiteral scrut_alts_61 = stgCurVal;
  // I j_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_62 = stgAllocCont( &it_alts_62);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i / (scrut_alts_61.op->payload[0]).i;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
<<<<<<< HEAD
DEFUN0(alts_17) {
  fprintf(stderr, "alts_17 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_17 = stgCurVal;
  // x_h ->
  Obj *result_3 = stgNewHeapObj( &it_result_3 );
  result_3->payload[0] = scrut_alts_17; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_3
=======
DEFUN0(alts_62) {
  fprintf(stderr, "alts_62 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_62 = stgCurVal;
  // x_h ->
  Obj *result_6 = stgNewHeapObj( &it_result_6 );
  result_6->payload[0] = scrut_alts_62; // x_h
  stgCurVal = HOTOPL((Obj *)STGHEAPAT(1,1)); // result_6
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// forall t723.Int[B]  -> List[B] t723 -> List[B] t723
// ((["n","xs"],[]),([Int[B] ,List[B] t723],[]))
DEFUN3(fun_drop, self, n, xs) {
  fprintf(stderr, "drop here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_40 = stgAllocCont( &it_alts_40);
      // load payload with FVs n xs
    ccont_alts_40->payload[0] = n; // n
    ccont_alts_40->payload[1] = xs; // xs
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  fprintf(stderr, "drop returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// List[B] t723
DEFUN0(alts_40) {
  fprintf(stderr, "alts_40 here\n");
  Obj *ccont_alts_40 = stgPopCont();
  PtrOrLiteral n = ccont_alts_40->payload[0];
  PtrOrLiteral xs = ccont_alts_40->payload[1];
  PtrOrLiteral scrut_alts_40 = stgCurVal;
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
      Obj *ccont_alts_41 = stgAllocCont( &it_alts_41);
          // load payload with FVs n
        ccont_alts_41->payload[0] = n; // n
      stgCurVal = xs; // xs
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// List[B] t723
DEFUN0(alts_41) {
  fprintf(stderr, "alts_41 here\n");
  Obj *ccont_alts_41 = stgPopCont();
  PtrOrLiteral n = ccont_alts_41->payload[0];
  PtrOrLiteral scrut_alts_41 = stgCurVal;
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
      STGAPPLYPP(HOTOPL(&sho_drop), HOTOPL((Obj *)STGHEAPAT(2,1)), scrut_alts_41.op->payload[1]);
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

// List[B] Int[B]  -> List[B] Int[B]  -> Bool[B] 
// ((["xs","ys"],[]),([List[B] Int[B] ,List[B] Int[B] ],[]))
DEFUN3(fun_eqList, self, xs, ys) {
  fprintf(stderr, "eqList here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_69 = stgAllocCont( &it_alts_69);
      // load payload with FVs ys
    ccont_alts_69->payload[0] = ys; // ys
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "eqList returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

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
=======
// Bool[B] 
DEFUN0(alts_69) {
  fprintf(stderr, "alts_69 here\n");
  Obj *ccont_alts_69 = stgPopCont();
  PtrOrLiteral ys = ccont_alts_69->payload[0];
  PtrOrLiteral scrut_alts_69 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      // scrutinee may heap alloc
      Obj *ccont_alts_70 = stgAllocCont( &it_alts_70);
          // no FVs
        stgCurVal = ys; // ys
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h1 t1 ->
    case 1: {
      // scrutinee may heap alloc
      Obj *ccont_alts_71 = stgAllocCont( &it_alts_71);
          // load payload with FVs h1 t1
        ccont_alts_71->payload[0] = scrut_alts_69.op->payload[0]; // h1
        ccont_alts_71->payload[1] = scrut_alts_69.op->payload[1]; // t1
      stgCurVal = ys; // ys
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  ENDFUN;
}


// Bool[B] 
<<<<<<< HEAD
DEFUN0(alts_5) {
  fprintf(stderr, "alts_5 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_5 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
=======
DEFUN0(alts_70) {
  fprintf(stderr, "alts_70 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_70 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_true); // true
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
=======
    // Cons h t ->
    case 1: {
      stgCurVal = HOTOPL(&sho_false); // false
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN0(alts_71) {
  fprintf(stderr, "alts_71 here\n");
  Obj *ccont_alts_71 = stgPopCont();
  PtrOrLiteral h1 = ccont_alts_71->payload[0];
  PtrOrLiteral t1 = ccont_alts_71->payload[1];
  PtrOrLiteral scrut_alts_71 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // Cons h2 t2 ->
    case 1: {
      Obj *expr_0 = stgNewHeapObj( &it_expr_0 );
      expr_0->payload[1] = h1; // h1
      expr_0->payload[2] = scrut_alts_71.op->payload[0]; // h2
      // scrutinee may heap alloc
      Obj *ccont_alts_72 = stgAllocCont( &it_alts_72);
          // load payload with FVs t1 t2
        ccont_alts_72->payload[0] = t1; // t1
        ccont_alts_72->payload[1] = scrut_alts_71.op->payload[1]; // t2
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // expr_0
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


// Bool[B] 
DEFUN1(fun_expr_0, self) {
  fprintf(stderr, "expr_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt h1 h2
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "expr_0 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

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
=======
// Bool[B] 
DEFUN0(alts_72) {
  fprintf(stderr, "alts_72 here\n");
  Obj *ccont_alts_72 = stgPopCont();
  PtrOrLiteral t1 = ccont_alts_72->payload[0];
  PtrOrLiteral t2 = ccont_alts_72->payload[1];
  PtrOrLiteral scrut_alts_72 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      Obj *final_0 = stgNewHeapObj( &it_final_0 );
      final_0->payload[1] = t1; // t1
      final_0->payload[2] = t2; // t2
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // final_0
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN1(fun_final_0, self) {
  fprintf(stderr, "final_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqList t1 t2
  STGAPPLYPP(HOTOPL(&sho_eqList), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "final_0 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

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
=======
// Bool[B]  -> Bool[B] 
// ((["b"],[]),([Bool[B] ],[]))
DEFUN2(fun_not, self, b) {
  fprintf(stderr, "not here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_59 = stgAllocCont( &it_alts_59);
      // no FVs
    stgCurVal = b; // b
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "not returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN0(alts_59) {
  fprintf(stderr, "alts_59 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_59 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // False  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_true); // true
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
    // False  ->
    case 0: {
      // scrutinee may heap alloc
      Obj *ccont_alts_45 = stgAllocCont( &it_alts_45);
          // load payload with FVs n
        ccont_alts_45->payload[0] = n; // n
      stgCurVal = xs; // xs
=======
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_false); // false
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// UBInt -> Bool[B] 
// (([],["i_h"]),([],[UBInt]))
DEFUN2(fun_odd_h, self, i_h) {
  fprintf(stderr, "odd_h here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_56 = stgAllocCont( &it_alts_56);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (i_h).i % 2;
  fprintf(stderr, "odd_h returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_56) {
  fprintf(stderr, "alts_56 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_56 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
<<<<<<< HEAD
    // Cons h t ->
    case 1: {
      Obj *m_1 = stgNewHeapObj( &it_m_1 );
      m_1->payload[1] = n; // n
      // INDIRECT TAIL CALL drop m_1 t
      STGAPPLYPP(HOTOPL(&sho_drop), HOTOPL((Obj *)STGHEAPAT(2,1)), scrut_alts_45.op->payload[1]);
=======
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      // boxed EAtom 
      STGEVAL(stgCurVal);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// Int[B] 
DEFUN1(fun_m_1, self) {
  fprintf(stderr, "m_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "m_1 returning\n");
=======
// Int[B]  -> Bool[B] 
// ((["i"],[]),([Int[B] ],[]))
DEFUN2(fun_odd, self, i) {
  fprintf(stderr, "odd here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_58 = stgAllocCont( &it_alts_58);
      // no FVs
    stgCurVal = i; // i
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "odd returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_58) {
  fprintf(stderr, "alts_58 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_58 = stgCurVal;
  // I i_h ->
  // INDIRECT TAIL CALL odd_h i_h
  STGAPPLYN(HOTOPL(&sho_odd_h), scrut_alts_58.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> Bool[B] 
DEFUN1(fun_even, self) {
  fprintf(stderr, "even here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL compose not odd
  STGAPPLYPP(HOTOPL(&sho_compose), HOTOPL(&sho_not), HOTOPL(&sho_odd));
  fprintf(stderr, "even returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt -> Bool[B] 
// (([],["i_h"]),([],[UBInt]))
DEFUN2(fun_even_h, self, i_h) {
  fprintf(stderr, "even_h here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_57 = stgAllocCont( &it_alts_57);
      // no FVs
    // INDIRECT TAIL CALL odd_h i_h
  STGAPPLYN(HOTOPL(&sho_odd_h), i_h);
  fprintf(stderr, "even_h returning\n");
  STGRETURN0();
  ENDFUN;
}

// Bool[B] 
DEFUN0(alts_57) {
  fprintf(stderr, "alts_57 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_57 = stgCurVal;
  // x ->
  // INDIRECT TAIL CALL not x
  STGAPPLYP(HOTOPL(&sho_not), scrut_alts_57);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t456.(t456 -> Bool[B] ) -> List[B] t456 -> List[B] t456
// ((["p","xs"],[]),([t456 -> Bool[B] ,List[B] t456],[]))
DEFUN3(fun_filter, self, p, xs) {
  fprintf(stderr, "filter here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_53 = stgAllocCont( &it_alts_53);
      // load payload with FVs p
    ccont_alts_53->payload[0] = p; // p
=======

// forall t745.(t745 -> Bool[B] ) -> List[B] t745 -> List[B] t745
// ((["p","xs"],[]),([t745 -> Bool[B] ,List[B] t745],[]))
DEFUN3(fun_filter, self, p, xs) {
  fprintf(stderr, "filter here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_49 = stgAllocCont( &it_alts_49);
      // load payload with FVs p
    ccont_alts_49->payload[0] = p; // p
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "filter returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t456
DEFUN0(alts_53) {
  fprintf(stderr, "alts_53 here\n");
  Obj *ccont_alts_53 = stgPopCont();
  PtrOrLiteral p = ccont_alts_53->payload[0];
  PtrOrLiteral scrut_alts_53 = stgCurVal;
=======
// List[B] t745
DEFUN0(alts_49) {
  fprintf(stderr, "alts_49 here\n");
  Obj *ccont_alts_49 = stgPopCont();
  PtrOrLiteral p = ccont_alts_49->payload[0];
  PtrOrLiteral scrut_alts_49 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      tail_0->payload[2] = scrut_alts_53.op->payload[1]; // t
      // scrutinee may heap alloc
      Obj *ccont_alts_54 = stgAllocCont( &it_alts_54);
          // load payload with FVs h tail_0
        ccont_alts_54->payload[0] = scrut_alts_53.op->payload[0]; // h
        ccont_alts_54->payload[1] = HOTOPL((Obj *)STGHEAPAT(3,1)); // tail_0
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_53.op->payload[0]);
=======
      tail_0->payload[2] = scrut_alts_49.op->payload[1]; // t
      // scrutinee may heap alloc
      Obj *ccont_alts_50 = stgAllocCont( &it_alts_50);
          // load payload with FVs h tail_0
        ccont_alts_50->payload[0] = scrut_alts_49.op->payload[0]; // h
        ccont_alts_50->payload[1] = HOTOPL((Obj *)STGHEAPAT(3,1)); // tail_0
      // INDIRECT TAIL CALL p h
      STGAPPLYP(p, scrut_alts_49.op->payload[0]);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// List[B] t456
=======
// List[B] t745
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_tail_0, self) {
  fprintf(stderr, "tail_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL filter p t
  STGAPPLYPP(HOTOPL(&sho_filter), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "tail_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t456
DEFUN0(alts_54) {
  fprintf(stderr, "alts_54 here\n");
  Obj *ccont_alts_54 = stgPopCont();
  PtrOrLiteral h = ccont_alts_54->payload[0];
  PtrOrLiteral tail_0 = ccont_alts_54->payload[1];
  PtrOrLiteral scrut_alts_54 = stgCurVal;
=======
// List[B] t745
DEFUN0(alts_50) {
  fprintf(stderr, "alts_50 here\n");
  Obj *ccont_alts_50 = stgPopCont();
  PtrOrLiteral h = ccont_alts_50->payload[0];
  PtrOrLiteral tail_0 = ccont_alts_50->payload[1];
  PtrOrLiteral scrut_alts_50 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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


<<<<<<< HEAD
// forall t238,t244.(t244 -> t238 -> t244) -> t244 -> List[B] t238 -> t244
// ((["f","acc","list"],[]),([t244 -> t238 -> t244,t244,List[B] t238],[]))
DEFUN4(fun_foldl, self, f, acc, list) {
  fprintf(stderr, "foldl here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_36 = stgAllocCont( &it_alts_36);
      // load payload with FVs acc f
    ccont_alts_36->payload[0] = acc; // acc
    ccont_alts_36->payload[1] = f; // f
=======
// forall t226,t232.(t232 -> t226 -> t232) -> t232 -> List[B] t226 -> t232
// ((["f","acc","list"],[]),([t232 -> t226 -> t232,t232,List[B] t226],[]))
DEFUN4(fun_foldl, self, f, acc, list) {
  fprintf(stderr, "foldl here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_32 = stgAllocCont( &it_alts_32);
      // load payload with FVs acc f
    ccont_alts_32->payload[0] = acc; // acc
    ccont_alts_32->payload[1] = f; // f
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "foldl returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t244
DEFUN0(alts_36) {
  fprintf(stderr, "alts_36 here\n");
  Obj *ccont_alts_36 = stgPopCont();
  PtrOrLiteral acc = ccont_alts_36->payload[0];
  PtrOrLiteral f = ccont_alts_36->payload[1];
  PtrOrLiteral scrut_alts_36 = stgCurVal;
=======
// t232
DEFUN0(alts_32) {
  fprintf(stderr, "alts_32 here\n");
  Obj *ccont_alts_32 = stgPopCont();
  PtrOrLiteral acc = ccont_alts_32->payload[0];
  PtrOrLiteral f = ccont_alts_32->payload[1];
  PtrOrLiteral scrut_alts_32 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      newAcc_0->payload[3] = scrut_alts_36.op->payload[0]; // h
      // INDIRECT TAIL CALL foldl f newAcc_0 t
      STGAPPLYPPP(HOTOPL(&sho_foldl), f, HOTOPL((Obj *)STGHEAPAT(4,1)), scrut_alts_36.op->payload[1]);
=======
      newAcc_0->payload[3] = scrut_alts_32.op->payload[0]; // h
      // INDIRECT TAIL CALL foldl f newAcc_0 t
      STGAPPLYPPP(HOTOPL(&sho_foldl), f, HOTOPL((Obj *)STGHEAPAT(4,1)), scrut_alts_32.op->payload[1]);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// t244
=======
// t232
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_newAcc_0, self) {
  fprintf(stderr, "newAcc_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL f acc h
  STGAPPLYPP(self.op->payload[2], self.op->payload[1], self.op->payload[3]);
  fprintf(stderr, "newAcc_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t252,t259.(t252 -> t259 -> t259) -> t259 -> List[B] t252 -> t259
// ((["f","sd","list"],[]),([t252 -> t259 -> t259,t259,List[B] t252],[]))
DEFUN4(fun_foldr, self, f, sd, list) {
  fprintf(stderr, "foldr here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_37 = stgAllocCont( &it_alts_37);
      // load payload with FVs f sd
    ccont_alts_37->payload[0] = f; // f
    ccont_alts_37->payload[1] = sd; // sd
=======
// forall t240,t247.(t240 -> t247 -> t247) -> t247 -> List[B] t240 -> t247
// ((["f","sd","list"],[]),([t240 -> t247 -> t247,t247,List[B] t240],[]))
DEFUN4(fun_foldr, self, f, sd, list) {
  fprintf(stderr, "foldr here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_33 = stgAllocCont( &it_alts_33);
      // load payload with FVs f sd
    ccont_alts_33->payload[0] = f; // f
    ccont_alts_33->payload[1] = sd; // sd
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "foldr returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t259
DEFUN0(alts_37) {
  fprintf(stderr, "alts_37 here\n");
  Obj *ccont_alts_37 = stgPopCont();
  PtrOrLiteral f = ccont_alts_37->payload[0];
  PtrOrLiteral sd = ccont_alts_37->payload[1];
  PtrOrLiteral scrut_alts_37 = stgCurVal;
=======
// t247
DEFUN0(alts_33) {
  fprintf(stderr, "alts_33 here\n");
  Obj *ccont_alts_33 = stgPopCont();
  PtrOrLiteral f = ccont_alts_33->payload[0];
  PtrOrLiteral sd = ccont_alts_33->payload[1];
  PtrOrLiteral scrut_alts_33 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      res_1->payload[3] = scrut_alts_37.op->payload[1]; // t
      // INDIRECT TAIL CALL f h res_1
      STGAPPLYPP(f, scrut_alts_37.op->payload[0], HOTOPL((Obj *)STGHEAPAT(4,1)));
=======
      res_1->payload[3] = scrut_alts_33.op->payload[1]; // t
      // INDIRECT TAIL CALL f h res_1
      STGAPPLYPP(f, scrut_alts_33.op->payload[0], HOTOPL((Obj *)STGHEAPAT(4,1)));
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// t259
=======
// t247
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_res_1, self) {
  fprintf(stderr, "res_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL foldr f sd t
  STGAPPLYPPP(HOTOPL(&sho_foldr), self.op->payload[1], self.op->payload[2], self.op->payload[3]);
  fprintf(stderr, "res_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t58,t59.t58 -> t59 -> t59
// ((["x","y"],[]),([t58,t59],[]))
DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_0 = stgAllocCont( &it_alts_0);
      // load payload with FVs y
    ccont_alts_0->payload[0] = y; // y
=======
// forall t415,t416.t415 -> t416 -> t416
// ((["x","y"],[]),([t415,t416],[]))
DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_55 = stgAllocCont( &it_alts_55);
      // load payload with FVs y
    ccont_alts_55->payload[0] = y; // y
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "seq returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t59
DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  Obj *ccont_alts_0 = stgPopCont();
  PtrOrLiteral y = ccont_alts_0->payload[0];
  PtrOrLiteral scrut_alts_0 = stgCurVal;
=======
// t416
DEFUN0(alts_55) {
  fprintf(stderr, "alts_55 here\n");
  Obj *ccont_alts_55 = stgPopCont();
  PtrOrLiteral y = ccont_alts_55->payload[0];
  PtrOrLiteral scrut_alts_55 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  // z ->
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


<<<<<<< HEAD
// forall t451.List[B] t451 -> Unit[B] 
// ((["list"],[]),([List[B] t451],[]))
DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_41 = stgAllocCont( &it_alts_41);
=======
// forall t740.List[B] t740 -> Unit[B] 
// ((["list"],[]),([List[B] t740],[]))
DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_37 = stgAllocCont( &it_alts_37);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // no FVs
    stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "forcelist returning\n");
  STGRETURN0();
  ENDFUN;
}

// Unit[B] 
<<<<<<< HEAD
DEFUN0(alts_41) {
  fprintf(stderr, "alts_41 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_41 = stgCurVal;
=======
DEFUN0(alts_37) {
  fprintf(stderr, "alts_37 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_37 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      rec_2->payload[1] = scrut_alts_41.op->payload[1]; // t
      // INDIRECT TAIL CALL seq h rec_2
      STGAPPLYPP(HOTOPL(&sho_seq), scrut_alts_41.op->payload[0], HOTOPL((Obj *)STGHEAPAT(2,1)));
=======
      rec_2->payload[1] = scrut_alts_37.op->payload[1]; // t
      // INDIRECT TAIL CALL seq h rec_2
      STGAPPLYPP(HOTOPL(&sho_seq), scrut_alts_37.op->payload[0], HOTOPL((Obj *)STGHEAPAT(2,1)));
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// forall t79,t80.Tupl2[B] t79 t80 -> t79
// ((["t2"],[]),([Tupl2[B] t79 t80],[]))
DEFUN2(fun_fst, self, t2) {
  fprintf(stderr, "fst here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_1 = stgAllocCont( &it_alts_1);
=======
// forall t75,t76.Tupl2[B] t75 t76 -> t75
// ((["t2"],[]),([Tupl2[B] t75 t76],[]))
DEFUN2(fun_fst, self, t2) {
  fprintf(stderr, "fst here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_0 = stgAllocCont( &it_alts_0);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // no FVs
    stgCurVal = t2; // t2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "fst returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t79
DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // TP2 a b ->
  stgCurVal = scrut_alts_1.op->payload[0]; // a
=======
// t75
DEFUN0(alts_0) {
  fprintf(stderr, "alts_0 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_0 = stgCurVal;
  // TP2 a b ->
  stgCurVal = scrut_alts_0.op->payload[0]; // a
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
  Obj *ccont_alts_27 = stgAllocCont( &it_alts_27);
      // load payload with FVs a_h b_h
    ccont_alts_27->payload[0] = a_h; // a_h
    ccont_alts_27->payload[1] = b_h; // b_h
=======
  Obj *ccont_alts_23 = stgAllocCont( &it_alts_23);
      // load payload with FVs a_h b_h
    ccont_alts_23->payload[0] = a_h; // a_h
    ccont_alts_23->payload[1] = b_h; // b_h
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = b_h; // b_h
  // unboxed EAtom
  fprintf(stderr, "gcd_h returning\n");
  STGRETURN0();
  ENDFUN;
}

// UBInt
<<<<<<< HEAD
DEFUN0(alts_27) {
  fprintf(stderr, "alts_27 here\n");
  Obj *ccont_alts_27 = stgPopCont();
  PtrOrLiteral a_h = ccont_alts_27->payload[0];
  PtrOrLiteral b_h = ccont_alts_27->payload[1];
  PtrOrLiteral scrut_alts_27 = stgCurVal;
=======
DEFUN0(alts_23) {
  fprintf(stderr, "alts_23 here\n");
  Obj *ccont_alts_23 = stgPopCont();
  PtrOrLiteral a_h = ccont_alts_23->payload[0];
  PtrOrLiteral b_h = ccont_alts_23->payload[1];
  PtrOrLiteral scrut_alts_23 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      Obj *ccont_alts_28 = stgAllocCont( &it_alts_28);
          // load payload with FVs b_h
        ccont_alts_28->payload[0] = b_h; // b_h
=======
      Obj *ccont_alts_24 = stgAllocCont( &it_alts_24);
          // load payload with FVs b_h
        ccont_alts_24->payload[0] = b_h; // b_h
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      stgCurVal.argType = INT;
      stgCurVal.i = (a_h).i % (b_h).i;
      STGRETURN0();
    }
  }
  ENDFUN;
}


// UBInt
<<<<<<< HEAD
DEFUN0(alts_28) {
  fprintf(stderr, "alts_28 here\n");
  Obj *ccont_alts_28 = stgPopCont();
  PtrOrLiteral b_h = ccont_alts_28->payload[0];
  PtrOrLiteral scrut_alts_28 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL gcd_h b_h r_h
  STGAPPLYNN(HOTOPL(&sho_gcd_h), b_h, scrut_alts_28);
=======
DEFUN0(alts_24) {
  fprintf(stderr, "alts_24 here\n");
  Obj *ccont_alts_24 = stgPopCont();
  PtrOrLiteral b_h = ccont_alts_24->payload[0];
  PtrOrLiteral scrut_alts_24 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL gcd_h b_h r_h
  STGAPPLYNN(HOTOPL(&sho_gcd_h), b_h, scrut_alts_24);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> Int[B]  -> Int[B] 
// ((["a","b"],[]),([Int[B] ,Int[B] ],[]))
DEFUN3(fun_gcd, self, a, b) {
  fprintf(stderr, "gcd here\n");
  // scrutinee may heap alloc
<<<<<<< HEAD
  Obj *ccont_alts_29 = stgAllocCont( &it_alts_29);
      // load payload with FVs b
    ccont_alts_29->payload[0] = b; // b
=======
  Obj *ccont_alts_25 = stgAllocCont( &it_alts_25);
      // load payload with FVs b
    ccont_alts_25->payload[0] = b; // b
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = a; // a
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "gcd returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
<<<<<<< HEAD
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
=======
DEFUN0(alts_25) {
  fprintf(stderr, "alts_25 here\n");
  Obj *ccont_alts_25 = stgPopCont();
  PtrOrLiteral b = ccont_alts_25->payload[0];
  PtrOrLiteral scrut_alts_25 = stgCurVal;
  // I a_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_26 = stgAllocCont( &it_alts_26);
      // load payload with FVs a_h
    ccont_alts_26->payload[0] = scrut_alts_25.op->payload[0]; // a_h
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = b; // b
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
<<<<<<< HEAD
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
=======
DEFUN0(alts_26) {
  fprintf(stderr, "alts_26 here\n");
  Obj *ccont_alts_26 = stgPopCont();
  PtrOrLiteral a_h = ccont_alts_26->payload[0];
  PtrOrLiteral scrut_alts_26 = stgCurVal;
  // I b_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_27 = stgAllocCont( &it_alts_27);
      // no FVs
    // INDIRECT TAIL CALL gcd_h a_h b_h
  STGAPPLYNN(HOTOPL(&sho_gcd_h), a_h, scrut_alts_26.op->payload[0]);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
<<<<<<< HEAD
DEFUN0(alts_31) {
  fprintf(stderr, "alts_31 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_31 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_31);
=======
DEFUN0(alts_27) {
  fprintf(stderr, "alts_27 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_27 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_27);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}


<<<<<<< HEAD
// forall t439.List[B] t439 -> t439
// ((["xs"],[]),([List[B] t439],[]))
DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_34 = stgAllocCont( &it_alts_34);
=======
// forall t719.List[B] t719 -> t719
// ((["xs"],[]),([List[B] t719],[]))
DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_30 = stgAllocCont( &it_alts_30);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "head returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t439
DEFUN0(alts_34) {
  fprintf(stderr, "alts_34 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_34 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_34.op->payload[0]; // hd
=======
// t719
DEFUN0(alts_30) {
  fprintf(stderr, "alts_30 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_30 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_30.op->payload[0]; // hd
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
<<<<<<< HEAD
      Obj *alts_34_exhaust = stgNewHeapObj( &it_alts_34_exhaust );
      alts_34_exhaust->payload[1] = scrut_alts_34; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_34_exhaust
=======
      Obj *alts_30_exhaust = stgNewHeapObj( &it_alts_30_exhaust );
      alts_30_exhaust->payload[1] = scrut_alts_30; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_30_exhaust
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// forall t223.t223
DEFUN1(fun_alts_34_exhaust, self) {
  fprintf(stderr, "alts_34_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_34_exhaust returning\n");
=======
// forall t211.t211
DEFUN1(fun_alts_30_exhaust, self) {
  fprintf(stderr, "alts_30_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_30_exhaust returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// forall t731.Int[B]  -> List[B] t731 -> t731
// ((["n","xs"],[]),([Int[B] ,List[B] t731],[]))
DEFUN3(fun_index, self, n, xs) {
  fprintf(stderr, "index here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_64 = stgAllocCont( &it_alts_64);
      // load payload with FVs n
    ccont_alts_64->payload[0] = n; // n
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "index returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// Bool[B] 
DEFUN0(alts_50) {
  fprintf(stderr, "alts_50 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_50 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_true); // true
=======
// t731
DEFUN0(alts_64) {
  fprintf(stderr, "alts_64 here\n");
  Obj *ccont_alts_64 = stgPopCont();
  PtrOrLiteral n = ccont_alts_64->payload[0];
  PtrOrLiteral scrut_alts_64 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons h1 t1 ->
    case 1: {
      Obj *a_2 = stgNewHeapObj( &it_a_2 );
      a_2->payload[1] = n; // n
      // scrutinee may heap alloc
      Obj *ccont_alts_65 = stgAllocCont( &it_alts_65);
          // load payload with FVs h1 n t1
        ccont_alts_65->payload[0] = scrut_alts_64.op->payload[0]; // h1
        ccont_alts_65->payload[1] = n; // n
        ccont_alts_65->payload[2] = scrut_alts_64.op->payload[1]; // t1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // a_2
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
<<<<<<< HEAD
      stgCurVal = HOTOPL(&sho_false); // false
=======
      Obj *alts_64_exhaust = stgNewHeapObj( &it_alts_64_exhaust );
      alts_64_exhaust->payload[1] = scrut_alts_64; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_64_exhaust
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN1(fun_a_2, self) {
  fprintf(stderr, "a_2 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], HOTOPL(&sho_zero));
  fprintf(stderr, "a_2 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// t731
DEFUN0(alts_65) {
  fprintf(stderr, "alts_65 here\n");
  Obj *ccont_alts_65 = stgPopCont();
  PtrOrLiteral h1 = ccont_alts_65->payload[0];
  PtrOrLiteral n = ccont_alts_65->payload[1];
  PtrOrLiteral t1 = ccont_alts_65->payload[2];
  PtrOrLiteral scrut_alts_65 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = h1; // h1
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
<<<<<<< HEAD
      Obj *l_0 = stgNewHeapObj( &it_l_0 );
      l_0->payload[1] = t; // t
      // INDIRECT TAIL CALL cons h l_0
      STGAPPLYPP(HOTOPL(&sho_cons), h, HOTOPL((Obj *)STGHEAPAT(2,1)));
=======
      Obj *c_1 = stgNewHeapObj( &it_c_1 );
      Obj *d_1 = stgNewHeapObj( &it_d_1 );
      c_1->payload[1] = n; // n
      d_1->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // c_1
      d_1->payload[2] = t1; // t1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // d_1
      // boxed EAtom 
      STGEVAL(stgCurVal);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// List[B] t455
DEFUN1(fun_l_0, self) {
  fprintf(stderr, "l_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL init t
  STGAPPLYP(HOTOPL(&sho_init), self.op->payload[1]);
  fprintf(stderr, "l_0 returning\n");
=======
// Int[B] 
DEFUN1(fun_c_1, self) {
  fprintf(stderr, "c_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL subInt n one
  STGAPPLYPP(HOTOPL(&sho_subInt), self.op->payload[1], HOTOPL(&sho_one));
  fprintf(stderr, "c_1 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// Int[B]  -> Int[B]  -> Bool[B] 
DEFUN1(fun_intLE, self) {
  fprintf(stderr, "intLE here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL _intComp _ile
  STGAPPLYP(HOTOPL(&sho__intComp), HOTOPL(&sho__ile));
  fprintf(stderr, "intLE returning\n");
=======
// t731
DEFUN1(fun_d_1, self) {
  fprintf(stderr, "d_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL index c_1 t1
  STGAPPLYPP(HOTOPL(&sho_index), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "d_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t509.t509
DEFUN1(fun_alts_64_exhaust, self) {
  fprintf(stderr, "alts_64_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_64_exhaust returning\n");
  STGRETURN0();
  ENDFUN;
}

// forall t347.List[B] t347 -> Bool[B] 
// ((["xs"],[]),([List[B] t347],[]))
DEFUN2(fun_null, self, xs) {
  fprintf(stderr, "null here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_46 = stgAllocCont( &it_alts_46);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "null returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN0(alts_46) {
  fprintf(stderr, "alts_46 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_46 = stgCurVal;
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


// forall t744.List[B] t744 -> List[B] t744
// ((["xs"],[]),([List[B] t744],[]))
DEFUN2(fun_init, self, xs) {
  fprintf(stderr, "init here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_47 = stgAllocCont( &it_alts_47);
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "init returning\n");
  STGRETURN0();
  ENDFUN;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
}

// List[B] t744
DEFUN0(alts_47) {
  fprintf(stderr, "alts_47 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_47 = stgCurVal;
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
      Obj *ccont_alts_48 = stgAllocCont( &it_alts_48);
          // load payload with FVs h t
        ccont_alts_48->payload[0] = scrut_alts_47.op->payload[0]; // h
        ccont_alts_48->payload[1] = scrut_alts_47.op->payload[1]; // t
      // INDIRECT TAIL CALL null t
      STGAPPLYP(HOTOPL(&sho_null), scrut_alts_47.op->payload[1]);
      STGRETURN0();
    }
  }
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t438
DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL map f t
  STGAPPLYPP(HOTOPL(&sho_map), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "rec_1 returning\n");
=======

// List[B] t744
DEFUN0(alts_48) {
  fprintf(stderr, "alts_48 here\n");
  Obj *ccont_alts_48 = stgPopCont();
  PtrOrLiteral h = ccont_alts_48->payload[0];
  PtrOrLiteral t = ccont_alts_48->payload[1];
  PtrOrLiteral scrut_alts_48 = stgCurVal;
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


// List[B] t744
DEFUN1(fun_l_0, self) {
  fprintf(stderr, "l_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL init t
  STGAPPLYP(HOTOPL(&sho_init), self.op->payload[1]);
  fprintf(stderr, "l_0 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t438
=======
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

// forall t739.List[B] t739 -> Int[B] 
// ((["xs"],[]),([List[B] t739],[]))
DEFUN2(fun_length, self, xs) {
  fprintf(stderr, "length here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_34 = stgAllocCont( &it_alts_34);
      // no FVs
    // INDIRECT TAIL CALL _length xs 0
  STGAPPLYPN(HOTOPL(&sho__length), xs, ((PtrOrLiteral){.argType = INT,    .i = 0 }));
  fprintf(stderr, "length returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_34) {
  fprintf(stderr, "alts_34 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_34 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_34);
  STGRETURN0();
  ENDFUN;
}


// forall t196,t718.(t196 -> t718) -> List[B] t196 -> List[B] t718
// ((["f","list"],[]),([t196 -> t718,List[B] t196],[]))
DEFUN3(fun_map, self, f, list) {
  fprintf(stderr, "map here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_29 = stgAllocCont( &it_alts_29);
      // load payload with FVs f
    ccont_alts_29->payload[0] = f; // f
  stgCurVal = list; // list
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "map returning\n");
  STGRETURN0();
  ENDFUN;
}

// List[B] t718
DEFUN0(alts_29) {
  fprintf(stderr, "alts_29 here\n");
  Obj *ccont_alts_29 = stgPopCont();
  PtrOrLiteral f = ccont_alts_29->payload[0];
  PtrOrLiteral scrut_alts_29 = stgCurVal;
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
      rec_1->payload[2] = scrut_alts_29.op->payload[1]; // t
      x_0->payload[1] = f; // f
      x_0->payload[2] = scrut_alts_29.op->payload[0]; // h
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


// List[B] t718
DEFUN1(fun_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL map f t
  STGAPPLYPP(HOTOPL(&sho_map), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "rec_1 returning\n");
  STGRETURN0();
  ENDFUN;
}

// t718
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
  Obj *ccont_alts_26 = stgAllocCont( &it_alts_26);
      // load payload with FVs a b
    ccont_alts_26->payload[0] = a; // a
    ccont_alts_26->payload[1] = b; // b
=======
  Obj *ccont_alts_22 = stgAllocCont( &it_alts_22);
      // load payload with FVs a b
    ccont_alts_22->payload[0] = a; // a
    ccont_alts_22->payload[1] = b; // b
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  // INDIRECT TAIL CALL intLE a b
  STGAPPLYPP(HOTOPL(&sho_intLE), a, b);
  fprintf(stderr, "minInt returning\n");
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
<<<<<<< HEAD
DEFUN0(alts_26) {
  fprintf(stderr, "alts_26 here\n");
  Obj *ccont_alts_26 = stgPopCont();
  PtrOrLiteral a = ccont_alts_26->payload[0];
  PtrOrLiteral b = ccont_alts_26->payload[1];
  PtrOrLiteral scrut_alts_26 = stgCurVal;
=======
DEFUN0(alts_22) {
  fprintf(stderr, "alts_22 here\n");
  Obj *ccont_alts_22 = stgPopCont();
  PtrOrLiteral a = ccont_alts_22->payload[0];
  PtrOrLiteral b = ccont_alts_22->payload[1];
  PtrOrLiteral scrut_alts_22 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
  Obj *ccont_alts_18 = stgAllocCont( &it_alts_18);
      // load payload with FVs y
    ccont_alts_18->payload[0] = y; // y
=======
  Obj *ccont_alts_14 = stgAllocCont( &it_alts_14);
      // load payload with FVs y
    ccont_alts_14->payload[0] = y; // y
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = x; // x
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "modInt returning\n");
<<<<<<< HEAD
=======
  STGRETURN0();
  ENDFUN;
}

// Int[B] 
DEFUN0(alts_14) {
  fprintf(stderr, "alts_14 here\n");
  Obj *ccont_alts_14 = stgPopCont();
  PtrOrLiteral y = ccont_alts_14->payload[0];
  PtrOrLiteral scrut_alts_14 = stgCurVal;
  // I x_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_15 = stgAllocCont( &it_alts_15);
      // load payload with FVs x_h
    ccont_alts_15->payload[0] = scrut_alts_14.op->payload[0]; // x_h
  stgCurVal = y; // y
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_15) {
  fprintf(stderr, "alts_15 here\n");
  Obj *ccont_alts_15 = stgPopCont();
  PtrOrLiteral x_h = ccont_alts_15->payload[0];
  PtrOrLiteral scrut_alts_15 = stgCurVal;
  // I y_h ->
  // scrutinee may heap alloc
  Obj *ccont_alts_16 = stgAllocCont( &it_alts_16);
      // no FVs
    stgCurVal.argType = INT;
  stgCurVal.i = (x_h).i % (scrut_alts_15.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


// Int[B] 
DEFUN0(alts_16) {
  fprintf(stderr, "alts_16 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_16 = stgCurVal;
  // r_h ->
  // INDIRECT TAIL CALL int r_h
  STGAPPLYN(HOTOPL(&sho_int), scrut_alts_16);
  STGRETURN0();
  ENDFUN;
}


// Int[B]  -> List[B] Int[B]  -> List[B] Int[B] 
// ((["n","xs"],[]),([Int[B] ,List[B] Int[B] ],[]))
DEFUN3(fun_remove, self, n, xs) {
  fprintf(stderr, "remove here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_66 = stgAllocCont( &it_alts_66);
      // load payload with FVs n
    ccont_alts_66->payload[0] = n; // n
  stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "remove returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
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
=======
// List[B] Int[B] 
DEFUN0(alts_66) {
  fprintf(stderr, "alts_66 here\n");
  Obj *ccont_alts_66 = stgPopCont();
  PtrOrLiteral n = ccont_alts_66->payload[0];
  PtrOrLiteral scrut_alts_66 = stgCurVal;
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
      Obj *a_3 = stgNewHeapObj( &it_a_3 );
      a_3->payload[1] = scrut_alts_66.op->payload[0]; // h1
      a_3->payload[2] = n; // n
      // scrutinee may heap alloc
      Obj *ccont_alts_67 = stgAllocCont( &it_alts_67);
          // load payload with FVs h1 n t1
        ccont_alts_67->payload[0] = scrut_alts_66.op->payload[0]; // h1
        ccont_alts_67->payload[1] = n; // n
        ccont_alts_67->payload[2] = scrut_alts_66.op->payload[1]; // t1
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(3,1)); // a_3
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// Bool[B] 
DEFUN1(fun_a_3, self) {
  fprintf(stderr, "a_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL eqInt h1 n
  STGAPPLYPP(HOTOPL(&sho_eqInt), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "a_3 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

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
=======
// List[B] Int[B] 
DEFUN0(alts_67) {
  fprintf(stderr, "alts_67 here\n");
  Obj *ccont_alts_67 = stgPopCont();
  PtrOrLiteral h1 = ccont_alts_67->payload[0];
  PtrOrLiteral n = ccont_alts_67->payload[1];
  PtrOrLiteral t1 = ccont_alts_67->payload[2];
  PtrOrLiteral scrut_alts_67 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = t1; // t1
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // False  ->
    case 0: {
      Obj *b_3 = stgNewHeapObj( &it_b_3 );
      Obj *c_2 = stgNewHeapObj( &it_c_2 );
      b_3->payload[1] = n; // n
      b_3->payload[2] = t1; // t1
      c_2->payload[0] = h1; // h1
      c_2->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // b_3
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // c_2
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  ENDFUN;
}


<<<<<<< HEAD
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
=======
// List[B] Int[B] 
DEFUN1(fun_b_3, self) {
  fprintf(stderr, "b_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL remove n t1
  STGAPPLYPP(HOTOPL(&sho_remove), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "b_3 returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD

// forall t459.t459 -> List[B] t459
// ((["x"],[]),([t459],[]))
=======
// forall t748.t748 -> List[B] t748
// ((["x"],[]),([t748],[]))
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// List[B] t459
=======
// List[B] t748
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_next_0, self) {
  fprintf(stderr, "next_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL repeat x
  STGAPPLYP(HOTOPL(&sho_repeat), self.op->payload[1]);
  fprintf(stderr, "next_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t442.Int[B]  -> List[B] t442 -> List[B] t442
// ((["n","xs"],[]),([Int[B] ,List[B] t442],[]))
DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_42 = stgAllocCont( &it_alts_42);
      // load payload with FVs n xs
    ccont_alts_42->payload[0] = n; // n
    ccont_alts_42->payload[1] = xs; // xs
=======
// forall t722.Int[B]  -> List[B] t722 -> List[B] t722
// ((["n","xs"],[]),([Int[B] ,List[B] t722],[]))
DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_38 = stgAllocCont( &it_alts_38);
      // load payload with FVs n xs
    ccont_alts_38->payload[0] = n; // n
    ccont_alts_38->payload[1] = xs; // xs
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  // INDIRECT TAIL CALL eqInt n zero
  STGAPPLYPP(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  fprintf(stderr, "take returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t442
DEFUN0(alts_42) {
  fprintf(stderr, "alts_42 here\n");
  Obj *ccont_alts_42 = stgPopCont();
  PtrOrLiteral n = ccont_alts_42->payload[0];
  PtrOrLiteral xs = ccont_alts_42->payload[1];
  PtrOrLiteral scrut_alts_42 = stgCurVal;
=======
// List[B] t722
DEFUN0(alts_38) {
  fprintf(stderr, "alts_38 here\n");
  Obj *ccont_alts_38 = stgPopCont();
  PtrOrLiteral n = ccont_alts_38->payload[0];
  PtrOrLiteral xs = ccont_alts_38->payload[1];
  PtrOrLiteral scrut_alts_38 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      Obj *ccont_alts_43 = stgAllocCont( &it_alts_43);
          // load payload with FVs n
        ccont_alts_43->payload[0] = n; // n
=======
      Obj *ccont_alts_39 = stgAllocCont( &it_alts_39);
          // load payload with FVs n
        ccont_alts_39->payload[0] = n; // n
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      stgCurVal = xs; // xs
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// List[B] t442
DEFUN0(alts_43) {
  fprintf(stderr, "alts_43 here\n");
  Obj *ccont_alts_43 = stgPopCont();
  PtrOrLiteral n = ccont_alts_43->payload[0];
  PtrOrLiteral scrut_alts_43 = stgCurVal;
=======
// List[B] t722
DEFUN0(alts_39) {
  fprintf(stderr, "alts_39 here\n");
  Obj *ccont_alts_39 = stgPopCont();
  PtrOrLiteral n = ccont_alts_39->payload[0];
  PtrOrLiteral scrut_alts_39 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      Obj *result_5 = stgNewHeapObj( &it_result_5 );
      m_0->payload[1] = n; // n
      rec_3->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // m_0
      rec_3->payload[2] = scrut_alts_43.op->payload[1]; // tl
      result_5->payload[0] = scrut_alts_43.op->payload[0]; // hd
      result_5->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // rec_3
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_5
=======
      Obj *result_4 = stgNewHeapObj( &it_result_4 );
      m_0->payload[1] = n; // n
      rec_3->payload[1] = HOTOPL((Obj *)STGHEAPAT(7,3)); // m_0
      rec_3->payload[2] = scrut_alts_39.op->payload[1]; // tl
      result_4->payload[0] = scrut_alts_39.op->payload[0]; // hd
      result_4->payload[1] = HOTOPL((Obj *)STGHEAPAT(5,2)); // rec_3
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_4
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// List[B] t442
=======
// List[B] t722
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_rec_3, self) {
  fprintf(stderr, "rec_3 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL take m_0 tl
  STGAPPLYPP(HOTOPL(&sho_take), self.op->payload[1], self.op->payload[2]);
  fprintf(stderr, "rec_3 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t465.Int[B]  -> t465 -> List[B] t465
// ((["n","x"],[]),([Int[B] ,t465],[]))
=======
// forall t757.Int[B]  -> t757 -> List[B] t757
// ((["n","x"],[]),([Int[B] ,t757],[]))
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// List[B] t465
=======
// List[B] t757
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_list_0, self) {
  fprintf(stderr, "list_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL repeat x
  STGAPPLYP(HOTOPL(&sho_repeat), self.op->payload[1]);
  fprintf(stderr, "list_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t84,t85.Tupl2[B] t84 t85 -> t85
// ((["t2"],[]),([Tupl2[B] t84 t85],[]))
DEFUN2(fun_snd, self, t2) {
  fprintf(stderr, "snd here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_2 = stgAllocCont( &it_alts_2);
=======
// forall t80,t81.Tupl2[B] t80 t81 -> t81
// ((["t2"],[]),([Tupl2[B] t80 t81],[]))
DEFUN2(fun_snd, self, t2) {
  fprintf(stderr, "snd here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_1 = stgAllocCont( &it_alts_1);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // no FVs
    stgCurVal = t2; // t2
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "snd returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// t85
DEFUN0(alts_2) {
  fprintf(stderr, "alts_2 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_2 = stgCurVal;
  // TP2 a b ->
  stgCurVal = scrut_alts_2.op->payload[1]; // b
=======
// t81
DEFUN0(alts_1) {
  fprintf(stderr, "alts_1 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_1 = stgCurVal;
  // TP2 a b ->
  stgCurVal = scrut_alts_1.op->payload[1]; // b
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  // boxed EAtom 
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}


<<<<<<< HEAD
// forall t453.List[B] t453 -> List[B] t453
// ((["xs"],[]),([List[B] t453],[]))
DEFUN2(fun_strictList, self, xs) {
  fprintf(stderr, "strictList here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_48 = stgAllocCont( &it_alts_48);
=======
// forall t742.List[B] t742 -> List[B] t742
// ((["xs"],[]),([List[B] t742],[]))
DEFUN2(fun_strictList, self, xs) {
  fprintf(stderr, "strictList here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_44 = stgAllocCont( &it_alts_44);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "strictList returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t453
DEFUN0(alts_48) {
  fprintf(stderr, "alts_48 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_48 = stgCurVal;
=======
// List[B] t742
DEFUN0(alts_44) {
  fprintf(stderr, "alts_44 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_44 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      Obj *ccont_alts_49 = stgAllocCont( &it_alts_49);
          // load payload with FVs h
        ccont_alts_49->payload[0] = scrut_alts_48.op->payload[0]; // h
      // INDIRECT TAIL CALL strictList t
      STGAPPLYP(HOTOPL(&sho_strictList), scrut_alts_48.op->payload[1]);
=======
      Obj *ccont_alts_45 = stgAllocCont( &it_alts_45);
          // load payload with FVs h
        ccont_alts_45->payload[0] = scrut_alts_44.op->payload[0]; // h
      // INDIRECT TAIL CALL strictList t
      STGAPPLYP(HOTOPL(&sho_strictList), scrut_alts_44.op->payload[1]);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// List[B] t453
DEFUN0(alts_49) {
  fprintf(stderr, "alts_49 here\n");
  Obj *ccont_alts_49 = stgPopCont();
  PtrOrLiteral h = ccont_alts_49->payload[0];
  PtrOrLiteral scrut_alts_49 = stgCurVal;
  // x ->
  // INDIRECT TAIL CALL cons h x
  STGAPPLYPP(HOTOPL(&sho_cons), h, scrut_alts_49);
=======
// List[B] t742
DEFUN0(alts_45) {
  fprintf(stderr, "alts_45 here\n");
  Obj *ccont_alts_45 = stgPopCont();
  PtrOrLiteral h = ccont_alts_45->payload[0];
  PtrOrLiteral scrut_alts_45 = stgCurVal;
  // x ->
  // INDIRECT TAIL CALL cons h x
  STGAPPLYPP(HOTOPL(&sho_cons), h, scrut_alts_45);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// forall t229.List[B] t229 -> List[B] t229
// ((["xs"],[]),([List[B] t229],[]))
DEFUN2(fun_tail, self, xs) {
  fprintf(stderr, "tail here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_35 = stgAllocCont( &it_alts_35);
=======
// forall t217.List[B] t217 -> List[B] t217
// ((["xs"],[]),([List[B] t217],[]))
DEFUN2(fun_tail, self, xs) {
  fprintf(stderr, "tail here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_31 = stgAllocCont( &it_alts_31);
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // no FVs
    stgCurVal = xs; // xs
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "tail returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t229
DEFUN0(alts_35) {
  fprintf(stderr, "alts_35 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_35 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_35.op->payload[1]; // tl
=======
// List[B] t217
DEFUN0(alts_31) {
  fprintf(stderr, "alts_31 here\n");
  stgPopCont();
  PtrOrLiteral scrut_alts_31 = stgCurVal;
  switch(getInfoPtr(stgCurVal.op)->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_31.op->payload[1]; // tl
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    // x ->
    default: {
<<<<<<< HEAD
      Obj *alts_35_exhaust = stgNewHeapObj( &it_alts_35_exhaust );
      alts_35_exhaust->payload[1] = scrut_alts_35; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_35_exhaust
=======
      Obj *alts_31_exhaust = stgNewHeapObj( &it_alts_31_exhaust );
      alts_31_exhaust->payload[1] = scrut_alts_31; // x
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // alts_31_exhaust
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// forall t230.t230
DEFUN1(fun_alts_35_exhaust, self) {
  fprintf(stderr, "alts_35_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_35_exhaust returning\n");
=======
// forall t218.t218
DEFUN1(fun_alts_31_exhaust, self) {
  fprintf(stderr, "alts_31_exhaust here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL stg_case_not_exhaustive x
  STGAPPLYP(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[1]);
  fprintf(stderr, "alts_31_exhaust returning\n");
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t71,t72.t71 -> t72 -> Tupl2[B] t71 t72
// ((["a","b"],[]),([t71,t72],[]))
=======
// forall t67,t68.t67 -> t68 -> Tupl2[B] t67 t68
// ((["a","b"],[]),([t67,t68],[]))
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// forall t89,t90,t91.t89 -> t90 -> t91 -> Tupl3[B] t89 t90 t91
// ((["a","b","c"],[]),([t89,t90,t91],[]))
=======
// forall t85,t86,t87.t85 -> t86 -> t87 -> Tupl3[B] t85 t86 t87
// ((["a","b","c"],[]),([t85,t86,t87],[]))
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

<<<<<<< HEAD
// forall t327,t330,t445.(t327 -> t330 -> t445) -> List[B] t327 -> List[B] t330 -> List[B] t445
// ((["f","list1","list2"],[]),([t327 -> t330 -> t445,List[B] t327,List[B] t330],[]))
DEFUN4(fun_zipWith, self, f, list1, list2) {
  fprintf(stderr, "zipWith here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_46 = stgAllocCont( &it_alts_46);
      // load payload with FVs f list2
    ccont_alts_46->payload[0] = f; // f
    ccont_alts_46->payload[1] = list2; // list2
=======
// forall t315,t318,t725.(t315 -> t318 -> t725) -> List[B] t315 -> List[B] t318 -> List[B] t725
// ((["f","list1","list2"],[]),([t315 -> t318 -> t725,List[B] t315,List[B] t318],[]))
DEFUN4(fun_zipWith, self, f, list1, list2) {
  fprintf(stderr, "zipWith here\n");
  // scrutinee may heap alloc
  Obj *ccont_alts_42 = stgAllocCont( &it_alts_42);
      // load payload with FVs f list2
    ccont_alts_42->payload[0] = f; // f
    ccont_alts_42->payload[1] = list2; // list2
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
  stgCurVal = list1; // list1
  // boxed EAtom 
  STGEVAL(stgCurVal);
  fprintf(stderr, "zipWith returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t445
DEFUN0(alts_46) {
  fprintf(stderr, "alts_46 here\n");
  Obj *ccont_alts_46 = stgPopCont();
  PtrOrLiteral f = ccont_alts_46->payload[0];
  PtrOrLiteral list2 = ccont_alts_46->payload[1];
  PtrOrLiteral scrut_alts_46 = stgCurVal;
=======
// List[B] t725
DEFUN0(alts_42) {
  fprintf(stderr, "alts_42 here\n");
  Obj *ccont_alts_42 = stgPopCont();
  PtrOrLiteral f = ccont_alts_42->payload[0];
  PtrOrLiteral list2 = ccont_alts_42->payload[1];
  PtrOrLiteral scrut_alts_42 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
      Obj *ccont_alts_47 = stgAllocCont( &it_alts_47);
          // load payload with FVs f h1 t1
        ccont_alts_47->payload[0] = f; // f
        ccont_alts_47->payload[1] = scrut_alts_46.op->payload[0]; // h1
        ccont_alts_47->payload[2] = scrut_alts_46.op->payload[1]; // t1
=======
      Obj *ccont_alts_43 = stgAllocCont( &it_alts_43);
          // load payload with FVs f h1 t1
        ccont_alts_43->payload[0] = f; // f
        ccont_alts_43->payload[1] = scrut_alts_42.op->payload[0]; // h1
        ccont_alts_43->payload[2] = scrut_alts_42.op->payload[1]; // t1
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      stgCurVal = list2; // list2
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// List[B] t445
DEFUN0(alts_47) {
  fprintf(stderr, "alts_47 here\n");
  Obj *ccont_alts_47 = stgPopCont();
  PtrOrLiteral f = ccont_alts_47->payload[0];
  PtrOrLiteral h1 = ccont_alts_47->payload[1];
  PtrOrLiteral t1 = ccont_alts_47->payload[2];
  PtrOrLiteral scrut_alts_47 = stgCurVal;
=======
// List[B] t725
DEFUN0(alts_43) {
  fprintf(stderr, "alts_43 here\n");
  Obj *ccont_alts_43 = stgPopCont();
  PtrOrLiteral f = ccont_alts_43->payload[0];
  PtrOrLiteral h1 = ccont_alts_43->payload[1];
  PtrOrLiteral t1 = ccont_alts_43->payload[2];
  PtrOrLiteral scrut_alts_43 = stgCurVal;
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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
<<<<<<< HEAD
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
=======
      Obj *result_5 = stgNewHeapObj( &it_result_5 );
      newHead_0->payload[1] = f; // f
      newHead_0->payload[2] = h1; // h1
      newHead_0->payload[3] = scrut_alts_43.op->payload[0]; // h2
      newTail_0->payload[1] = f; // f
      newTail_0->payload[2] = t1; // t1
      newTail_0->payload[3] = scrut_alts_43.op->payload[1]; // t2
      result_5->payload[0] = HOTOPL((Obj *)STGHEAPAT(10,3)); // newHead_0
      result_5->payload[1] = HOTOPL((Obj *)STGHEAPAT(6,2)); // newTail_0
      stgCurVal = HOTOPL((Obj *)STGHEAPAT(2,1)); // result_5
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
      // boxed EAtom 
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


<<<<<<< HEAD
// t445
=======
// t725
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_newHead_0, self) {
  fprintf(stderr, "newHead_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL f h1 h2
  STGAPPLYPP(self.op->payload[1], self.op->payload[2], self.op->payload[3]);
  fprintf(stderr, "newHead_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// List[B] t445
=======
// List[B] t725
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
DEFUN1(fun_newTail_0, self) {
  fprintf(stderr, "newTail_0 here\n");
  stgThunk(self);
  // INDIRECT TAIL CALL zipWith f t1 t2
  STGAPPLYPPP(HOTOPL(&sho_zipWith), self.op->payload[1], self.op->payload[2], self.op->payload[3]);
  fprintf(stderr, "newTail_0 returning\n");
  STGRETURN0();
  ENDFUN;
}

<<<<<<< HEAD
// forall t462,t463.List[B] t462 -> List[B] t463 -> List[B] Tupl2[B] t462 t463
=======
// forall t754,t755.List[B] t754 -> List[B] t755 -> List[B] Tupl2[B] t754 t755
>>>>>>> fcedfabb7bb7eca1bf0095bb706c467e49282596
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

