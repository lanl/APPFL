#include "stg_header.h"
void registerSHOs();
FnPtr fun_append();
FnPtr alts_35();
FnPtr fun_rec_4();
FnPtr fun_alts_35_exhaust();
FnPtr fun_apply();
FnPtr fun_const();
FnPtr fun_eqInt();
FnPtr alts_23();
FnPtr alts_24();
FnPtr alts_25();
FnPtr fun_foldl();
FnPtr alts_39();
FnPtr fun_newAcc_1();
FnPtr fun_alts_39_exhaust();
FnPtr fun_seq();
FnPtr alts_42();
FnPtr fun_forcelist();
FnPtr alts_43();
FnPtr fun_rec_6();
FnPtr fun_alts_43_exhaust();
FnPtr fun_head();
FnPtr alts_37();
FnPtr fun_alts_37_exhaust();
FnPtr fun_map();
FnPtr alts_36();
FnPtr fun_rec_5();
FnPtr fun_x_1();
FnPtr fun_alts_36_exhaust();
FnPtr fun_multInt();
FnPtr alts_26();
FnPtr alts_27();
FnPtr alts_28();
FnPtr fun_plusInt();
FnPtr alts_29();
FnPtr alts_30();
FnPtr alts_31();
FnPtr fun_subInt();
FnPtr alts_32();
FnPtr alts_33();
FnPtr alts_34();
FnPtr fun_sum();
FnPtr fun_tail();
FnPtr alts_38();
FnPtr fun_alts_38_exhaust();
FnPtr fun_take();
FnPtr alts_44();
FnPtr alts_45();
FnPtr fun_m_1();
FnPtr fun_rec_7();
FnPtr fun_alts_45_exhaust();
FnPtr fun_alts_44_exhaust();
FnPtr fun_zipWith();
FnPtr alts_40();
FnPtr alts_41();
FnPtr fun_newHead_1();
FnPtr fun_newTail_1();
FnPtr fun_alts_41_exhaust();
FnPtr fun_alts_40_exhaust();
InfoTab it_append = 
  { .name                = "append",
    .fvCount             = 0,
    .entryCode           = &fun_append,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_35 = 
  { .name                = "alts_35",
    .fvCount             = 1,
    .entryCode           = &alts_35,
  };
InfoTab it_rec_4 = 
  { .name                = "rec_4",
    .fvCount             = 2,
    .entryCode           = &fun_rec_4,
    .objType             = THUNK,
  };
InfoTab it_result_9 = 
  { .name                = "result_9",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_alts_35_exhaust = 
  { .name                = "alts_35_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_35_exhaust,
    .objType             = THUNK,
  };
InfoTab it_apply = 
  { .name                = "apply",
    .fvCount             = 0,
    .entryCode           = &fun_apply,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_const = 
  { .name                = "const",
    .fvCount             = 0,
    .entryCode           = &fun_const,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_eight = 
  { .name                = "eight",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_false = 
  { .name                = "false",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "False",
  };
InfoTab it_true = 
  { .name                = "true",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "True",
  };
InfoTab it_eqInt = 
  { .name                = "eqInt",
    .fvCount             = 0,
    .entryCode           = &fun_eqInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_23 = 
  { .name                = "alts_23",
    .fvCount             = 1,
    .entryCode           = &alts_23,
  };
InfoTab it_alts_24 = 
  { .name                = "alts_24",
    .fvCount             = 1,
    .entryCode           = &alts_24,
  };
InfoTab it_alts_25 = 
  { .name                = "alts_25",
    .fvCount             = 0,
    .entryCode           = &alts_25,
  };
InfoTab it_error = 
  { .name                = "error",
    .fvCount             = 0,
    .entryCode           = &stg_error,
    .objType             = BLACKHOLE,
  };
InfoTab it_five = 
  { .name                = "five",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_foldl = 
  { .name                = "foldl",
    .fvCount             = 0,
    .entryCode           = &fun_foldl,
    .objType             = FUN,
    .funFields.arity     = 3,
  };
InfoTab it_alts_39 = 
  { .name                = "alts_39",
    .fvCount             = 2,
    .entryCode           = &alts_39,
  };
InfoTab it_newAcc_1 = 
  { .name                = "newAcc_1",
    .fvCount             = 3,
    .entryCode           = &fun_newAcc_1,
    .objType             = THUNK,
  };
InfoTab it_alts_39_exhaust = 
  { .name                = "alts_39_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_39_exhaust,
    .objType             = THUNK,
  };
InfoTab it_seq = 
  { .name                = "seq",
    .fvCount             = 0,
    .entryCode           = &fun_seq,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_42 = 
  { .name                = "alts_42",
    .fvCount             = 1,
    .entryCode           = &alts_42,
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "Unit",
  };
InfoTab it_forcelist = 
  { .name                = "forcelist",
    .fvCount             = 0,
    .entryCode           = &fun_forcelist,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_43 = 
  { .name                = "alts_43",
    .fvCount             = 0,
    .entryCode           = &alts_43,
  };
InfoTab it_rec_6 = 
  { .name                = "rec_6",
    .fvCount             = 1,
    .entryCode           = &fun_rec_6,
    .objType             = THUNK,
  };
InfoTab it_alts_43_exhaust = 
  { .name                = "alts_43_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_43_exhaust,
    .objType             = THUNK,
  };
InfoTab it_four = 
  { .name                = "four",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_head = 
  { .name                = "head",
    .fvCount             = 0,
    .entryCode           = &fun_head,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_37 = 
  { .name                = "alts_37",
    .fvCount             = 0,
    .entryCode           = &alts_37,
  };
InfoTab it_alts_37_exhaust = 
  { .name                = "alts_37_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_37_exhaust,
    .objType             = THUNK,
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 0,
    .conFields.conName   = "Nil",
  };
InfoTab it_map = 
  { .name                = "map",
    .fvCount             = 0,
    .entryCode           = &fun_map,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_36 = 
  { .name                = "alts_36",
    .fvCount             = 1,
    .entryCode           = &alts_36,
  };
InfoTab it_rec_5 = 
  { .name                = "rec_5",
    .fvCount             = 2,
    .entryCode           = &fun_rec_5,
    .objType             = THUNK,
  };
InfoTab it_x_1 = 
  { .name                = "x_1",
    .fvCount             = 2,
    .entryCode           = &fun_x_1,
    .objType             = THUNK,
  };
InfoTab it_res_1 = 
  { .name                = "res_1",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_alts_36_exhaust = 
  { .name                = "alts_36_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_36_exhaust,
    .objType             = THUNK,
  };
InfoTab it_multInt = 
  { .name                = "multInt",
    .fvCount             = 0,
    .entryCode           = &fun_multInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_26 = 
  { .name                = "alts_26",
    .fvCount             = 1,
    .entryCode           = &alts_26,
  };
InfoTab it_alts_27 = 
  { .name                = "alts_27",
    .fvCount             = 1,
    .entryCode           = &alts_27,
  };
InfoTab it_alts_28 = 
  { .name                = "alts_28",
    .fvCount             = 0,
    .entryCode           = &alts_28,
  };
InfoTab it_result_6 = 
  { .name                = "result_6",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_nine = 
  { .name                = "nine",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_plusInt = 
  { .name                = "plusInt",
    .fvCount             = 0,
    .entryCode           = &fun_plusInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_29 = 
  { .name                = "alts_29",
    .fvCount             = 1,
    .entryCode           = &alts_29,
  };
InfoTab it_alts_30 = 
  { .name                = "alts_30",
    .fvCount             = 1,
    .entryCode           = &alts_30,
  };
InfoTab it_alts_31 = 
  { .name                = "alts_31",
    .fvCount             = 0,
    .entryCode           = &alts_31,
  };
InfoTab it_result_7 = 
  { .name                = "result_7",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_seven = 
  { .name                = "seven",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_six = 
  { .name                = "six",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_subInt = 
  { .name                = "subInt",
    .fvCount             = 0,
    .entryCode           = &fun_subInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_32 = 
  { .name                = "alts_32",
    .fvCount             = 1,
    .entryCode           = &alts_32,
  };
InfoTab it_alts_33 = 
  { .name                = "alts_33",
    .fvCount             = 1,
    .entryCode           = &alts_33,
  };
InfoTab it_alts_34 = 
  { .name                = "alts_34",
    .fvCount             = 0,
    .entryCode           = &alts_34,
  };
InfoTab it_result_8 = 
  { .name                = "result_8",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_zero = 
  { .name                = "zero",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_sum = 
  { .name                = "sum",
    .fvCount             = 0,
    .entryCode           = &fun_sum,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_tail = 
  { .name                = "tail",
    .fvCount             = 0,
    .entryCode           = &fun_tail,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_alts_38 = 
  { .name                = "alts_38",
    .fvCount             = 0,
    .entryCode           = &alts_38,
  };
InfoTab it_alts_38_exhaust = 
  { .name                = "alts_38_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_38_exhaust,
    .objType             = THUNK,
  };
InfoTab it_take = 
  { .name                = "take",
    .fvCount             = 0,
    .entryCode           = &fun_take,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_alts_44 = 
  { .name                = "alts_44",
    .fvCount             = 2,
    .entryCode           = &alts_44,
  };
InfoTab it_alts_45 = 
  { .name                = "alts_45",
    .fvCount             = 1,
    .entryCode           = &alts_45,
  };
InfoTab it_m_1 = 
  { .name                = "m_1",
    .fvCount             = 1,
    .entryCode           = &fun_m_1,
    .objType             = THUNK,
  };
InfoTab it_rec_7 = 
  { .name                = "rec_7",
    .fvCount             = 2,
    .entryCode           = &fun_rec_7,
    .objType             = THUNK,
  };
InfoTab it_result_11 = 
  { .name                = "result_11",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_alts_45_exhaust = 
  { .name                = "alts_45_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_45_exhaust,
    .objType             = THUNK,
  };
InfoTab it_alts_44_exhaust = 
  { .name                = "alts_44_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_44_exhaust,
    .objType             = THUNK,
  };
InfoTab it_ten = 
  { .name                = "ten",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_three = 
  { .name                = "three",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_two = 
  { .name                = "two",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 0,
    .conFields.conName   = "I",
  };
InfoTab it_zipWith = 
  { .name                = "zipWith",
    .fvCount             = 0,
    .entryCode           = &fun_zipWith,
    .objType             = FUN,
    .funFields.arity     = 3,
  };
InfoTab it_alts_40 = 
  { .name                = "alts_40",
    .fvCount             = 2,
    .entryCode           = &alts_40,
  };
InfoTab it_alts_41 = 
  { .name                = "alts_41",
    .fvCount             = 3,
    .entryCode           = &alts_41,
  };
InfoTab it_newHead_1 = 
  { .name                = "newHead_1",
    .fvCount             = 3,
    .entryCode           = &fun_newHead_1,
    .objType             = THUNK,
  };
InfoTab it_newTail_1 = 
  { .name                = "newTail_1",
    .fvCount             = 3,
    .entryCode           = &fun_newTail_1,
    .objType             = THUNK,
  };
InfoTab it_result_10 = 
  { .name                = "result_10",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 1,
    .conFields.conName   = "Cons",
  };
InfoTab it_alts_41_exhaust = 
  { .name                = "alts_41_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_41_exhaust,
    .objType             = THUNK,
  };
InfoTab it_alts_40_exhaust = 
  { .name                = "alts_40_exhaust",
    .fvCount             = 1,
    .entryCode           = &fun_alts_40_exhaust,
    .objType             = THUNK,
  };

extern Obj sho_append;
extern Obj sho_apply;
extern Obj sho_const;
extern Obj sho_eight;
extern Obj sho_false;
extern Obj sho_true;
extern Obj sho_eqInt;
extern Obj sho_error;
extern Obj sho_five;
extern Obj sho_foldl;
extern Obj sho_seq;
extern Obj sho_unit;
extern Obj sho_forcelist;
extern Obj sho_four;
extern Obj sho_head;
extern Obj sho_nil;
extern Obj sho_map;
extern Obj sho_multInt;
extern Obj sho_nine;
extern Obj sho_one;
extern Obj sho_plusInt;
extern Obj sho_seven;
extern Obj sho_six;
extern Obj sho_subInt;
extern Obj sho_zero;
extern Obj sho_sum;
extern Obj sho_tail;
extern Obj sho_take;
extern Obj sho_ten;
extern Obj sho_three;
extern Obj sho_two;
extern Obj sho_zipWith;

Obj sho_append =
{
  .infoPtr   = &it_append,
  .objType   = FUN,
  .ident     = "append",
  };
Obj sho_apply =
{
  .infoPtr   = &it_apply,
  .objType   = FUN,
  .ident     = "apply",
  };
Obj sho_const =
{
  .infoPtr   = &it_const,
  .objType   = FUN,
  .ident     = "const",
  };
Obj sho_eight =
{
  .infoPtr   = &it_eight,
  .objType   = CON,
  .ident     = "eight",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 8,
  };
Obj sho_false =
{
  .infoPtr   = &it_false,
  .objType   = CON,
  .ident     = "false",
  };
Obj sho_true =
{
  .infoPtr   = &it_true,
  .objType   = CON,
  .ident     = "true",
  };
Obj sho_eqInt =
{
  .infoPtr   = &it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  };
Obj sho_error =
{
  .infoPtr   = &it_error,
  .objType   = BLACKHOLE,
  .ident     = "error",
  };
Obj sho_five =
{
  .infoPtr   = &it_five,
  .objType   = CON,
  .ident     = "five",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 5,
  };
Obj sho_foldl =
{
  .infoPtr   = &it_foldl,
  .objType   = FUN,
  .ident     = "foldl",
  };
Obj sho_seq =
{
  .infoPtr   = &it_seq,
  .objType   = FUN,
  .ident     = "seq",
  };
Obj sho_unit =
{
  .infoPtr   = &it_unit,
  .objType   = CON,
  .ident     = "unit",
  };
Obj sho_forcelist =
{
  .infoPtr   = &it_forcelist,
  .objType   = FUN,
  .ident     = "forcelist",
  };
Obj sho_four =
{
  .infoPtr   = &it_four,
  .objType   = CON,
  .ident     = "four",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 4,
  };
Obj sho_head =
{
  .infoPtr   = &it_head,
  .objType   = FUN,
  .ident     = "head",
  };
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
  };
Obj sho_map =
{
  .infoPtr   = &it_map,
  .objType   = FUN,
  .ident     = "map",
  };
Obj sho_multInt =
{
  .infoPtr   = &it_multInt,
  .objType   = FUN,
  .ident     = "multInt",
  };
Obj sho_nine =
{
  .infoPtr   = &it_nine,
  .objType   = CON,
  .ident     = "nine",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 9,
  };
Obj sho_one =
{
  .infoPtr   = &it_one,
  .objType   = CON,
  .ident     = "one",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 1,
  };
Obj sho_plusInt =
{
  .infoPtr   = &it_plusInt,
  .objType   = FUN,
  .ident     = "plusInt",
  };
Obj sho_seven =
{
  .infoPtr   = &it_seven,
  .objType   = CON,
  .ident     = "seven",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 7,
  };
Obj sho_six =
{
  .infoPtr   = &it_six,
  .objType   = CON,
  .ident     = "six",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 6,
  };
Obj sho_subInt =
{
  .infoPtr   = &it_subInt,
  .objType   = FUN,
  .ident     = "subInt",
  };
Obj sho_zero =
{
  .infoPtr   = &it_zero,
  .objType   = CON,
  .ident     = "zero",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 0,
  };
Obj sho_sum =
{
  .infoPtr   = &it_sum,
  .objType   = FUN,
  .ident     = "sum",
  };
Obj sho_tail =
{
  .infoPtr   = &it_tail,
  .objType   = FUN,
  .ident     = "tail",
  };
Obj sho_take =
{
  .infoPtr   = &it_take,
  .objType   = FUN,
  .ident     = "take",
  };
Obj sho_ten =
{
  .infoPtr   = &it_ten,
  .objType   = CON,
  .ident     = "ten",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 10,
  };
Obj sho_three =
{
  .infoPtr   = &it_three,
  .objType   = CON,
  .ident     = "three",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 3,
  };
Obj sho_two =
{
  .infoPtr   = &it_two,
  .objType   = CON,
  .ident     = "two",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 2,
  };
Obj sho_zipWith =
{
  .infoPtr   = &it_zipWith,
  .objType   = FUN,
  .ident     = "zipWith",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_append;
  stgStatObj[stgStatObjCount++] = &sho_apply;
  stgStatObj[stgStatObjCount++] = &sho_const;
  stgStatObj[stgStatObjCount++] = &sho_eight;
  stgStatObj[stgStatObjCount++] = &sho_false;
  stgStatObj[stgStatObjCount++] = &sho_true;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_five;
  stgStatObj[stgStatObjCount++] = &sho_foldl;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
  stgStatObj[stgStatObjCount++] = &sho_four;
  stgStatObj[stgStatObjCount++] = &sho_head;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_map;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_nine;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_plusInt;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_six;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_sum;
  stgStatObj[stgStatObjCount++] = &sho_tail;
  stgStatObj[stgStatObjCount++] = &sho_take;
  stgStatObj[stgStatObjCount++] = &sho_ten;
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_zipWith;
}


DEFUN3(fun_append, self, l1, l2) {
  fprintf(stderr, "append here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_35,
      .objType = CASECONT,
      .ident = "CCont for alts_35",
      // load payload with FVs l2
      .payload[0] = l2, // l2
    });
  stgCurVal = l1; // l1
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_35) {
  fprintf(stderr, "alts_35 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_35 = stgPopCont();
  PtrOrLiteral scrut_alts_35 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = ccont_alts_35.payload[0]; // l2
      STGRETURN0();
    }
    // Cons hd tl ->
    case 1: {
      Obj *rec_4 = stgNewHeapObj();
      Obj *result_9 = stgNewHeapObj();
      *rec_4 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_4,
              .ident = "rec_4",
              .payload[0] = ccont_alts_35.payload[0], // l2
              .payload[1] = scrut_alts_35.op->payload[1], // tl
            };
      *result_9 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_9,
              .ident = "result_9",
              .payload[0] = scrut_alts_35.op->payload[0], // hd
              .payload[1] = HOTOPL(STGHEAPAT(-2)), // rec_4
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_9
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_35_exhaust = stgNewHeapObj();
      *alts_35_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_35_exhaust,
              .ident = "alts_35_exhaust",
              .payload[0] = scrut_alts_35, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_35_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_4, self) {
  fprintf(stderr, "rec_4 here\n");
  stgThunk(self);
  // append tl l2
  STGAPPLY2(HOTOPL(&sho_append), self.op->payload[1], self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_35_exhaust, self) {
  fprintf(stderr, "alts_35_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  // f x
  STGAPPLY1(f, x);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_const, self, x, y) {
  fprintf(stderr, "const here\n");
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_23,
      .objType = CASECONT,
      .ident = "CCont for alts_23",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_23) {
  fprintf(stderr, "alts_23 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_23 = stgPopCont();
  PtrOrLiteral scrut_alts_23 = stgCurVal;
  // I i# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_24,
      .objType = CASECONT,
      .ident = "CCont for alts_24",
      // load payload with FVs i#
      .payload[0] = scrut_alts_23.op->payload[0], // i#
    });
  stgCurVal = ccont_alts_23.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_24) {
  fprintf(stderr, "alts_24 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_24 = stgPopCont();
  PtrOrLiteral scrut_alts_24 = stgCurVal;
  // I j# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_25,
      .objType = CASECONT,
      .ident = "CCont for alts_25",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_24.payload[0]).i == (scrut_alts_24.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_25) {
  fprintf(stderr, "alts_25 here\n");
  // unboxed scrutinee
  stgPopCont();
  PtrOrLiteral scrut_alts_25 = stgCurVal;
  switch(stgCurVal.i) {
    // 0  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_false); // false
      STGRETURN0();
    }
    // x ->
    default: {
      stgCurVal = HOTOPL(&sho_true); // true
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN4(fun_foldl, self, f, acc, list) {
  fprintf(stderr, "foldl here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_39,
      .objType = CASECONT,
      .ident = "CCont for alts_39",
      // load payload with FVs acc f
      .payload[0] = acc, // acc
      .payload[1] = f, // f
    });
  stgCurVal = list; // list
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_39) {
  fprintf(stderr, "alts_39 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_39 = stgPopCont();
  PtrOrLiteral scrut_alts_39 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = ccont_alts_39.payload[0]; // acc
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *newAcc_1 = stgNewHeapObj();
      *newAcc_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_newAcc_1,
              .ident = "newAcc_1",
              .payload[0] = ccont_alts_39.payload[0], // acc
              .payload[1] = ccont_alts_39.payload[1], // f
              .payload[2] = scrut_alts_39.op->payload[0], // h
            };
      // foldl f newAcc_1 t
      STGAPPLY3(HOTOPL(&sho_foldl), ccont_alts_39.payload[1], HOTOPL(STGHEAPAT(-1)), scrut_alts_39.op->payload[1]);
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_39_exhaust = stgNewHeapObj();
      *alts_39_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_39_exhaust,
              .ident = "alts_39_exhaust",
              .payload[0] = scrut_alts_39, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_39_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_newAcc_1, self) {
  fprintf(stderr, "newAcc_1 here\n");
  stgThunk(self);
  // f acc h
  STGAPPLY2(self.op->payload[1], self.op->payload[0], self.op->payload[2]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_39_exhaust, self) {
  fprintf(stderr, "alts_39_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_42,
      .objType = CASECONT,
      .ident = "CCont for alts_42",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_42) {
  fprintf(stderr, "alts_42 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_42 = stgPopCont();
  PtrOrLiteral scrut_alts_42 = stgCurVal;
  // z ->
  stgCurVal = ccont_alts_42.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_43,
      .objType = CASECONT,
      .ident = "CCont for alts_43",
      // no FVs
        });
  stgCurVal = list; // list
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_43) {
  fprintf(stderr, "alts_43 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_43 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_unit); // unit
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *rec_6 = stgNewHeapObj();
      *rec_6 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_6,
              .ident = "rec_6",
              .payload[0] = scrut_alts_43.op->payload[1], // t
            };
      // seq h rec_6
      STGAPPLY2(HOTOPL(&sho_seq), scrut_alts_43.op->payload[0], HOTOPL(STGHEAPAT(-1)));
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_43_exhaust = stgNewHeapObj();
      *alts_43_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_43_exhaust,
              .ident = "alts_43_exhaust",
              .payload[0] = scrut_alts_43, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_43_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_6, self) {
  fprintf(stderr, "rec_6 here\n");
  stgThunk(self);
  // forcelist t
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_43_exhaust, self) {
  fprintf(stderr, "alts_43_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_37,
      .objType = CASECONT,
      .ident = "CCont for alts_37",
      // no FVs
        });
  stgCurVal = xs; // xs
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_37) {
  fprintf(stderr, "alts_37 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_37 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_37.op->payload[0]; // hd
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_37_exhaust = stgNewHeapObj();
      *alts_37_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_37_exhaust,
              .ident = "alts_37_exhaust",
              .payload[0] = scrut_alts_37, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_37_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_alts_37_exhaust, self) {
  fprintf(stderr, "alts_37_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_map, self, f, list) {
  fprintf(stderr, "map here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_36,
      .objType = CASECONT,
      .ident = "CCont for alts_36",
      // load payload with FVs f
      .payload[0] = f, // f
    });
  stgCurVal = list; // list
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_36) {
  fprintf(stderr, "alts_36 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_36 = stgPopCont();
  PtrOrLiteral scrut_alts_36 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // Cons h t ->
    case 1: {
      Obj *rec_5 = stgNewHeapObj();
      Obj *x_1 = stgNewHeapObj();
      Obj *res_1 = stgNewHeapObj();
      *rec_5 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_5,
              .ident = "rec_5",
              .payload[0] = ccont_alts_36.payload[0], // f
              .payload[1] = scrut_alts_36.op->payload[1], // t
            };
      *x_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_x_1,
              .ident = "x_1",
              .payload[0] = ccont_alts_36.payload[0], // f
              .payload[1] = scrut_alts_36.op->payload[0], // h
            };
      *res_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_res_1,
              .ident = "res_1",
              .payload[0] = HOTOPL(STGHEAPAT(-2)), // x_1
              .payload[1] = HOTOPL(STGHEAPAT(-3)), // rec_5
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // res_1
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_36_exhaust = stgNewHeapObj();
      *alts_36_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_36_exhaust,
              .ident = "alts_36_exhaust",
              .payload[0] = scrut_alts_36, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_36_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_rec_5, self) {
  fprintf(stderr, "rec_5 here\n");
  stgThunk(self);
  // map f t
  STGAPPLY2(HOTOPL(&sho_map), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_x_1, self) {
  fprintf(stderr, "x_1 here\n");
  stgThunk(self);
  // f h
  STGAPPLY1(self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_36_exhaust, self) {
  fprintf(stderr, "alts_36_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_26,
      .objType = CASECONT,
      .ident = "CCont for alts_26",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_26) {
  fprintf(stderr, "alts_26 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_26 = stgPopCont();
  PtrOrLiteral scrut_alts_26 = stgCurVal;
  // I i# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_27,
      .objType = CASECONT,
      .ident = "CCont for alts_27",
      // load payload with FVs i#
      .payload[0] = scrut_alts_26.op->payload[0], // i#
    });
  stgCurVal = ccont_alts_26.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_27) {
  fprintf(stderr, "alts_27 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_27 = stgPopCont();
  PtrOrLiteral scrut_alts_27 = stgCurVal;
  // I j# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_28,
      .objType = CASECONT,
      .ident = "CCont for alts_28",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_27.payload[0]).i * (scrut_alts_27.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_28) {
  fprintf(stderr, "alts_28 here\n");
  // unboxed scrutinee
  stgPopCont();
  PtrOrLiteral scrut_alts_28 = stgCurVal;
  // x# ->
  Obj *result_6 = stgNewHeapObj();
  *result_6 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_result_6,
          .ident = "result_6",
          .payload[0] = scrut_alts_28, // x#
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_6
  STGRETURN0();
  ENDFUN;
}


DEFUN3(fun_plusInt, self, x, y) {
  fprintf(stderr, "plusInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_29,
      .objType = CASECONT,
      .ident = "CCont for alts_29",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_29) {
  fprintf(stderr, "alts_29 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_29 = stgPopCont();
  PtrOrLiteral scrut_alts_29 = stgCurVal;
  // I i# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_30,
      .objType = CASECONT,
      .ident = "CCont for alts_30",
      // load payload with FVs i#
      .payload[0] = scrut_alts_29.op->payload[0], // i#
    });
  stgCurVal = ccont_alts_29.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_30) {
  fprintf(stderr, "alts_30 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_30 = stgPopCont();
  PtrOrLiteral scrut_alts_30 = stgCurVal;
  // I j# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_31,
      .objType = CASECONT,
      .ident = "CCont for alts_31",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_30.payload[0]).i + (scrut_alts_30.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_31) {
  fprintf(stderr, "alts_31 here\n");
  // unboxed scrutinee
  stgPopCont();
  PtrOrLiteral scrut_alts_31 = stgCurVal;
  // x# ->
  Obj *result_7 = stgNewHeapObj();
  *result_7 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_result_7,
          .ident = "result_7",
          .payload[0] = scrut_alts_31, // x#
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_7
  STGRETURN0();
  ENDFUN;
}


DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_32,
      .objType = CASECONT,
      .ident = "CCont for alts_32",
      // load payload with FVs y
      .payload[0] = y, // y
    });
  stgCurVal = x; // x
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_32) {
  fprintf(stderr, "alts_32 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_32 = stgPopCont();
  PtrOrLiteral scrut_alts_32 = stgCurVal;
  // I i# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_33,
      .objType = CASECONT,
      .ident = "CCont for alts_33",
      // load payload with FVs i#
      .payload[0] = scrut_alts_32.op->payload[0], // i#
    });
  stgCurVal = ccont_alts_32.payload[0]; // y
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_33) {
  fprintf(stderr, "alts_33 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_33 = stgPopCont();
  PtrOrLiteral scrut_alts_33 = stgCurVal;
  // I j# ->
  stgPushCont( (Cont)
    { .retAddr = &alts_34,
      .objType = CASECONT,
      .ident = "CCont for alts_34",
      // no FVs
        });
  stgCurVal.argType = INT;
  stgCurVal.i = (ccont_alts_33.payload[0]).i - (scrut_alts_33.op->payload[0]).i;
  STGRETURN0();
  ENDFUN;
}


DEFUN0(alts_34) {
  fprintf(stderr, "alts_34 here\n");
  // unboxed scrutinee
  stgPopCont();
  PtrOrLiteral scrut_alts_34 = stgCurVal;
  // x# ->
  Obj *result_8 = stgNewHeapObj();
  *result_8 = (Obj) 
        { .objType = CON,
          .infoPtr = &it_result_8,
          .ident = "result_8",
          .payload[0] = scrut_alts_34, // x#
        };
  stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_8
  STGRETURN0();
  ENDFUN;
}


DEFUN2(fun_sum, self, list) {
  fprintf(stderr, "sum here\n");
  // foldl plusInt zero list
  STGAPPLY3(HOTOPL(&sho_foldl), HOTOPL(&sho_plusInt), HOTOPL(&sho_zero), list);
  STGRETURN0();
  ENDFUN;
}

DEFUN2(fun_tail, self, xs) {
  fprintf(stderr, "tail here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_38,
      .objType = CASECONT,
      .ident = "CCont for alts_38",
      // no FVs
        });
  stgCurVal = xs; // xs
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_38) {
  fprintf(stderr, "alts_38 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  stgPopCont();
  PtrOrLiteral scrut_alts_38 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Cons hd tl ->
    case 1: {
      stgCurVal = scrut_alts_38.op->payload[1]; // tl
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_38_exhaust = stgNewHeapObj();
      *alts_38_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_38_exhaust,
              .ident = "alts_38_exhaust",
              .payload[0] = scrut_alts_38, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_38_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_alts_38_exhaust, self) {
  fprintf(stderr, "alts_38_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_44,
      .objType = CASECONT,
      .ident = "CCont for alts_44",
      // load payload with FVs n xs
      .payload[0] = n, // n
      .payload[1] = xs, // xs
    });
  // eqInt n zero
  STGAPPLY2(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_44) {
  fprintf(stderr, "alts_44 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_44 = stgPopCont();
  PtrOrLiteral scrut_alts_44 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // True  ->
    case 1: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // False  ->
    case 0: {
      stgPushCont( (Cont)
        { .retAddr = &alts_45,
          .objType = CASECONT,
          .ident = "CCont for alts_45",
          // load payload with FVs n
          .payload[0] = ccont_alts_44.payload[0], // n
        });
      stgCurVal = ccont_alts_44.payload[1]; // xs
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_44_exhaust = stgNewHeapObj();
      *alts_44_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_44_exhaust,
              .ident = "alts_44_exhaust",
              .payload[0] = scrut_alts_44, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_44_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_45) {
  fprintf(stderr, "alts_45 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_45 = stgPopCont();
  PtrOrLiteral scrut_alts_45 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // Cons hd tl ->
    case 1: {
      Obj *m_1 = stgNewHeapObj();
      Obj *rec_7 = stgNewHeapObj();
      Obj *result_11 = stgNewHeapObj();
      *m_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_m_1,
              .ident = "m_1",
              .payload[0] = ccont_alts_45.payload[0], // n
            };
      *rec_7 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_7,
              .ident = "rec_7",
              .payload[0] = HOTOPL(STGHEAPAT(-3)), // m_1
              .payload[1] = scrut_alts_45.op->payload[1], // tl
            };
      *result_11 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_11,
              .ident = "result_11",
              .payload[0] = scrut_alts_45.op->payload[0], // hd
              .payload[1] = HOTOPL(STGHEAPAT(-2)), // rec_7
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_11
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_45_exhaust = stgNewHeapObj();
      *alts_45_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_45_exhaust,
              .ident = "alts_45_exhaust",
              .payload[0] = scrut_alts_45, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_45_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_m_1, self) {
  fprintf(stderr, "m_1 here\n");
  stgThunk(self);
  // subInt n one
  STGAPPLY2(HOTOPL(&sho_subInt), self.op->payload[0], HOTOPL(&sho_one));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_rec_7, self) {
  fprintf(stderr, "rec_7 here\n");
  stgThunk(self);
  // take m_1 tl
  STGAPPLY2(HOTOPL(&sho_take), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_45_exhaust, self) {
  fprintf(stderr, "alts_45_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_44_exhaust, self) {
  fprintf(stderr, "alts_44_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN4(fun_zipWith, self, f, list1, list2) {
  fprintf(stderr, "zipWith here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_40,
      .objType = CASECONT,
      .ident = "CCont for alts_40",
      // load payload with FVs f list2
      .payload[0] = f, // f
      .payload[1] = list2, // list2
    });
  stgCurVal = list1; // list1
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_40) {
  fprintf(stderr, "alts_40 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_40 = stgPopCont();
  PtrOrLiteral scrut_alts_40 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // Cons h1 t1 ->
    case 1: {
      stgPushCont( (Cont)
        { .retAddr = &alts_41,
          .objType = CASECONT,
          .ident = "CCont for alts_41",
          // load payload with FVs f h1 t1
          .payload[0] = ccont_alts_40.payload[0], // f
          .payload[1] = scrut_alts_40.op->payload[0], // h1
          .payload[2] = scrut_alts_40.op->payload[1], // t1
        });
      stgCurVal = ccont_alts_40.payload[1]; // list2
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_40_exhaust = stgNewHeapObj();
      *alts_40_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_40_exhaust,
              .ident = "alts_40_exhaust",
              .payload[0] = scrut_alts_40, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_40_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_41) {
  fprintf(stderr, "alts_41 here\n");
  // boxed scrutinee
  STGEVAL(stgCurVal);
  Cont ccont_alts_41 = stgPopCont();
  PtrOrLiteral scrut_alts_41 = stgCurVal;
  switch(stgCurVal.op->infoPtr->conFields.tag) {
    // Nil  ->
    case 0: {
      stgCurVal = HOTOPL(&sho_nil); // nil
      STGRETURN0();
    }
    // Cons h2 t2 ->
    case 1: {
      Obj *newHead_1 = stgNewHeapObj();
      Obj *newTail_1 = stgNewHeapObj();
      Obj *result_10 = stgNewHeapObj();
      *newHead_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_newHead_1,
              .ident = "newHead_1",
              .payload[0] = ccont_alts_41.payload[0], // f
              .payload[1] = ccont_alts_41.payload[1], // h1
              .payload[2] = scrut_alts_41.op->payload[0], // h2
            };
      *newTail_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_newTail_1,
              .ident = "newTail_1",
              .payload[0] = ccont_alts_41.payload[0], // f
              .payload[1] = ccont_alts_41.payload[2], // t1
              .payload[2] = scrut_alts_41.op->payload[1], // t2
            };
      *result_10 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_10,
              .ident = "result_10",
              .payload[0] = HOTOPL(STGHEAPAT(-3)), // newHead_1
              .payload[1] = HOTOPL(STGHEAPAT(-2)), // newTail_1
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // result_10
      STGRETURN0();
    }
    // x ->
    default: {
      Obj *alts_41_exhaust = stgNewHeapObj();
      *alts_41_exhaust = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_alts_41_exhaust,
              .ident = "alts_41_exhaust",
              .payload[0] = scrut_alts_41, // x
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1)); // alts_41_exhaust
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(fun_newHead_1, self) {
  fprintf(stderr, "newHead_1 here\n");
  stgThunk(self);
  // f h1 h2
  STGAPPLY2(self.op->payload[0], self.op->payload[1], self.op->payload[2]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_newTail_1, self) {
  fprintf(stderr, "newTail_1 here\n");
  stgThunk(self);
  // zipWith f t1 t2
  STGAPPLY3(HOTOPL(&sho_zipWith), self.op->payload[0], self.op->payload[1], self.op->payload[2]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_41_exhaust, self) {
  fprintf(stderr, "alts_41_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(fun_alts_40_exhaust, self) {
  fprintf(stderr, "alts_40_exhaust here\n");
  stgThunk(self);
  // stg_case_not_exhaustive x
  STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), self.op->payload[0]);
  STGRETURN0();
  ENDFUN;
}
DEFUN0(start) {
  registerSHOs();
  stgPushCont(showResultCont);
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

