#include "stg_header.h"
void registerSHOs();
FnPtr tnk_main();
FnPtr tnk_main1();
FnPtr tnk_list4();
FnPtr tnk_list5();
FnPtr tnk_mainappend();
FnPtr tnk_f_0();
FnPtr fun_multInt();
FnPtr alts_0();
FnPtr alts_1();
FnPtr alts_2();
FnPtr fun_plusInt();
FnPtr alts_3();
FnPtr alts_4();
FnPtr alts_5();
FnPtr fun_subInt();
FnPtr alts_6();
FnPtr alts_7();
FnPtr alts_8();
FnPtr fun_eqInt();
FnPtr alts_9();
FnPtr alts_10();
FnPtr alts_11();
FnPtr fun_append();
FnPtr alts_12();
FnPtr tnk_rec_0();
FnPtr fun_constf();
FnPtr fun_apply();
FnPtr fun_map();
FnPtr alts_13();
FnPtr tnk_x_0();
FnPtr tnk_rec_1();
FnPtr fun_take();
FnPtr alts_14();
FnPtr alts_15();
FnPtr tnk_m_0();
FnPtr tnk_rec_2();
FnPtr fun_head();
FnPtr alts_16();
FnPtr fun_tail();
FnPtr alts_17();
FnPtr fun_foldl();
FnPtr alts_18();
FnPtr tnk_newAcc_0();
FnPtr fun_sum();
FnPtr fun_zipWith();
FnPtr alts_19();
FnPtr alts_20();
FnPtr tnk_newHead_0();
FnPtr tnk_newTail_0();
FnPtr fun_seq();
FnPtr alts_21();
FnPtr fun_forcelist();
FnPtr alts_22();
FnPtr tnk_rec_3();
InfoTab it_main = 
  { .name                = "main",
    .fvCount             = 0,
    .entryCode           = &tnk_main,
    .objType             = THUNK,
  };
InfoTab it_main1 = 
  { .name                = "main1",
    .fvCount             = 0,
    .entryCode           = &tnk_main1,
    .objType             = THUNK,
  };
InfoTab it_list1 = 
  { .name                = "list1",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_list2 = 
  { .name                = "list2",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_list3 = 
  { .name                = "list3",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_list4 = 
  { .name                = "list4",
    .fvCount             = 0,
    .entryCode           = &tnk_list4,
    .objType             = THUNK,
  };
InfoTab it_list5 = 
  { .name                = "list5",
    .fvCount             = 0,
    .entryCode           = &tnk_list5,
    .objType             = THUNK,
  };
InfoTab it_mainappend = 
  { .name                = "mainappend",
    .fvCount             = 0,
    .entryCode           = &tnk_mainappend,
    .objType             = THUNK,
  };
InfoTab it_f_0 = 
  { .name                = "f_0",
    .fvCount             = 0,
    .entryCode           = &tnk_f_0,
    .objType             = THUNK,
  };
InfoTab it_error = 
  { .name                = "error",
    .fvCount             = 0,
    .entryCode           = &bhl_error,
    .objType             = BLACKHOLE,
  };
InfoTab it_unit = 
  { .name                = "unit",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 1,
    .conFields.conName   = "Unit",
  };
InfoTab it_nil = 
  { .name                = "nil",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 0,
    .conFields.tag       = 2,
    .conFields.conName   = "Nil",
  };
InfoTab it_zero = 
  { .name                = "zero",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_one = 
  { .name                = "one",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_two = 
  { .name                = "two",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_three = 
  { .name                = "three",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_four = 
  { .name                = "four",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_five = 
  { .name                = "five",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_six = 
  { .name                = "six",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_seven = 
  { .name                = "seven",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_eight = 
  { .name                = "eight",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_nine = 
  { .name                = "nine",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_ten = 
  { .name                = "ten",
    .fvCount             = 0,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_multInt = 
  { .name                = "multInt",
    .fvCount             = 0,
    .entryCode           = &fun_multInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_0 = 
  { .name                = "result_0",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_plusInt = 
  { .name                = "plusInt",
    .fvCount             = 0,
    .entryCode           = &fun_plusInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_1 = 
  { .name                = "result_1",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_subInt = 
  { .name                = "subInt",
    .fvCount             = 0,
    .entryCode           = &fun_subInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_result_2 = 
  { .name                = "result_2",
    .fvCount             = 1,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 1,
    .conFields.tag       = 3,
    .conFields.conName   = "I",
  };
InfoTab it_eqInt = 
  { .name                = "eqInt",
    .fvCount             = 0,
    .entryCode           = &fun_eqInt,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_append = 
  { .name                = "append",
    .fvCount             = 0,
    .entryCode           = &fun_append,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_rec_0 = 
  { .name                = "rec_0",
    .fvCount             = 2,
    .entryCode           = &tnk_rec_0,
    .objType             = THUNK,
  };
InfoTab it_result_3 = 
  { .name                = "result_3",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_constf = 
  { .name                = "constf",
    .fvCount             = 0,
    .entryCode           = &fun_constf,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_apply = 
  { .name                = "apply",
    .fvCount             = 0,
    .entryCode           = &fun_apply,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_map = 
  { .name                = "map",
    .fvCount             = 0,
    .entryCode           = &fun_map,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_x_0 = 
  { .name                = "x_0",
    .fvCount             = 2,
    .entryCode           = &tnk_x_0,
    .objType             = THUNK,
  };
InfoTab it_rec_1 = 
  { .name                = "rec_1",
    .fvCount             = 2,
    .entryCode           = &tnk_rec_1,
    .objType             = THUNK,
  };
InfoTab it_res_0 = 
  { .name                = "res_0",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_take = 
  { .name                = "take",
    .fvCount             = 0,
    .entryCode           = &fun_take,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_m_0 = 
  { .name                = "m_0",
    .fvCount             = 1,
    .entryCode           = &tnk_m_0,
    .objType             = THUNK,
  };
InfoTab it_rec_2 = 
  { .name                = "rec_2",
    .fvCount             = 2,
    .entryCode           = &tnk_rec_2,
    .objType             = THUNK,
  };
InfoTab it_result_4 = 
  { .name                = "result_4",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_head = 
  { .name                = "head",
    .fvCount             = 0,
    .entryCode           = &fun_head,
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
InfoTab it_foldl = 
  { .name                = "foldl",
    .fvCount             = 0,
    .entryCode           = &fun_foldl,
    .objType             = FUN,
    .funFields.arity     = 3,
  };
InfoTab it_newAcc_0 = 
  { .name                = "newAcc_0",
    .fvCount             = 3,
    .entryCode           = &tnk_newAcc_0,
    .objType             = THUNK,
  };
InfoTab it_sum = 
  { .name                = "sum",
    .fvCount             = 0,
    .entryCode           = &fun_sum,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_zipWith = 
  { .name                = "zipWith",
    .fvCount             = 0,
    .entryCode           = &fun_zipWith,
    .objType             = FUN,
    .funFields.arity     = 3,
  };
InfoTab it_newHead_0 = 
  { .name                = "newHead_0",
    .fvCount             = 3,
    .entryCode           = &tnk_newHead_0,
    .objType             = THUNK,
  };
InfoTab it_newTail_0 = 
  { .name                = "newTail_0",
    .fvCount             = 3,
    .entryCode           = &tnk_newTail_0,
    .objType             = THUNK,
  };
InfoTab it_result_5 = 
  { .name                = "result_5",
    .fvCount             = 2,
    .entryCode           = &stg_constructorcall,
    .objType             = CON,
    .conFields.arity     = 2,
    .conFields.tag       = 0,
    .conFields.conName   = "Cons",
  };
InfoTab it_seq = 
  { .name                = "seq",
    .fvCount             = 0,
    .entryCode           = &fun_seq,
    .objType             = FUN,
    .funFields.arity     = 2,
  };
InfoTab it_forcelist = 
  { .name                = "forcelist",
    .fvCount             = 0,
    .entryCode           = &fun_forcelist,
    .objType             = FUN,
    .funFields.arity     = 1,
  };
InfoTab it_rec_3 = 
  { .name                = "rec_3",
    .fvCount             = 1,
    .entryCode           = &tnk_rec_3,
    .objType             = THUNK,
  };

extern Obj sho_main;
extern Obj sho_main1;
extern Obj sho_list1;
extern Obj sho_list2;
extern Obj sho_list3;
extern Obj sho_list4;
extern Obj sho_list5;
extern Obj sho_mainappend;
extern Obj sho_error;
extern Obj sho_unit;
extern Obj sho_nil;
extern Obj sho_zero;
extern Obj sho_one;
extern Obj sho_two;
extern Obj sho_three;
extern Obj sho_four;
extern Obj sho_five;
extern Obj sho_six;
extern Obj sho_seven;
extern Obj sho_eight;
extern Obj sho_nine;
extern Obj sho_ten;
extern Obj sho_multInt;
extern Obj sho_plusInt;
extern Obj sho_subInt;
extern Obj sho_eqInt;
extern Obj sho_append;
extern Obj sho_constf;
extern Obj sho_apply;
extern Obj sho_map;
extern Obj sho_take;
extern Obj sho_head;
extern Obj sho_tail;
extern Obj sho_foldl;
extern Obj sho_sum;
extern Obj sho_zipWith;
extern Obj sho_seq;
extern Obj sho_forcelist;

Obj sho_main =
{
  .infoPtr   = &it_main,
  .objType   = THUNK,
  .ident     = "main",
  };
Obj sho_main1 =
{
  .infoPtr   = &it_main1,
  .objType   = THUNK,
  .ident     = "main1",
  };
Obj sho_list1 =
{
  .infoPtr   = &it_list1,
  .objType   = CON,
  .ident     = "list1",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_one,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_nil,
  };
Obj sho_list2 =
{
  .infoPtr   = &it_list2,
  .objType   = CON,
  .ident     = "list2",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_one,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_list1,
  };
Obj sho_list3 =
{
  .infoPtr   = &it_list3,
  .objType   = CON,
  .ident     = "list3",
    .payload[ 0 ].argType = HEAPOBJ,
    .payload[ 0 ].op = &sho_zero,
    .payload[ 1 ].argType = HEAPOBJ,
    .payload[ 1 ].op = &sho_list2,
  };
Obj sho_list4 =
{
  .infoPtr   = &it_list4,
  .objType   = THUNK,
  .ident     = "list4",
  };
Obj sho_list5 =
{
  .infoPtr   = &it_list5,
  .objType   = THUNK,
  .ident     = "list5",
  };
Obj sho_mainappend =
{
  .infoPtr   = &it_mainappend,
  .objType   = THUNK,
  .ident     = "mainappend",
  };
Obj sho_error =
{
  .infoPtr   = &it_error,
  .objType   = BLACKHOLE,
  .ident     = "error",
  };
Obj sho_unit =
{
  .infoPtr   = &it_unit,
  .objType   = CON,
  .ident     = "unit",
  };
Obj sho_nil =
{
  .infoPtr   = &it_nil,
  .objType   = CON,
  .ident     = "nil",
  };
Obj sho_zero =
{
  .infoPtr   = &it_zero,
  .objType   = CON,
  .ident     = "zero",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 0,
  };
Obj sho_one =
{
  .infoPtr   = &it_one,
  .objType   = CON,
  .ident     = "one",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 1,
  };
Obj sho_two =
{
  .infoPtr   = &it_two,
  .objType   = CON,
  .ident     = "two",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 2,
  };
Obj sho_three =
{
  .infoPtr   = &it_three,
  .objType   = CON,
  .ident     = "three",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 3,
  };
Obj sho_four =
{
  .infoPtr   = &it_four,
  .objType   = CON,
  .ident     = "four",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 4,
  };
Obj sho_five =
{
  .infoPtr   = &it_five,
  .objType   = CON,
  .ident     = "five",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 5,
  };
Obj sho_six =
{
  .infoPtr   = &it_six,
  .objType   = CON,
  .ident     = "six",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 6,
  };
Obj sho_seven =
{
  .infoPtr   = &it_seven,
  .objType   = CON,
  .ident     = "seven",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 7,
  };
Obj sho_eight =
{
  .infoPtr   = &it_eight,
  .objType   = CON,
  .ident     = "eight",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 8,
  };
Obj sho_nine =
{
  .infoPtr   = &it_nine,
  .objType   = CON,
  .ident     = "nine",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 9,
  };
Obj sho_ten =
{
  .infoPtr   = &it_ten,
  .objType   = CON,
  .ident     = "ten",
    .payload[ 0 ].argType = INT,
    .payload[ 0 ].i = 10,
  };
Obj sho_multInt =
{
  .infoPtr   = &it_multInt,
  .objType   = FUN,
  .ident     = "multInt",
  };
Obj sho_plusInt =
{
  .infoPtr   = &it_plusInt,
  .objType   = FUN,
  .ident     = "plusInt",
  };
Obj sho_subInt =
{
  .infoPtr   = &it_subInt,
  .objType   = FUN,
  .ident     = "subInt",
  };
Obj sho_eqInt =
{
  .infoPtr   = &it_eqInt,
  .objType   = FUN,
  .ident     = "eqInt",
  };
Obj sho_append =
{
  .infoPtr   = &it_append,
  .objType   = FUN,
  .ident     = "append",
  };
Obj sho_constf =
{
  .infoPtr   = &it_constf,
  .objType   = FUN,
  .ident     = "constf",
  };
Obj sho_apply =
{
  .infoPtr   = &it_apply,
  .objType   = FUN,
  .ident     = "apply",
  };
Obj sho_map =
{
  .infoPtr   = &it_map,
  .objType   = FUN,
  .ident     = "map",
  };
Obj sho_take =
{
  .infoPtr   = &it_take,
  .objType   = FUN,
  .ident     = "take",
  };
Obj sho_head =
{
  .infoPtr   = &it_head,
  .objType   = FUN,
  .ident     = "head",
  };
Obj sho_tail =
{
  .infoPtr   = &it_tail,
  .objType   = FUN,
  .ident     = "tail",
  };
Obj sho_foldl =
{
  .infoPtr   = &it_foldl,
  .objType   = FUN,
  .ident     = "foldl",
  };
Obj sho_sum =
{
  .infoPtr   = &it_sum,
  .objType   = FUN,
  .ident     = "sum",
  };
Obj sho_zipWith =
{
  .infoPtr   = &it_zipWith,
  .objType   = FUN,
  .ident     = "zipWith",
  };
Obj sho_seq =
{
  .infoPtr   = &it_seq,
  .objType   = FUN,
  .ident     = "seq",
  };
Obj sho_forcelist =
{
  .infoPtr   = &it_forcelist,
  .objType   = FUN,
  .ident     = "forcelist",
  };

void registerSHOs() {
  stgStatObj[stgStatObjCount++] = &sho_main;
  stgStatObj[stgStatObjCount++] = &sho_main1;
  stgStatObj[stgStatObjCount++] = &sho_list1;
  stgStatObj[stgStatObjCount++] = &sho_list2;
  stgStatObj[stgStatObjCount++] = &sho_list3;
  stgStatObj[stgStatObjCount++] = &sho_list4;
  stgStatObj[stgStatObjCount++] = &sho_list5;
  stgStatObj[stgStatObjCount++] = &sho_mainappend;
  stgStatObj[stgStatObjCount++] = &sho_error;
  stgStatObj[stgStatObjCount++] = &sho_unit;
  stgStatObj[stgStatObjCount++] = &sho_nil;
  stgStatObj[stgStatObjCount++] = &sho_zero;
  stgStatObj[stgStatObjCount++] = &sho_one;
  stgStatObj[stgStatObjCount++] = &sho_two;
  stgStatObj[stgStatObjCount++] = &sho_three;
  stgStatObj[stgStatObjCount++] = &sho_four;
  stgStatObj[stgStatObjCount++] = &sho_five;
  stgStatObj[stgStatObjCount++] = &sho_six;
  stgStatObj[stgStatObjCount++] = &sho_seven;
  stgStatObj[stgStatObjCount++] = &sho_eight;
  stgStatObj[stgStatObjCount++] = &sho_nine;
  stgStatObj[stgStatObjCount++] = &sho_ten;
  stgStatObj[stgStatObjCount++] = &sho_multInt;
  stgStatObj[stgStatObjCount++] = &sho_plusInt;
  stgStatObj[stgStatObjCount++] = &sho_subInt;
  stgStatObj[stgStatObjCount++] = &sho_eqInt;
  stgStatObj[stgStatObjCount++] = &sho_append;
  stgStatObj[stgStatObjCount++] = &sho_constf;
  stgStatObj[stgStatObjCount++] = &sho_apply;
  stgStatObj[stgStatObjCount++] = &sho_map;
  stgStatObj[stgStatObjCount++] = &sho_take;
  stgStatObj[stgStatObjCount++] = &sho_head;
  stgStatObj[stgStatObjCount++] = &sho_tail;
  stgStatObj[stgStatObjCount++] = &sho_foldl;
  stgStatObj[stgStatObjCount++] = &sho_sum;
  stgStatObj[stgStatObjCount++] = &sho_zipWith;
  stgStatObj[stgStatObjCount++] = &sho_seq;
  stgStatObj[stgStatObjCount++] = &sho_forcelist;
}


DEFUN1(tnk_main, self) {
  fprintf(stderr, "main here\n");  stgThunk(self);
  stgCurVal = HOTOPL(&sho_main1);
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_main1, self) {
  fprintf(stderr, "main1 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_multInt), HOTOPL(&sho_three), HOTOPL(&sho_four));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_list4, self) {
  fprintf(stderr, "list4 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_append), HOTOPL(&sho_list3), HOTOPL(&sho_list3));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_list5, self) {
  fprintf(stderr, "list5 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_append), HOTOPL(&sho_list4), HOTOPL(&sho_list4));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_mainappend, self) {
  fprintf(stderr, "mainappend here\n");  stgThunk(self);
  Obj *f_0 = stgNewHeapObj();
  *f_0 = (Obj) 
        { .objType = THUNK,
          .infoPtr = &it_f_0,
          .ident = "f_0",
                };
  STGAPPLY2(HOTOPL(&sho_seq), HOTOPL(STGHEAPAT(-1)), HOTOPL(&sho_list5));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_f_0, self) {
  fprintf(stderr, "f_0 here\n");  stgThunk(self);
  STGAPPLY1(HOTOPL(&sho_forcelist), HOTOPL(&sho_list5));
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_multInt, self, x, y) {
  fprintf(stderr, "multInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_0,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_0) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_1,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_1) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_2,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i * (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_2) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      Obj *result_0 = stgNewHeapObj();
      *result_0 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_0,
              .ident = "result_0",
              .payload[0] = scrutinee,
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_plusInt, self, x, y) {
  fprintf(stderr, "plusInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_3,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_3) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_4,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_4) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_5,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i + (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_5) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      Obj *result_1 = stgNewHeapObj();
      *result_1 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_1,
              .ident = "result_1",
              .payload[0] = scrutinee,
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_subInt, self, x, y) {
  fprintf(stderr, "subInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_6,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_6) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_7,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_7) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_8,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i - (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_8) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      Obj *result_2 = stgNewHeapObj();
      *result_2 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_2,
              .ident = "result_2",
              .payload[0] = scrutinee,
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_eqInt, self, x, y) {
  fprintf(stderr, "eqInt here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_9,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_9) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_10,
          // load payload with FVs i
          .payload[0] = scrutinee.op->payload[0],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_10) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 3: {
      stgPushCont( (Cont)
        { .retAddr = &alts_11,
          // no FVs
            });
      stgCurVal.argType = INT;
      stgCurVal.i = (cont.payload[0]).i - (scrutinee.op->payload[0]).i;
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_11) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      stgCurVal = (scrutinee).i?HOTOPL(&sho_True):HOTOPL(&sho_False);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN3(fun_append, self, l1, l2) {
  fprintf(stderr, "append here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_12,
      // load payload with FVs l2
      .payload[0] = l2,
    });
  stgCurVal = l1;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_12) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      Obj *rec_0 = stgNewHeapObj();
      *rec_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_0,
              .ident = "rec_0",
              .payload[0] = scrutinee.op->payload[1],
              .payload[1] = cont.payload[0],
            };
      Obj *result_3 = stgNewHeapObj();
      *result_3 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_3,
              .ident = "result_3",
              .payload[0] = scrutinee.op->payload[0],
              .payload[1] = HOTOPL(STGHEAPAT(-2)),
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(tnk_rec_0, self) {
  fprintf(stderr, "rec_0 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_append), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_constf, self, x, y) {
  fprintf(stderr, "constf here\n");
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_apply, self, f, x) {
  fprintf(stderr, "apply here\n");
  STGAPPLY1(f, x);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_map, self, f, list) {
  fprintf(stderr, "map here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_13,
      // load payload with FVs f
      .payload[0] = f,
    });
  stgCurVal = list;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_13) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      Obj *x_0 = stgNewHeapObj();
      *x_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_x_0,
              .ident = "x_0",
              .payload[0] = cont.payload[0],
              .payload[1] = scrutinee.op->payload[0],
            };
      Obj *rec_1 = stgNewHeapObj();
      *rec_1 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_1,
              .ident = "rec_1",
              .payload[0] = cont.payload[0],
              .payload[1] = scrutinee.op->payload[1],
            };
      Obj *res_0 = stgNewHeapObj();
      *res_0 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_res_0,
              .ident = "res_0",
              .payload[0] = HOTOPL(STGHEAPAT(-3)),
              .payload[1] = HOTOPL(STGHEAPAT(-2)),
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(tnk_x_0, self) {
  fprintf(stderr, "x_0 here\n");  stgThunk(self);
  STGAPPLY1(self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_rec_1, self) {
  fprintf(stderr, "rec_1 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_map), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_take, self, n, xs) {
  fprintf(stderr, "take here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_14,
      // load payload with FVs xs n
      .payload[0] = xs,
      .payload[1] = n,
    });
  STGAPPLY2(HOTOPL(&sho_eqInt), n, HOTOPL(&sho_zero));
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_14) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 4: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 5: {
      stgPushCont( (Cont)
        { .retAddr = &alts_15,
          // load payload with FVs n
          .payload[0] = cont.payload[1],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_15) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      Obj *m_0 = stgNewHeapObj();
      *m_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_m_0,
              .ident = "m_0",
              .payload[0] = cont.payload[0],
            };
      Obj *rec_2 = stgNewHeapObj();
      *rec_2 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_2,
              .ident = "rec_2",
              .payload[0] = HOTOPL(STGHEAPAT(-3)),
              .payload[1] = scrutinee.op->payload[1],
            };
      Obj *result_4 = stgNewHeapObj();
      *result_4 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_4,
              .ident = "result_4",
              .payload[0] = scrutinee.op->payload[0],
              .payload[1] = HOTOPL(STGHEAPAT(-2)),
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(tnk_m_0, self) {
  fprintf(stderr, "m_0 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_subInt), self.op->payload[0], HOTOPL(&sho_one));
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_rec_2, self) {
  fprintf(stderr, "rec_2 here\n");  stgThunk(self);
  STGAPPLY2(HOTOPL(&sho_take), self.op->payload[0], self.op->payload[1]);
  STGRETURN0();
  ENDFUN;
}

DEFUN2(fun_head, self, xs) {
  fprintf(stderr, "head here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_16,
      // no FVs
        });
  stgCurVal = xs;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_16) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 0: {
      stgCurVal = scrutinee.op->payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN2(fun_tail, self, xs) {
  fprintf(stderr, "tail here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_17,
      // no FVs
        });
  stgCurVal = xs;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_17) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 0: {
      stgCurVal = scrutinee.op->payload[1];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN4(fun_foldl, self, f, acc, list) {
  fprintf(stderr, "foldl here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_18,
      // load payload with FVs acc f
      .payload[0] = acc,
      .payload[1] = f,
    });
  stgCurVal = list;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_18) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      Obj *newAcc_0 = stgNewHeapObj();
      *newAcc_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_newAcc_0,
              .ident = "newAcc_0",
              .payload[0] = cont.payload[1],
              .payload[1] = cont.payload[0],
              .payload[2] = scrutinee.op->payload[0],
            };
      STGAPPLY3(HOTOPL(&sho_foldl), cont.payload[1], HOTOPL(STGHEAPAT(-1)), scrutinee.op->payload[1]);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(tnk_newAcc_0, self) {
  fprintf(stderr, "newAcc_0 here\n");  stgThunk(self);
  STGAPPLY2(self.op->payload[0], self.op->payload[1], self.op->payload[2]);
  STGRETURN0();
  ENDFUN;
}

DEFUN2(fun_sum, self, list) {
  fprintf(stderr, "sum here\n");
  STGAPPLY3(HOTOPL(&sho_foldl), HOTOPL(&sho_plusInt), HOTOPL(&sho_zero), list);
  STGRETURN0();
  ENDFUN;
}

DEFUN4(fun_zipWith, self, f, list1, list2) {
  fprintf(stderr, "zipWith here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_19,
      // load payload with FVs list2 f
      .payload[0] = list2,
      .payload[1] = f,
    });
  stgCurVal = list1;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_19) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      stgPushCont( (Cont)
        { .retAddr = &alts_20,
          // load payload with FVs f h1 t1
          .payload[0] = cont.payload[1],
          .payload[1] = scrutinee.op->payload[0],
          .payload[2] = scrutinee.op->payload[1],
        });
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN0(alts_20) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = HOTOPL(&sho_nil);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      Obj *newHead_0 = stgNewHeapObj();
      *newHead_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_newHead_0,
              .ident = "newHead_0",
              .payload[0] = cont.payload[0],
              .payload[1] = cont.payload[1],
              .payload[2] = scrutinee.op->payload[0],
            };
      Obj *newTail_0 = stgNewHeapObj();
      *newTail_0 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_newTail_0,
              .ident = "newTail_0",
              .payload[0] = cont.payload[0],
              .payload[1] = cont.payload[2],
              .payload[2] = scrutinee.op->payload[1],
            };
      Obj *result_5 = stgNewHeapObj();
      *result_5 = (Obj) 
            { .objType = CON,
              .infoPtr = &it_result_5,
              .ident = "result_5",
              .payload[0] = HOTOPL(STGHEAPAT(-3)),
              .payload[1] = HOTOPL(STGHEAPAT(-2)),
            };
      stgCurVal = HOTOPL(STGHEAPAT(-1));
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(tnk_newHead_0, self) {
  fprintf(stderr, "newHead_0 here\n");  stgThunk(self);
  STGAPPLY2(self.op->payload[0], self.op->payload[1], self.op->payload[2]);
  STGRETURN0();
  ENDFUN;
}

DEFUN1(tnk_newTail_0, self) {
  fprintf(stderr, "newTail_0 here\n");  stgThunk(self);
  STGAPPLY3(HOTOPL(&sho_zipWith), self.op->payload[0], self.op->payload[1], self.op->payload[2]);
  STGRETURN0();
  ENDFUN;
}

DEFUN3(fun_seq, self, x, y) {
  fprintf(stderr, "seq here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_21,
      // load payload with FVs y
      .payload[0] = y,
    });
  stgCurVal = x;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_21) {
  Cont cont = stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    casedefault:
    default: {
      stgCurVal = cont.payload[0];
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN2(fun_forcelist, self, list) {
  fprintf(stderr, "forcelist here\n");
  stgPushCont( (Cont)
    { .retAddr = &alts_22,
      // no FVs
        });
  stgCurVal = list;
  STGEVAL(stgCurVal);
  STGRETURN0();
  ENDFUN;
}

DEFUN0(alts_22) {
stgPopCont();
  PtrOrLiteral scrutinee = stgCurVal;
  if (scrutinee.argType != HEAPOBJ ||
      scrutinee.op->objType != CON ) goto casedefault;
  switch(scrutinee.op->infoPtr->conFields.tag) {
    case 2: {
      stgCurVal = HOTOPL(&sho_unit);
      STGEVAL(stgCurVal);
      STGRETURN0();
    }
    case 0: {
      Obj *rec_3 = stgNewHeapObj();
      *rec_3 = (Obj) 
            { .objType = THUNK,
              .infoPtr = &it_rec_3,
              .ident = "rec_3",
              .payload[0] = scrutinee.op->payload[1],
            };
      STGAPPLY2(HOTOPL(&sho_seq), scrutinee.op->payload[0], HOTOPL(STGHEAPAT(-1)));
      STGRETURN0();
    }
    casedefault:
    default: {
      STGAPPLY1(HOTOPL(&sho_stg_case_not_exhaustive), scrutinee);
      STGRETURN0();
    }
  }
  ENDFUN;
}


DEFUN1(tnk_rec_3, self) {
  fprintf(stderr, "rec_3 here\n");  stgThunk(self);
  STGAPPLY1(HOTOPL(&sho_forcelist), self.op->payload[0]);
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
  showStgHeap();
  return 0;
}

