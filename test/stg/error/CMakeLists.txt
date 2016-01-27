project(stgc_test_error)

set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/stg/error)

macro(add_test_regex_fail name regex)
    set(tname error_${name})
    add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
    set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION ${regex})
    set_property(TEST ${tname} PROPERTY WILL_FAIL) 
endmacro(add_test_regex_fail)

add_test_regex_fail(comment 
  "mismatched comment braces:")

add_test_regex_fail(semi
    "Expected semicolon or EOF after object definition")

add_test_regex_fail(adt1
    "Expected valid constructor name in datatype declaration")

add_test_regex_fail(adt2
    "Expected one or more data constructor definitions separated by")

add_test_regex_fail(obj
    "Expected object definition")

add_test_regex_fail(thunk
    "Expected valid expression in thunk body")

add_test_regex_fail(freevar
    "top level free variables:")

add_test_regex_fail(fun1
    "Expected 1 or more valid function variables")

add_test_regex_fail(fun2
    "to initiate function body")

add_test_regex_fail(fun3
    "Expected valid expression in function body")

add_test_regex_fail(con1
    "Expected a valid constructor name")

add_test_regex_fail(con2
    "constructor J not in conmap")

add_test_regex_fail(polyUnboxed
    "cannot unify unboxed type")

add_test_regex_fail(pap2
    "unsupported explicit PAP")

add_test_regex_fail(topLevel
    "function name badArgs duplicated 2 times")

add_test_regex_fail(funArg1
    "variable x duplicated 2 times in location: toplevel.badArgs.")

add_test_regex_fail(funName1
    "function name y duplicated 2 times in location: toplevel.badFunc.let.")

add_test_regex_fail(dataDouble
    "data type Mybool duplicated 2 times")

add_test_regex_fail(dataDouble1
    "constructor False duplicated 2 times in location: toplevel.not1.case.alts.def.let.case.alts.")

add_test_regex_fail(funName2
    "function name a duplicated 2 times in location: toplevel.f1.case.alts.def.let.")

add_test_regex_fail(dataCon1
    "data type Bool duplicated 2 times  --  data constructor False duplicated 3 times in Bool,Bool,Bool1, --  data constructor True duplicated 3 times in Bool,Bool,Bool1, --")

add_test_regex_fail(dataType1
    "data type Bool1 duplicated 2 times  --  data constructor False duplicated 2 times in Bool,Bool1, --  data constructor Nil duplicated 2 times in List,Bool1, --  data constructor True duplicated 2 times in Bool,Bool1,")

add_test_regex_fail(dataCon2
    "data constructor False duplicated 3 times in Bool,Bool1,Bool2, --  data constructor Nil duplicated 2 times in List,Bool2, --  data constructor True duplicated 3 times in Bool,Bool1,Bool3,")

add_test_regex_fail(dataType2
    "data type Bool1 duplicated 2 times  --  data type Bool2 duplicated 2 times  --  data constructor False duplicated 4 times in Bool,Bool1,Bool1,Bool2, --  data constructor Nil duplicated 2 times in List,Bool1, --  data constructor True duplicated 4 times in Bool,Bool1,Bool2,Bool2,")

add_test_regex_fail(dataCons3
    "data constructor False duplicated 5 times in Bool,Bool1,Bool1,Bool2,Bool2, --  data constructor Nil duplicated 2 times in List,Bool2, --  data constructor True duplicated 4 times in Bool,Bool1,Bool1,Bool2")

add_test_regex_fail(dataType3
    "data type Bool1 duplicated 4 times  --  data type Bool2 duplicated 2 times  --  data type Bool3 duplicated 2 times  --  data constructor False duplicated 3 times in Bool,Bool1,Bool1, --  data constructor Int duplicated 2 times in Bool2,Bool3, --  data constructor Nil duplicated 2 times in List,Bool1, --  data constructor True duplicated 3 times in Bool,Bool1,Bool1,")

