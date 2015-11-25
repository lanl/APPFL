project(stgc_test_error)

set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/error)

set(name comment)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "mismatched comment braces:")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name semi)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected semicolon or EOF after object definition")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name adt1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected valid constructor name in datatype declaration")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name adt2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected one or more data constructor definitions separated by")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name obj)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected object definition")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name thunk)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected valid expression in thunk body")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name freevar)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "top level free variables:")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name fun1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected 1 or more valid function variables")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name fun2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "to initiate function body")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name fun3)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected valid expression in function body")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name con1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "Expected a valid constructor name")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name con2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "constructor J not in conmap")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name polyUnboxed)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "cannot unify unboxed type")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name pap2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "unsupported explicit PAP")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name topLevel)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "function name badArgs duplicated 2 times")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name funArg1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "variable x duplicated 2 times in location: toplevel.badArgs.")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name funName1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "function name y duplicated 2 times in location: toplevel.badFunc.let.")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name dataDouble)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data type Mybool duplicated 2 times")
set_property(TEST ${tname} PROPERTY WILL_FAIL) 

set(name dataDouble1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "constructor False duplicated 2 times in location: toplevel.not1.case.alts.def.let.case.alts.")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name funName2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "function name a duplicated 2 times in location: toplevel.f1.case.alts.def.let.")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name dataCon1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data type Bool duplicated 2 times  --  data constructor False duplicated 3 times in Bool,Bool,Bool1, --  data constructor True duplicated 3 times in Bool,Bool,Bool1, --")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name dataType1)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data type Bool1 duplicated 2 times  --  data constructor False duplicated 2 times in Bool,Bool1, --  data constructor Nil duplicated 2 times in List,Bool1, --  data constructor True duplicated 2 times in Bool,Bool1,")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name dataCon2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data constructor False duplicated 3 times in Bool,Bool1,Bool2, --  data constructor Nil duplicated 2 times in List,Bool2, --  data constructor True duplicated 3 times in Bool,Bool1,Bool3,")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name dataType2)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data type Bool1 duplicated 2 times  --  data type Bool2 duplicated 2 times  --  data constructor False duplicated 4 times in Bool,Bool1,Bool1,Bool2, --  data constructor Nil duplicated 2 times in List,Bool1, --  data constructor True duplicated 4 times in Bool,Bool1,Bool2,Bool2,")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name dataCons3)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data constructor False duplicated 5 times in Bool,Bool1,Bool1,Bool2,Bool2, --  data constructor Nil duplicated 2 times in List,Bool2, --  data constructor True duplicated 4 times in Bool,Bool1,Bool1,Bool2")
set_property(TEST ${tname} PROPERTY WILL_FAIL)

set(name dataType3)
set(tname error_${name})
add_test(${tname} ${STGCC} ${SRC_DIR}/${name}.stg)
set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION 
    "data type Bool1 duplicated 4 times  --  data type Bool2 duplicated 2 times  --  data type Bool3 duplicated 2 times  --  data constructor False duplicated 3 times in Bool,Bool1,Bool1, --  data constructor Int duplicated 2 times in Bool2,Bool3, --  data constructor Nil duplicated 2 times in List,Bool1, --  data constructor True duplicated 3 times in Bool,Bool1,Bool1,")
set_property(TEST ${tname} PROPERTY WILL_FAIL)