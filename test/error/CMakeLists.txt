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
    "cannot unify unboxed type UBInt with polymorphic type variable")
set_property(TEST ${tname} PROPERTY WILL_FAIL)
