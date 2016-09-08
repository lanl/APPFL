project(stgc_test_hs_error)

set(APPFL "${CMAKE_CURRENT_BINARY_DIR}/../../../bin/appfl")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/hs/error)

macro(add_test_fail name)
    set(tname hs_error_${name})
    add_test(${tname} ${APPFL} ${SRC_DIR}/${name}.hs ${ARGN})
    set_property(TEST ${tname} PROPERTY WILL_FAIL) 
endmacro(add_test_fail)

#TODO: why does this not work
macro(add_test_error_fail name)
    set(tname hs_error_${name})
    add_test(${tname} ${APPFL} ${SRC_DIR}/${name}.hs ${ARGN})
    set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION "APPFL_ERROR")
    set_property(TEST ${tname} PROPERTY WILL_FAIL) 
endmacro(add_test_error_fail)


add_test_fail(emptyhead)
add_test_fail(undefined)
