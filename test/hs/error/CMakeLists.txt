project(stgc_test_hs_error)

set(APPFL "${CMAKE_CURRENT_BINARY_DIR}/../../../bin/appfl")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/hs/error)

macro(add_test_fail name)
    set(tname error_${name})
    add_test(${tname} ${APPFL} ${SRC_DIR}/${name}.hs)
    set_property(TEST ${tname} PROPERTY WILL_FAIL) 
endmacro(add_test_fail)

macro(add_test_regex_fail name regex)
    set(tname hs_error_${name})
    add_test(${tname} ${APPFL} ${SRC_DIR}/${name}.hs)
    set_property(TEST ${tname} PROPERTY PASS_REGULAR_EXPRESSION ${regex})
    set_property(TEST ${tname} PROPERTY WILL_FAIL) 
endmacro(add_test_regex_fail)

add_test_fail(emptyhead)

