project(stgc_test_stg_nonstrict)
  
set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/stg/nonstrict)
set(BIN_DIR ${BINARY_DIR}/stg/nonstrict)

macro(add_test_blackhole name build command)
    add_test(${name} ${command} ${ARGN})
    set_tests_properties(${name} PROPERTIES DEPENDS ${build})
    set_tests_properties(${name} PROPERTIES PASS_REGULAR_EXPRESSION
      "terminating on BLACKHOLE")
endmacro(add_test_blackhole)

file(GLOB ALL_SRCS *.stg)

foreach (test ${ALL_SRCS})
    get_filename_component(tname ${test} NAME_WE) 
    set(build build_${tname})
    set(binary ${BIN_DIR}/${tname})
    add_test(${build} ${STGCC} -v ${SRC_DIR}/${tname}.stg -o ${tname})
     
    #non-strict w/ gc
    add_test_true(${tname}_nonstrict ${build} ${binary} -e0 2>&1)           
     
    #non-strict w/o gc
    add_test_true(${tname}_nonstrict_nogc ${build} ${binary} -e0 -g 2>&1)     

    #strict w/ gc 
    add_test_blackhole(${tname}_strict ${build} ${binary} -e1 2>&1)           

    #strict w/o gc
    add_test_blackhole(${tname}_strict_nogc ${build} ${binary} -e1 -g 2>&1)           
endforeach(test)

