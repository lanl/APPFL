project(stgc_test_stg)
  
set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/stg)
set(BIN_DIR ${BINARY_DIR}/stg)

file(GLOB ALL_SRCS *.stg)

foreach (test ${ALL_SRCS})
    get_filename_component(tname ${test} NAME_WE) 
     set(bname build_${tname})
     add_test(${bname} ${STGCC} -v ${SRC_DIR}/${tname}.stg -o ${tname})
     add_test(${tname} ${BIN_DIR}/${tname} -e0 2>&1)  #non-strict w/ gc
     set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
     set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
     "The answer is[\r\n\t ]*true = True")
     set(t2name ${tname}_nogc)
     add_test(${t2name} ${BIN_DIR}/${tname} -e0 -g 2>&1) #non-strict w/o gc
     set_tests_properties(${t2name} PROPERTIES DEPENDS ${bname})
     set_tests_properties(${t2name} PROPERTIES PASS_REGULAR_EXPRESSION
     "The answer is[\r\n\t ]*true = True")
     set(tnames ${tname}_strict)
     add_test(${tnames} ${BIN_DIR}/${tname} -e1 2>&1)  #strict w/ gc
     set_tests_properties(${tnames} PROPERTIES DEPENDS ${bname})
     set_tests_properties(${tnames} PROPERTIES PASS_REGULAR_EXPRESSION
     "The answer is[\r\n\t ]*true = True")
      set(t2names ${tnames}_nogc)
     add_test(${t2names} ${BIN_DIR}/${tname} -e1 -g 2>&1)  #strict w/o gc
     set_tests_properties(${t2names} PROPERTIES DEPENDS ${bname})
     set_tests_properties(${t2names} PROPERTIES PASS_REGULAR_EXPRESSION
     "The answer is[\r\n\t ]*true = True")
endforeach(test)

add_subdirectory(nonstrict)
add_subdirectory(error)
