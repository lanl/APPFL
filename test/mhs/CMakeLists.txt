project(stgc_test_mhs)

set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/mhs)

file(GLOB ALL_SRCS *.mhs)

  foreach (test ${ALL_SRCS})
    get_filename_component(name ${test} NAME_WE)
    set(tname mhs_${name})
    if(name STREQUAL "eqintfalse")
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -v -c ${SRC_DIR}/${name}.mhs -o ${tname})
       add_test(${tname} ${BINARY_DIR}/mhs/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]gfc_False = False")
    else()
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -v -c ${SRC_DIR}/${name}.mhs -o ${tname})
       add_test(${tname} ${BINARY_DIR}/mhs/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]gfc_True = True")
    endif()
  endforeach(test)
