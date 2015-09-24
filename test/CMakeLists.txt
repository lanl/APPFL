project(stgc_test)
  
  cmake_minimum_required(VERSION 2.8.12.2)
  set(DART_TESTING_TIMEOUT "15" CACHE STRING "" FORCE)
  include(CTest)

  set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../bin/stgc")
  set(SRC_DIR ${CMAKE_SOURCE_DIR}/test)
  set(BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

  file(GLOB ALL_SRCS *.stg)

  foreach (test ${ALL_SRCS})
    get_filename_component(tname ${test} NAME_WE) 
    if(tname STREQUAL "repminlist")
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -v ${SRC_DIR}/${tname}.stg -o ${tname})
       add_test(${tname} ${BINARY_DIR}/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*true = True")
       set(t2name ${tname}_nogc)
       add_test(${t2name} ${BINARY_DIR}/${tname} -g 2>&1)
       set_tests_properties(${t2name} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${t2name} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*true = True")
    elseif(tname STREQUAL "eqintfalse")
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -v ${SRC_DIR}/${tname}.stg -o ${tname})
       add_test(${tname} ${BINARY_DIR}/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*false = False")
       set(t2name ${tname}_nogc)
       add_test(${t2name} ${BINARY_DIR}/${tname} -g 2>&1)
       set_tests_properties(${t2name} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${t2name} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*false = False")
       set(tnames ${tname}_strict)
       set(bnames build_${tnames})
       add_test(${bnames} ${STGCC} -v -s ${SRC_DIR}/${tname}.stg -o ${tnames})
       add_test(${tnames} ${BINARY_DIR}/${tnames} 2>&1)
       set_tests_properties(${tnames} PROPERTIES DEPENDS ${bnames})
       set_tests_properties(${tnames} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*false = False")
       set(t2names ${tnames}_nogc)
       add_test(${t2names} ${BINARY_DIR}/${tnames} -g 2>&1)
       set_tests_properties(${t2names} PROPERTIES DEPENDS ${bnames})
       set_tests_properties(${t2names} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*false = False")
    else()
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -v ${SRC_DIR}/${tname}.stg -o ${tname})
       add_test(${tname} ${BINARY_DIR}/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*true = True")
       set(t2name ${tname}_nogc)
       add_test(${t2name} ${BINARY_DIR}/${tname} -g 2>&1)
       set_tests_properties(${t2name} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${t2name} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*true = True")
       set(tnames ${tname}_strict)
       set(bnames build_${tnames})
       add_test(${bnames} ${STGCC} -v -s ${SRC_DIR}/${tname}.stg -o ${tnames})
       add_test(${tnames} ${BINARY_DIR}/${tnames} 2>&1)
       set_tests_properties(${tnames} PROPERTIES DEPENDS ${bnames})
       set_tests_properties(${tnames} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*true = True")
        set(t2names ${tnames}_nogc)
       add_test(${t2names} ${BINARY_DIR}/${tnames} -g 2>&1)
       set_tests_properties(${t2names} PROPERTIES DEPENDS ${bnames})
       set_tests_properties(${t2names} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*true = True")
    endif()
  endforeach(test)

  add_subdirectory(error)
  add_subdirectory(mhs)
