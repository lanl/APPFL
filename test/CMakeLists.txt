project(stgc_test)
  
  cmake_minimum_required(VERSION 2.8.12.2)
  set(DART_TESTING_TIMEOUT "15" CACHE STRING "" FORCE)
  include(CTest)

  set(STGCC "../codegen/dist/build/stgc/stgc")
  set(SRC_DIR ${CMAKE_SOURCE_DIR})
  set(BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

  file(GLOB ALL_SRCS *.stg)

  foreach (test ${ALL_SRCS})
    get_filename_component(tname ${test} NAME_WE) 
    if(tname STREQUAL "eqintfalse")
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -c ${SRC_DIR}/${tname}.stg -o ${tname})
       add_test(${tname} ${BINARY_DIR}/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*= False")
    else()
       set(bname build_${tname})
       add_test(${bname} ${STGCC} -c ${SRC_DIR}/${tname}.stg -o ${tname})
       add_test(${tname} ${BINARY_DIR}/${tname} 2>&1)
       set_tests_properties(${tname} PROPERTIES DEPENDS ${bname})
       set_tests_properties(${tname} PROPERTIES PASS_REGULAR_EXPRESSION
       "The answer is[\r\n\t ]*= True")
    endif()
  endforeach(test)
