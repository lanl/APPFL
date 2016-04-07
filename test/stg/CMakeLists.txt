project(stgc_test_stg)
  
set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/stg)
set(BIN_DIR ${BINARY_DIR}/stg)

# run a test and check that result is True
macro(add_test_true name build command)
    add_test(${name} ${command} ${ARGN})
    set_tests_properties(${name} PROPERTIES DEPENDS ${build})
    set_tests_properties(${name} PROPERTIES PASS_REGULAR_EXPRESSION
      "The answer is[\r\n\t ]*true = True")
endmacro(add_test_true)

file(GLOB ALL_SRCS *.stg)

foreach (test ${ALL_SRCS})
    get_filename_component(tname ${test} NAME_WE) 
    set(build build_${tname})
    set(binary ${BIN_DIR}/${tname})
    add_test(${build} ${STGCC} -v ${SRC_DIR}/${tname}.stg -o ${tname})
   
    foreach (strict 0 1 2)
      if(strict EQUAL 0) 
         set(name ${tname}_nonstrict)
      else()
         set(name ${tname}_strict${strict})
      endif() 
      
      #with gc
      add_test_true(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} 2>&1)  

      #w/o gc
      add_test_true(${name}_nogc ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -g 2>&1)  
    
    endforeach(strict)

endforeach(test)

add_subdirectory(nonstrict)
add_subdirectory(error)
