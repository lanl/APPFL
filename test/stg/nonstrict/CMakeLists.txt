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

    #with gc     
    add_test_true(${tname}_nonstrict ${build} ${binary} -l${RT_LOG_LEVEL} -e0 2>&1)           
    
    #w/o gc 
    add_test_true(${tname}_nonstrict_nogc ${build} ${binary} -l${RT_LOG_LEVEL} -e0 -g 2>&1)     

    foreach (strict 0 1 2)
      foreach (constr 0) 

        set(name ${tname}_e${strict}_c${constr})
        if (strict EQUAL 0) 

          #with gc
          add_test_true(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} 2>&1)  

          #w/o gc
          add_test_true(${name}_g ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -g 2>&1)  
        else() 
          #with gc
          add_test_blackhole(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} 2>&1)  

          #w/o gc
          add_test_blackhole(${name}_g ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -g 2>&1)  

        endif()
      endforeach(constr) 
    endforeach(strict)
endforeach(test)

