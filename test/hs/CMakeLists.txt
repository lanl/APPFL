project(stgc_test_stg)
  
set(STGCC "${CMAKE_CURRENT_BINARY_DIR}/../../bin/stgc")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/hs)
set(BIN_DIR ${BINARY_DIR}/hs)

# run a test and check that result is True
macro(add_test_true name build command)
    add_test(${name} ${command} ${ARGN})
    set_tests_properties(${name} PROPERTIES DEPENDS ${build})
    set_tests_properties(${name} PROPERTIES PASS_REGULAR_EXPRESSION
      "The answer is[\r\n\t ]*[a-zA-Z0-9_]* = APPFLziTypesziTrue")
endmacro(add_test_true)

file(GLOB ALL_SRCS *.hs)

foreach (test ${ALL_SRCS})
    get_filename_component(fname ${test} NAME_WE) 
    set(tname hs_${fname})
    set(build build_${tname})
    set(binary ${BIN_DIR}/${tname})
    add_test(${build} ${STGCC} ${SRC_DIR}/${fname}.hs -o ${tname})
   
    foreach (strict 0 1 2)
      foreach (constr 0) 
        foreach (gc "" "-g")
          foreach (sanity "" "-s")
            set(name ${tname}-e${strict}-c${constr}${gc}${sanity})
      
            add_test_true(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -c${constr} ${gc} ${sanity} 2>&1)  
          endforeach(sanity)
        endforeach(gc)
      endforeach(constr)
    endforeach(strict)
endforeach(test)

