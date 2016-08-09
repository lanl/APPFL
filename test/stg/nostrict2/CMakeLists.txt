project(stgc_test_stg_nostrict2)
  
set(APPFL "${CMAKE_CURRENT_BINARY_DIR}/../../../bin/appfl")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/stg/nostrict2)
set(BIN_DIR ${BINARY_DIR}/stg/nostrict2)

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
    add_test(${build} ${APPFL} ${SRC_DIR}/${tname}.stg -o ${tname})

 foreach (strict ${strict_list})
      foreach (constr ${constr_list})
        foreach (gc ${gc_list})
          foreach (sanity ${sanity_list})
            set(name ${tname}-e${strict}-c${constr}-g${gc}-s${sanity})
            if (strict EQUAL 2)
              add_test_blackhole(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -c${constr} -g${gc} -s${sanity} 2>&1)  
            else()
              add_test_true(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -c${constr} -g${gc} -s${sanity} 2>&1)  
            endif()
          endforeach(sanity)
        endforeach(gc)
      endforeach(constr)
    endforeach(strict)
endforeach(test)

