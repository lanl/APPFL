project(stgc_test_stg_nonstrict_timeout)
  
set(APPFL "${CMAKE_CURRENT_BINARY_DIR}/../../../../bin/appfl")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/stg/nonstrict/timeout)
set(BIN_DIR ${BINARY_DIR}/stg/nonstrict/timeout)


macro(add_test_timeout name build command)
    add_test(${name} timeout 1 ${command} ${ARGN})
    set_tests_properties(${name} PROPERTIES DEPENDS ${build})
    #set_tests_properties(${name} PROPERTIES TIMEOUT 2)
    set_tests_properties(${name} PROPERTIES WILL_FAIL 1)
endmacro(add_test_timeout)

file(GLOB ALL_SRCS *.stg)

foreach (test ${ALL_SRCS})
    get_filename_component(fname ${test} NAME_WE) 
    set(tname timeout_${fname})
    set(build build_${tname})
    set(binary ${BIN_DIR}/${tname})
    add_test(${build} ${APPFL} ${SRC_DIR}/${fname}.stg -o ${tname})

    foreach (strict 0 1 2)
      foreach (constr 0) 
        foreach (gc "" "-g")
          foreach (sanity "" "-s")
            set(name ${tname}-e${strict}-c${constr}${gc}${sanity})

            if (strict EQUAL 0) 
              add_test_true(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -c${constr} ${gc} ${sanity} 2>&1)  
            else() 
              if (UNIX AND NOT APPLE)
                add_test_timeout(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -c${constr} ${gc} ${sanity} 2>&1)  
              endif()
            endif()
          endforeach(sanity)
        endforeach(gc)
      endforeach(constr) 
    endforeach(strict)
endforeach(test)

