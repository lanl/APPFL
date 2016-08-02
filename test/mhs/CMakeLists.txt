project(stgc_test_mhs)

set(APPFL "${CMAKE_CURRENT_BINARY_DIR}/../../bin/appfl")
set(SRC_DIR ${CMAKE_SOURCE_DIR}/test/mhs)
set(BIN_DIR ${BINARY_DIR}/mhs)

# run a test and check that result is True
macro(add_test_true name build command)
    add_test(${name} ${command} ${ARGN})
    set_tests_properties(${name} PROPERTIES DEPENDS ${build})
    set_tests_properties(${name} PROPERTIES PASS_REGULAR_EXPRESSION
    "The answer is[\r\n\t ]gfc_True = True")
endmacro(add_test_true)

file(GLOB ALL_SRCS *.mhs)

foreach (test ${ALL_SRCS})
    get_filename_component(fname ${test} NAME_WE)
    set(tname mhs_${fname})
    set(build build_${tname})
    set(binary ${BIN_DIR}/${tname})
    add_test(${build} ${APPFL} ${SRC_DIR}/${fname}.mhs -o ${tname})
    foreach (strict ${strict_list})
      foreach (constr ${constr_list})
        foreach (gc ${gc_list})
          foreach (sanity ${sanity_list})
            set(name ${tname}-e${strict}-c${constr}-g${gc}-s${sanity})
            add_test_true(${name} ${build} ${binary} -l${RT_LOG_LEVEL} -e${strict} -c${constr} -g${gc} -is${sanity} 2>&1)
          endforeach(sanity)
        endforeach(gc)
      endforeach(constr)
    endforeach(strict)
endforeach(test)
