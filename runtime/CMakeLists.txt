project(runtime)
  cmake_minimum_required(VERSION 2.8.12.2)

  set(SHAREDLIB OFF)

  set(rt_hdrs args.h 
              cmm.h
              gc.h
              obj.h
              stg.h
              stgutils.h
	      cruntime.h
	      stgc.h
    )

  set(xrt_hdrs ../codegen/options.h
               ../stgApply/stgApply.h
     )

  set(rt_srcs args.c 
              cmm.c
              gc.c
              stg.c
              stgutils.c
	      stghs.c
    )

include_directories("../codegen"
                   .
                   )

  # put headers in build/include
  foreach (header ${rt_hdrs})
    configure_file(${header} ${PROJECT_BINARY_DIR}/../include/${header} COPYONLY)
  endforeach()
  configure_file(../codegen/options.h ${PROJECT_BINARY_DIR}/../include/options.h COPYONLY)
  configure_file(../stgApply/stgApply.h ${PROJECT_BINARY_DIR}/../include/stgApply.h COPYONLY)

  if(NOT APPLE) 
    set(CMAKE_C_FLAGS "-Wno-unused-but-set-variable")
  else()
    set(CMAKE_C_FLAGS "-Wno-unused-const-variable")
  endif()

  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -std=gnu99 -Wall")

  include_directories(
    "/usr/include/malloc"
  )

  # put in build/lib
  if(SHAREDLIB) 
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
    add_library(runtime-ns SHARED ${rt_srcs} ../stgApply/nonstrict.c ${rt_hdrs} ${xrt_hdrs})
    add_library(runtime-s SHARED ${rt_srcs} ../stgApply/strict.c ${rt_hdrs} ${xrt_hdrs})
  else()
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
    add_library(runtime-ns STATIC ${rt_srcs} ../stgApply/nonstrict.c ${rt_hdrs} ${xrt_hdrs})
    add_library(runtime-s STATIC ${rt_srcs} ../stgApply/strict.c ${rt_hdrs} ${xrt_hdrs})
  endif()

