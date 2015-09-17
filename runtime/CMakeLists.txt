project(runtime)
  cmake_minimum_required(VERSION 2.8.12.2)

  set(SHAREDLIB OFF)

  set(rt_hdrs args.h 
              cmm.h
              gc.h
              stg.h
              stgutils.h
	      stgApply.h
    )

  set(rt_srcs args.c 
              cmm.c
              gc.c
              stg.c
              stgutils.c
    )

  # put headers in build/include
  foreach (header ${rt_hdrs})
    configure_file(${header} ${PROJECT_BINARY_DIR}/../include/${header} COPYONLY)
  endforeach()
  configure_file(stgc.h ${PROJECT_BINARY_DIR}/../include/stgc.h COPYONLY)
  configure_file(cruntime.h ${PROJECT_BINARY_DIR}/../include/cruntime.h COPYONLY)

  if(NOT APPLE) 
    set(CMAKE_C_FLAGS "-Wno-unused-but-set-variable")
  else()
    set(CMAKE_C_FLAGS "-Wno-unused-const-variable")
  endif()

  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -std=gnu99 -DDEBUGSTGAPPLY=1")

  include_directories(
    "/usr/include/malloc"
  )

  # put in build/lib
  if(SHAREDLIB) 
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
    add_library(runtime-ns SHARED ${rt_srcs} stgApply-nonstrict.c ${rt_hdrs})
    add_library(runtime-s SHARED ${rt_srcs} stgApply-strict.c ${rt_hdrs})
  else()
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
    add_library(runtime-ns STATIC ${rt_srcs} stgApply-nonstrict.c ${rt_hdrs})
    add_library(runtime-s STATIC ${rt_srcs} stgApply-strict.c ${rt_hdrs})
  endif()

