project(runtime)
  cmake_minimum_required(VERSION 2.8.12.2)

  set(SHAREDLIB OFF)

  set(rt_hdrs args.h 
              cmm.h
              gc.h
              log.h
              obj.h
	      heap.h
              stack.h
              stg.h
              stgutils.h
    	      cruntime.h
	      stgc.h
              stgapply.h
    )

  set(xrt_hdrs ../build/include/options.h
     )

  set(rt_srcs args.c 
              gc.c
              stg.c
              stgutils.c
	      show.c
              stack.c
	      stgapply.c
	      heap.c
    )

include_directories(".."
                    ${PROJECT_BINARY_DIR}/../include
                   .
                   )

  # put headers in build/include
  foreach (header ${rt_hdrs})
    configure_file(${header} ${PROJECT_BINARY_DIR}/../include/${header} COPYONLY)
  endforeach()

  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -g -std=gnu99 -Wall -Werror -O0" )

  include_directories(
    "/usr/include/malloc"
  )

  # put in build/lib
  if(SHAREDLIB) 
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
    add_library(runtime SHARED ${rt_srcs} ${rt_hdrs} ${xrt_hdrs})
  else()
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/../lib)
    add_library(runtime STATIC ${rt_srcs} ${rt_hdrs} ${xrt_hdrs})
  endif()

