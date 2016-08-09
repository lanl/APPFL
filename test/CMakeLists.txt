project(stgc_test)
  
  cmake_minimum_required(VERSION 2.8.12.2)
  set(DART_TESTING_TIMEOUT "30" CACHE STRING "" FORCE)
  include(CTest)

  set(BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

  # default log level result/fatal/error only.
  set(RT_LOG_LEVEL 3)
  # run tests over these lists
  set(strict_list "0;1;2")
  set(constr_list "0")
  set(gc_list "0;1")
  set(sanity_list "0;1") 

  add_subdirectory(stg)
  add_subdirectory(mhs)
  add_subdirectory(hs)
