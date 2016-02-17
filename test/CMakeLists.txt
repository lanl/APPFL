project(stgc_test)
  
  cmake_minimum_required(VERSION 2.8.12.2)
  set(DART_TESTING_TIMEOUT "30" CACHE STRING "" FORCE)
  include(CTest)

  set(BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR})

  # default log level result/fatal/error only.
  set(RT_LOG_LEVEL 3)

  add_subdirectory(stg)
  add_subdirectory(mhs)
