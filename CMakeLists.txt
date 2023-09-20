cmake_minimum_required(VERSION 3.10)
project(matchbox VERSION 0.1 LANGUAGES CXX)

option(MATCHBOX_BUILD_TESTS "Build tests" OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_library(matchbox INTERFACE include/matchbox.hh)
target_include_directories(matchbox INTERFACE include)

if (MATCHBOX_BUILD_TESTS)
    add_subdirectory(tests)
endif ()
