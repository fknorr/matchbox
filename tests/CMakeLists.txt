find_package(Catch2 REQUIRED)
add_executable(test test.cc)
target_link_libraries(test PRIVATE Catch2::Catch2 matchbox)
