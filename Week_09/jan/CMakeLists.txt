cmake_minimum_required(VERSION 2.6)

project(polyparser_test)

#set(CMAKE_CXX_FLAGS, "$(CMAKE_CXX_FLAGS) --std=c++11")
set (CMAKE_CXX_STANDARD 14)

# Locate GTest
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

# Link runTests with what we want to test and the GTest and pthread library
add_executable(
    runTests
    main.cpp
    src/func_constant.cpp
    src/factory.cpp
    tests/func_constant.cpp
)

target_link_libraries(runTests ${GTEST_LIBRARIES} pthread)