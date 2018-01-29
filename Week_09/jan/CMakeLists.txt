cmake_minimum_required(VERSION 3.0  )

project(polyparser_test)

#set(CMAKE_CXX_FLAGS, "$(CMAKE_CXX_FLAGS) --std=c++11")
set (CMAKE_CXX_STANDARD 14)

# Locate GTest
find_package(GTest REQUIRED)
include_directories(${GTEST_INCLUDE_DIRS})

# Link polyparser with what we want to test and the GTest and pthread library
add_executable(
    polyparser
    main.cpp
    src/derivable.cpp
    
    src/func_constant.cpp
    src/func_x.cpp
    src/func_monom.cpp
    src/factory.cpp

    tests/func_constant.cpp
    tests/func_x.cpp
    tests/func_monom.cpp
)

target_link_libraries(polyparser ${GTEST_LIBRARIES} pthread)