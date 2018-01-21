cmake_minimum_required(VERSION 3.8)
project(week7)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release)
endif()

set( CMAKE_EXPORT_COMPILE_COMMANDS ON )

set(CMAKE_CXX_STANDARD 17)

# include_directories("/Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1" "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include") 

set (PROJECT_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src)
set (HELLO_SOURCES ${PROJECT_SOURCE_DIR}/main.cpp )

# These are the corresponding output paths
set (EXECUTABLE_OUTPUT_PATH ${CMAKE_SOURCE_DIR}/bin)

if(CMAKE_BUILD_TYPE)
string(TOUPPER ${CMAKE_BUILD_TYPE} CMAKE_BUILD_TYPE)
IF(CMAKE_BUILD_TYPE MATCHES DEBUG)
    message("debug mode")
	add_definitions("-DTASDEBUG")
ENDIF(CMAKE_BUILD_TYPE MATCHES DEBUG)
endif(CMAKE_BUILD_TYPE)

add_executable (week7 ${HELLO_SOURCES})
find_library(Curl_LIBRARY Curl REQUIRED)
target_link_libraries (week7 ${Curl_LIBRARY} )


message("CMAKE_CXX_FLAGS_DEBUG is ${CMAKE_CXX_FLAGS_DEBUG}")
message("CMAKE_CXX_FLAGS_RELEASE is ${CMAKE_CXX_FLAGS_RELEASE}")

