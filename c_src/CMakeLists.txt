##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

cmake_minimum_required(VERSION 3.0.0)
project(erlang_tls C CXX)

set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

find_package(Threads REQUIRED)
find_package(OpenSSL REQUIRED)
find_package(Erlang REQUIRED)
find_package(Boost REQUIRED)

add_library(compile_options INTERFACE)
target_compile_options(compile_options INTERFACE -std=c++14 -Wall -Wpedantic)
target_compile_definitions(compile_options INTERFACE
    -DBOOST_ERROR_CODE_HEADER_ONLY
    -DBOOST_SYSTEM_NO_DEPRECATED)

add_library(common_libraries INTERFACE)
target_link_libraries(common_libraries INTERFACE
    ${CMAKE_THREAD_LIBS_INIT}
    ${OPENSSL_LIBRARIES}
    ${Boost_LIBRARIES})
target_include_directories(common_libraries SYSTEM INTERFACE
    ${OPENSSL_INCLUDE_DIR}
    ${Boost_INCLUDE_DIRS}
    ${ERLANG_ERTS_INCLUDE_PATH})

enable_testing()
add_subdirectory(src)
add_subdirectory(test) # TODO: EXCLUDE_FROM_ALL

