##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

cmake_minimum_required(VERSION 3.0.0)
project(erlang_tls C CXX)

find_package(Threads REQUIRED)
find_package(OpenSSL REQUIRED)

set(Boost_USE_STATIC_LIBS    OFF)
set(Boost_USE_MULTITHREADED   ON)
set(Boost_USE_STATIC_RUNTIME OFF)
find_package(Boost COMPONENTS coroutine system REQUIRED)

add_library(compile_options INTERFACE)
target_compile_options(compile_options INTERFACE -std=c++14 -Wall -Wpedantic)

add_library(common_libraries INTERFACE)
target_link_libraries(common_libraries INTERFACE
    ${CMAKE_THREAD_LIBS_INIT}
    ${OPENSSL_LIBRARIES}
    ${Boost_LIBRARIES})
target_include_directories(common_libraries INTERFACE
    ${OPENSSL_INCLUDE_DIR}
    ${Boost_INCLUDE_DIRS})

add_subdirectory(src)
add_subdirectory(test) # TODO: EXCLUDE_FROM_ALL
