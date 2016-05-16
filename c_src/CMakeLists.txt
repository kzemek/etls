##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

cmake_minimum_required(VERSION 3.0.0)
project(erlang_tls C CXX)

set(CMAKE_SKIP_INSTALL_ALL_DEPENDENCY True)
set(CMAKE_POSITION_INDEPENDENT_CODE True)
set(CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)

# Setup Threads
find_package(Threads REQUIRED)

# Setup Erlang
find_package(Erlang REQUIRED)

# Setup BoringSSL
add_subdirectory(deps/boringssl)
set(BORINGSSL_LIBRARIES ssl crypto)
set(BORINGSSL_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/deps/boringssl/include)

# Setup Asio
set(ASIO_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR}/deps/asio/asio/include)

if(APPLE)
    set(CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS
        "${CMAKE_SHARED_LIBRARY_CREATE_CXX_FLAGS} -flat_namespace -undefined dynamic_lookup")
endif(APPLE)

add_compile_options(-Wall -Wshadow -Wpedantic)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++14")
add_definitions(-DASIO_STANDALONE)

enable_testing()
add_subdirectory(src)
add_subdirectory(test EXCLUDE_FROM_ALL)

add_library(erlang_tls SHARED src/wrapper.cpp ${PROJECT_SOURCES})
target_link_libraries(erlang_tls PRIVATE
    ${ERLANG_TLS_LIBRARIES})
target_include_directories(erlang_tls SYSTEM PRIVATE
    ${ERLANG_ERTS_INCLUDE_PATH}
    ${ERLANG_TLS_SYSTEM_INCLUDE_DIRS})
target_include_directories(erlang_tls PRIVATE
    ${ERLANG_TLS_INCLUDE_DIRS})
set_target_properties(erlang_tls PROPERTIES SUFFIX .so)

install(TARGETS erlang_tls DESTINATION .)
