##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.md'
#

cmake_minimum_required(VERSION 3.1.0)
project(etls C CXX)

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

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED On)
add_compile_options(-Wall -Wshadow -Wpedantic)
add_compile_options(-fvisibility=hidden -fvisibility-inlines-hidden)
add_definitions(-DASIO_STANDALONE)

enable_testing()
add_subdirectory(src)
add_subdirectory(test EXCLUDE_FROM_ALL)

add_library(etls SHARED src/wrapper.cpp ${PROJECT_SOURCES})
target_link_libraries(etls PRIVATE ${ETLS_LIBRARIES})
target_include_directories(etls SYSTEM PRIVATE
    ${ERLANG_ERTS_INCLUDE_PATH}
    ${ETLS_SYSTEM_INCLUDE_DIRS})
target_include_directories(etls PRIVATE
    ${ETLS_INCLUDE_DIRS})
set_target_properties(etls PROPERTIES SUFFIX .so)

install(TARGETS etls DESTINATION .)
