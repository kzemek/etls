##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

set(ERLANG_TLS_SYSTEM_INCLUDE_DIRS
    ${BORINGSSL_INCLUDE_DIRS}
    ${ASIO_INCLUDE_DIRS})

add_library(erlang_tls_obj OBJECT
    callback.hpp
    detail.cpp
    tlsAcceptor.cpp
    tlsApplication.cpp
    tlsSocket.cpp)

target_include_directories(erlang_tls_obj SYSTEM PRIVATE
    ${ERLANG_TLS_SYSTEM_INCLUDE_DIRS})

set(ERLANG_TLS_SYSTEM_INCLUDE_DIRS
    ${ERLANG_TLS_SYSTEM_INCLUDE_DIRS}
    PARENT_SCOPE)

set(ERLANG_TLS_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}
    PARENT_SCOPE)

set(ERLANG_TLS_LIBRARIES
    ${CMAKE_THREAD_LIBS_INIT}
    ${BORINGSSL_LIBRARIES}
    PARENT_SCOPE)

set(PROJECT_SOURCES
    $<TARGET_OBJECTS:erlang_tls_obj>
    ${PROJECT_SOURCES}
    PARENT_SCOPE)
