##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.md'
#

set(ETLS_SYSTEM_INCLUDE_DIRS
    ${BORINGSSL_INCLUDE_DIRS}
    ${ASIO_INCLUDE_DIRS})

add_library(etls_obj OBJECT
    callback.hpp
    detail.cpp
    tlsAcceptor.cpp
    tlsApplication.cpp
    tlsSocket.cpp)

target_include_directories(etls_obj SYSTEM PRIVATE
    ${ETLS_SYSTEM_INCLUDE_DIRS})

set(ETLS_SYSTEM_INCLUDE_DIRS
    ${ETLS_SYSTEM_INCLUDE_DIRS}
    PARENT_SCOPE)

set(ETLS_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}
    PARENT_SCOPE)

set(ETLS_LIBRARIES
    ${CMAKE_THREAD_LIBS_INIT}
    ${BORINGSSL_LIBRARIES}
    PARENT_SCOPE)

set(PROJECT_SOURCES
    $<TARGET_OBJECTS:etls_obj>
    ${PROJECT_SOURCES}
    PARENT_SCOPE)
