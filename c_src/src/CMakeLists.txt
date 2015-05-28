##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

set(SOURCES
    tlsApplication.cpp
    tlsSocket.cpp)

add_library(erlang_tls_obj OBJECT ${SOURCES})
set_target_properties(erlang_tls_obj PROPERTIES
    POSITION_INDEPENDENT_CODE True
    COMPILE_OPTIONS "$<TARGET_PROPERTY:compile_options,INTERFACE_COMPILE_OPTIONS>"
    COMPILE_DEFINITIONS "$<TARGET_PROPERTY:compile_options,INTERFACE_COMPILE_DEFINITIONS>")

target_include_directories(erlang_tls_obj SYSTEM PRIVATE
    $<TARGET_PROPERTY:common_libraries,INTERFACE_SYSTEM_INCLUDE_DIRECTORIES>)

add_library(erlang_tls SHARED wrapper.cpp $<TARGET_OBJECTS:erlang_tls_obj>)
target_link_libraries(erlang_tls PRIVATE compile_options common_libraries)

install(TARGETS erlang_tls DESTINATION .)
