##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

set(SOURCES
    tlsApplication.cpp
    tlsSocket.cpp
    wrapper.cpp)

add_library(erlang_tls SHARED ${SOURCES})
target_compile_options(erlang_tls PRIVATE -std=c++14 -Wall -Wpedantic)
