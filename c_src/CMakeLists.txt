cmake_minimum_required(VERSION 3.0.0)
project(erlang_tls C CXX)

set(SOURCES
    tlsApplication.cpp
    tlsSocket.cpp
    wrapper.cpp)

add_library(erlang_tls SHARED ${SOURCES})
target_compile_options(erlang_tls PRIVATE -std=c++14 -Wall -Wpedantic)

