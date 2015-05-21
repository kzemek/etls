##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

add_subdirectory(gmock)
enable_testing()

file(COPY server.pem DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

add_library(test_server OBJECT testServer.cpp)
target_include_directories(test_server SYSTEM PRIVATE
    gmock/include
    gtest/include)

set_target_properties(gmock gmock_main gtest gtest_main test_server PROPERTIES
    COMPILE_OPTIONS "$<TARGET_PROPERTY:compile_options,INTERFACE_COMPILE_OPTIONS>")
