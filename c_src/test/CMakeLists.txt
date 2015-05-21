##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

add_subdirectory(gmock)
enable_testing()

file(COPY server.pem DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

set(TESTS
    tlsSocket_test.cpp)

foreach(TEST_FILE ${TESTS})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_FILE} testServer.hpp)
    target_include_directories(${TEST_NAME} PRIVATE
        gmock/include
        gtest/include)
    target_link_libraries(${TEST_NAME} PRIVATE
        gtest_main
        gmock
        compile_options
        common_libraries)

    add_test(${TEST_NAME} ${TEST_NAME})
endforeach()
