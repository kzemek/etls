##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'
#

add_subdirectory(gmock)

file(COPY server.pem server.key DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/dummy.cpp)

add_library(erlang_tls_test SHARED
    ${CMAKE_CURRENT_BINARY_DIR}/dummy.cpp $<TARGET_OBJECTS:erlang_tls_obj>)
target_link_libraries(erlang_tls_test PRIVATE compile_options common_libraries)

set(TESTS
    tlsAcceptor_test.cpp
    tlsSocket_test.cpp)

foreach(TEST_FILE ${TESTS})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_FILE} testServer.hpp testUtils.hpp)
    target_include_directories(${TEST_NAME} PRIVATE
	${PROJECT_SOURCE_DIR}/src)
    target_include_directories(${TEST_NAME} PRIVATE SYSTEM
	gmock/include
	gtest/include)
    target_link_libraries(${TEST_NAME} PRIVATE
	erlang_tls_test
	gtest_main
	gmock
	compile_options
	common_libraries)

    add_test(${TEST_NAME} ${TEST_NAME})
endforeach()

add_executable(bandwidthCap bandwidthCap.cpp)
target_link_libraries(bandwidthCap PRIVATE compile_options common_libraries)
