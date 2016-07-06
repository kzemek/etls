##
# Author: Konrad Zemek
# Copyright (C) 2015 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.md'
#

add_subdirectory(${PROJECT_SOURCE_DIR}/deps/gmock
    ${PROJECT_BINARY_DIR}/deps/gmock)

file(COPY server.pem server.key DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(WRITE ${CMAKE_CURRENT_BINARY_DIR}/dummy.cpp)

add_library(etls_test STATIC
    ${CMAKE_CURRENT_BINARY_DIR}/dummy.cpp
    ${PROJECT_SOURCES})
target_link_libraries(etls_test PUBLIC ${ETLS_LIBRARIES})
target_include_directories(etls_test SYSTEM PUBLIC ${ETLS_SYSTEM_INCLUDE_DIRS})
target_include_directories(etls_test PUBLIC ${ETLS_INCLUDE_DIRS})

set(TESTS
    tlsAcceptor_test.cpp
    tlsSocket_test.cpp)

add_custom_target(test_compile)

foreach(TEST_FILE ${TESTS})
    get_filename_component(TEST_NAME ${TEST_FILE} NAME_WE)
    add_executable(${TEST_NAME} ${TEST_FILE} testServer.hpp testUtils.hpp)
    target_include_directories(${TEST_NAME} PRIVATE
        ${PROJECT_SOURCE_DIR}/src)
    target_include_directories(${TEST_NAME} PRIVATE SYSTEM
        ${PROJECT_SOURCE_DIR}/deps/gtest/include)
    target_link_libraries(${TEST_NAME} PRIVATE
        etls_test
        gtest_main)

    add_test(${TEST_NAME} ${TEST_NAME})
    add_dependencies(test_compile ${TEST_NAME})
endforeach()

add_executable(bandwidthCap bandwidthCap.cpp)
target_link_libraries(bandwidthCap PRIVATE compile_options common_libraries)
