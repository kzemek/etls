cmake_minimum_required(VERSION 3.0)

message(STATUS "Detecting ERTS include directory")
execute_process(
  COMMAND erl -noshell -s init stop -eval "io:format(\"~s/erts-~s/include/\", [code:root_dir(), erlang:system_info(version)])."
  OUTPUT_VARIABLE ERTS_INCLUDE_DIR)
message(STATUS "Detecting ERTS include directory -- ${ERTS_INCLUDE_DIR}")

message(STATUS "Detecting erl_interface include directory")
execute_process(
  COMMAND erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, include)])."
  OUTPUT_VARIABLE ERL_INTERFACE_INCLUDE_DIR)
message(STATUS "Detecting erl_interface include directory -- ${ERL_INTERFACE_INCLUDE_DIR}")

message(STATUS "Detecting erl_interface lib directory")
execute_process(
  COMMAND erl -noshell -s init stop -eval "io:format(\"~s\", [code:lib_dir(erl_interface, lib)])."
  OUTPUT_VARIABLE ERL_INTERFACE_LIB_DIR)
message(STATUS "Detecting erl_interface lib directory -- ${ERL_INTERFACE_LIB_DIR}")

set(BORINGSSL_INCLUDE_DIR ${CMAKE_SOURCE_DIR}/../boringssl/include)
set(BORINGSSL_BUILD_DIR ${CMAKE_SOURCE_DIR}/../_build/boringssl/)

link_directories(${ERL_INTERFACE_LIB_DIR})
include_directories(SYSTEM
  ${ERTS_INCLUDE_DIR}
  ${ERL_INTERFACE_INCLUDE_DIR}
  ${BORINGSSL_INCLUDE_DIR})

add_compile_options(-std=c11 -Wall -Wshadow -pedantic)
add_library(etls_nif SHARED etls_nif.c)
set_target_properties(etls_nif PROPERTIES SUFFIX .so)
target_link_libraries(etls_nif PRIVATE
  ${BORINGSSL_BUILD_DIR}/ssl/libssl.a
  ${BORINGSSL_BUILD_DIR}/crypto/libcrypto.a
  -lerl_interface
  -lei)
