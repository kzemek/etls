#
# BSD LICENSE
#
# Copyright (c) 2009-2011, Michael Truog
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#      * Redistributions of source code must retain the above copyright
#        notice, this list of conditions and the following disclaimer.
#      * Redistributions in binary form must reproduce the above copyright
#        notice, this list of conditions and the following disclaimer in
#        the documentation and/or other materials provided with the
#        distribution.
#      * All advertising materials mentioning features or use of this
#        software must display the following acknowledgment:
#          This product includes software developed by Michael Truog
#      * The name of the author may not be used to endorse or promote
#        products derived from this software without specific prior
#        written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
# 

# - Find Erlang
# This module finds if Erlang is installed and determines where the
# include files and libraries are. This code sets the following
# variables:
#
#  ERLANG_RUNTIME    = the full path to the Erlang runtime
#  ERLANG_COMPILE    = the full path to the Erlang compiler
#  ERLANG_EI_PATH    = the full path to the Erlang erl_interface path
#  ERLANG_ERTS_PATH    = the full path to the Erlang erts path
#  ERLANG_EI_INCLUDE_PATH = /include appended to ERLANG_EI_PATH
#  ERLANG_EI_LIBRARY_PATH = /lib appended to ERLANG_EI_PATH
#  ERLANG_ERTS_INCLUDE_PATH = /include appended to ERLANG_ERTS_PATH
#  ERLANG_ERTS_LIBRARY_PATH = /lib appended to ERLANG_ERTS_PATH
#

SET(ERLANG_BIN_PATH
  $ENV{ERLANG_HOME}/bin
  /usr/bin
  /usr/local/bin
  /opt/local/bin
  /sw/bin
  )
FIND_PROGRAM(ERLANG_RUNTIME
  NAMES erl
  PATHS ${ERLANG_BIN_PATH}
)

FIND_PROGRAM(ERLANG_COMPILE
  NAMES erlc
  PATHS ${ERLANG_BIN_PATH}
)

EXECUTE_PROCESS(COMMAND
         erl -noshell -eval "io:format(\"~s\", [code:lib_dir()])" -s erlang halt
         OUTPUT_VARIABLE ERLANG_OTP_LIB_DIR)

EXECUTE_PROCESS(COMMAND
         erl -noshell -eval "io:format(\"~s\", [code:root_dir()])" -s erlang halt
         OUTPUT_VARIABLE ERLANG_OTP_ROOT_DIR)

MESSAGE(STATUS "Using OTP lib: ${ERLANG_OTP_LIB_DIR} - found")

EXECUTE_PROCESS(COMMAND ls ${ERLANG_OTP_LIB_DIR}
                COMMAND grep erl_interface
                COMMAND sort -n
                COMMAND tail -1
                COMMAND tr -d \n
                OUTPUT_VARIABLE ERLANG_EI_DIR)

EXECUTE_PROCESS(COMMAND ls ${ERLANG_OTP_ROOT_DIR}
                COMMAND grep erts
                COMMAND sort -n
                COMMAND tail -1
                COMMAND tr -d \n
                OUTPUT_VARIABLE ERLANG_ERTS_DIR)

MESSAGE(STATUS "Using erl_interface version: ${ERLANG_EI_DIR}")
MESSAGE(STATUS "Using erts version: ${ERLANG_ERTS_DIR}")

SET(ERLANG_EI_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR})
SET(ERLANG_EI_INCLUDE_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR}/include)
SET(ERLANG_EI_LIBRARY_PATH ${ERLANG_OTP_LIB_DIR}/${ERLANG_EI_DIR}/lib)

SET(ERLANG_ERTS_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR})
SET(ERLANG_ERTS_INCLUDE_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR}/include)
SET(ERLANG_ERTS_LIBRARY_PATH ${ERLANG_OTP_ROOT_DIR}/${ERLANG_ERTS_DIR}/lib)

FIND_PROGRAM(ERLANG_ARCHIVE
  NAMES zip
  PATHS ${ERLANG_BIN_PATH}
)
MARK_AS_ADVANCED(
ERLANG_RUNTIME
ERLANG_COMPILE
ERLANG_ARCHIVE
ERLANG_EI_PATH
ERLANG_EI_INCLUDE_PATH
ERLANG_EI_LIBRARY_PATH
)