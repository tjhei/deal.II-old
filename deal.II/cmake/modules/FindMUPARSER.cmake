## ---------------------------------------------------------------------
## $Id$
##
## Copyright (C) 2013 by the deal.II authors
##
## This file is part of the deal.II library.
##
## The deal.II library is free software; you can use it, redistribute
## it, and/or modify it under the terms of the GNU Lesser General
## Public License as published by the Free Software Foundation; either
## version 2.1 of the License, or (at your option) any later version.
## The full text of the license can be found in the file LICENSE at
## the top level of the deal.II distribution.
##
## ---------------------------------------------------------------------

#
# Try to find the MUPARSER library
#
# This module exports
#
#   MUPARSER_LIBRARIES
#   MUPARSER_INCLUDE_DIRS
#   MUPARSER_VERSION
#   MUPARSER_VERSION_MAJOR
#   MUPARSER_VERSION_MINOR
#   MUPARSER_VERSION_SUBMINOR
#

INCLUDE(FindPackageHandleStandardArgs)

SET(MUPARSER_DIR "" CACHE PATH "An optional hint to a MUPARSER installation")
SET_IF_EMPTY(MUPARSER_DIR "$ENV{MUPARSER_DIR}")

FIND_LIBRARY(MUPARSER_LIBRARY
  NAMES muparser muparserd
  HINTS ${MUPARSER_DIR}
  PATH_SUFFIXES lib${LIB_SUFFIX} lib64 lib
  )

FIND_PATH(MUPARSER_INCLUDE_DIR muParserDef.h
  HINTS ${MUPARSER_DIR}
  PATH_SUFFIXES include
  )

FIND_PACKAGE_HANDLE_STANDARD_ARGS(MUPARSER DEFAULT_MSG
  MUPARSER_LIBRARY
  MUPARSER_INCLUDE_DIR
  )

IF(MUPARSER_FOUND)
  SET(MUPARSER_LIBRARIES ${MUPARSER_LIBRARY})
  SET(MUPARSER_INCLUDE_DIRS ${MUPARSER_INCLUDE_DIR})

  FILE(STRINGS "${MUPARSER_INCLUDE_DIR}/muParserDef.h" MUPARSER_VERSION_STRING
    REGEX "#define MUP_VERSION _T"
    )
  STRING(REGEX REPLACE ".*\"([0-9]+)\\.[0-9]+\\..*" "\\1"
    MUPARSER_VERSION_MAJOR "${MUPARSER_VERSION_STRING}"
    )
  STRING(REGEX REPLACE ".*\\.([0-9]+)\\..*" "\\1"
    MUPARSER_VERSION_MINOR "${MUPARSER_VERSION_STRING}"
    )
  STRING(REGEX REPLACE ".*\\.[0-9]+\\.([0-9]+)\".*" "\\1"
    MUPARSER_VERSION_SUBMINOR "${MUPARSER_VERSION_STRING}"
    )
  SET(MUPARSER_VERSION
    "${MUPARSER_VERSION_MAJOR}.${MUPARSER_VERSION_MINOR}.${MUPARSER_VERSION_SUBMINOR}"
    )

  MARK_AS_ADVANCED(MUPARSER_DIR)
ENDIF()
