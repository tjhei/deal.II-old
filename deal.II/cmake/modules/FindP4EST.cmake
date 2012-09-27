#
# Try to find the P4EST library
#

INCLUDE(FindPackageHandleStandardArgs)

SET_IF_EMPTY(P4EST_DIR "$ENV{P4EST_DIR}")

FIND_PATH(P4EST_INCLUDE_DIR p4est.h
  HINTS
    ${P4EST_DIR}
  PATH_SUFFIXES
    p4est include/p4est include src p4est/src
  )

FIND_LIBRARY(P4EST_LIBRARY
  NAMES p4est
  HINTS
    ${P4EST_DIR}
  PATH_SUFFIXES
    lib${LIB_SUFFIX} lib64 lib src
  )

FIND_PACKAGE_HANDLE_STANDARD_ARGS(P4EST DEFAULT_MSG P4EST_LIBRARY P4EST_INCLUDE_DIR)

IF(P4EST_FOUND)
  MARK_AS_ADVANCED(
    P4EST_LIBRARY
    P4EST_INCLUDE_DIR
    P4EST_DIR
  )
ELSE()
  SET(P4EST_DIR "" CACHE STRING
    "An optional hint to a p4est installation/directory"
    )
ENDIF()

