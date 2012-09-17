# Try to find P4EST

INCLUDE(FindPackageHandleStandardArgs)

FIND_PATH(P4EST_INCLUDE_DIR p4est.h)

FIND_LIBRARY(P4EST_LIBRARY
  NAMES p4est
  PATH_SUFFIXES lib64 lib
)

FIND_LIBRARY(P4EST_DEBUG_LIBRARY
  NAMES p4est_debug p4est.g
  PATH_SUFFIXES lib64 lib
)

FIND_PACKAGE_HANDLE_STANDARD_ARGS(P4EST DEFAULT_MSG P4EST_LIBRARY P4EST_INCLUDE_DIR)
FIND_PACKAGE_HANDLE_STANDARD_ARGS(P4EST_DEBUG DEFAULT_MSG P4EST_DEBUG_LIBRARY P4EST_INCLUDE_DIR)

IF(P4EST_FOUND)
  MARK_AS_ADVANCED(
    P4EST_LIBRARY
    P4EST_DEBUG_LIBRARY
    P4EST_INCLUDE_DIR
  )
ENDIF()
