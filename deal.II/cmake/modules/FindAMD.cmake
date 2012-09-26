#
# Try to find the AMD library
#

INCLUDE(FindPackageHandleStandardArgs)

SET_IF_EMPTY(AMD_DIR "$ENV{AMD_DIR}")
SET_IF_EMPTY(UMFPACK_DIR "$ENV{UMFPACK_DIR}")

FIND_PATH(AMD_INCLUDE_DIR amd.h
  HINTS
    ${AMD_DIR}
    ${UMFPACK_DIR}
  PATH_SUFFIXES
    amd include/amd include Include AMD/Include ../AMD/Include
)

FIND_LIBRARY(AMD_LIBRARY
  NAMES amd
  HINTS
    ${AMD_DIR}
    ${UMFPACK_DIR}
  PATH_SUFFIXES
    lib${LIB_SUFFIX} lib64 lib Lib AMD/Lib ../AMD/Lib
  )

FIND_PACKAGE_HANDLE_STANDARD_ARGS(AMD DEFAULT_MSG AMD_LIBRARY AMD_INCLUDE_DIR)

IF(AMD_FOUND)
  MARK_AS_ADVANCED(
    AMD_LIBRARY
    AMD_INCLUDE_DIR
    AMD_DIR
  )
ELSE()
  SET(AMD_DIR "" CACHE STRING
    "An optional hint to an AMD directory"
    )
ENDIF()
