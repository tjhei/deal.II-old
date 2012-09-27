#
# Try to find the petsc library
#
# This module exports:
#
#     PETSC_FOUND
#     PETSC_LIBRARIES
#     PETSC_INCLUDE_DIRS
#     PETSC_VERSION
#     PETSC_VERSION_MAJOR
#     PETSC_VERSION_MINOR
#     PETSC_VERSION_SUBMINOR
#     PETSC_VERSION_PATCH
#     PETSC_RELEASE
#     PETSC_COMPLEX
#

INCLUDE(FindPackageHandleStandardArgs)

SET_IF_EMPTY(PETSC_DIR "$ENV{PETSC_DIR}")
SET_IF_EMPTY(PETSC_ARCH "$ENV{PETSC_ARCH}")

#
# So, well, yes. I'd like to include the PETScConfig.cmake file via
# FIND_PACKAGE(), but it is broken beyond belief:
#
# - In source, i.e. PETSC_DIR/PETSC_ARCH, it sets BUILD_SHARED_LIBS.
# - It does not contain its very own version number
# - It does not contain its very own library location(s) or name(s)
# - It does not contain necessary includes
#
# - It writes a lot of FIND_LIBRARY(..) statements. Seriously. What the
#   heck? If its not the same library you're linking against, you cannot
#   assume to be API compatible, so why not just give a list of libraries?
#

#
# TODO: We'll have to guess which external libraries we'll have to link
# against someday...
#

#
# Search for the first part of the includes:
#
FIND_PATH(PETSC_INCLUDE_DIR_ARCH petscconf.h
  HINTS
    # petsc is special. Account for that
    ${PETSC_DIR}/${PETSC_ARCH}/include
    ${PETSC_INCLUDE_DIRS}
  PATH_SUFFIXES petsc
)

FIND_LIBRARY(PETSC_LIBRARIES
  NAMES petsc
  HINTS
    # petsc is special. Account for that
    ${PETSC_DIR}/${PETSC_ARCH}
  PATH_SUFFIXES lib${LIB_SUFFIX} lib64 lib
)


#
# So, up to this point it was easy. Now, the tricky part.
# If petsc is not installed, but a source tree is given by PETSC_DIR petsc
# splits up its include files to two places:
#   ${PETSC_DIR}/include
#   ${PETSC_DIR}/${PETSC_ARCH}/include
#
IF("${PETSC_INCLUDE_DIR_ARCH}" STREQUAL "${PETSC_DIR}/${PETSC_ARCH}/include")
  #
  # We found petsc and we used PETSC_DIR and PETSC_ARCH for finding it.
  # So, we assume a source tree layout:
  #
  IF( PETSC_INCLUDE_DIRS MATCHES "-NOTFOUND" OR
      "${PETSC_INCLUDE_DIRS}" STREQUAL "" )
    SET(PETSC_INCLUDE_DIRS "${PETSC_INCLUDE_DIR_ARCH};${PETSC_DIR}/include"
      CACHE STRING "Include paths for petsc"
      FORCE
      )
  ENDIF()
ELSE()
  IF( PETSC_INCLUDE_DIRS MATCHES "-NOTFOUND" OR
      "${PETSC_INCLUDE_DIRS}" STREQUAL "" )
    SET(PETSC_INCLUDE_DIRS "${PETSC_INCLUDE_DIR_ARCH}"
      CACHE STRING "Include paths for petsc"
      FORCE
      )
  ENDIF()
ENDIF()


#
# Let's see whether we are able to find petscversion.h and petscconf.h:
#
FIND_FILE(PETSC_PETSCVERSION_H petscversion.h
  HINTS
    ${PETSC_INCLUDE_DIRS}
    NO_DEFAULT_PATH
    NO_CMAKE_ENVIRONMENT_PATH
    NO_CMAKE_PATH
    NO_SYSTEM_ENVIRONMENT_PATH
    NO_CMAKE_SYSTEM_PATH
    NO_CMAKE_FIND_ROOT_PATH
  )

FIND_FILE(PETSC_PETSCCONF_H petscconf.h
  HINTS
    ${PETSC_INCLUDE_DIRS}
    NO_DEFAULT_PATH
    NO_CMAKE_ENVIRONMENT_PATH
    NO_CMAKE_PATH
    NO_SYSTEM_ENVIRONMENT_PATH
    NO_CMAKE_SYSTEM_PATH
    NO_CMAKE_FIND_ROOT_PATH
  )

#
# If no, PETSC_INCLUDE_DIRS is invalid, so reset it to NOTFOUND:
#
IF( PETSC_PETSCCONF_H MATCHES "-NOTFOUND" OR
    PETSC_PETSCVERSION_H MATCHES "-NOTFOUND" )
  SET(PETSC_INCLUDE_DIRS "PETSC_INCLUDE_DIRS-NOTFOUND"
    CACHE STRING "Include paths for petsc"
    FORCE
    )
ENDIF()

FIND_PACKAGE_HANDLE_STANDARD_ARGS(PETSC DEFAULT_MSG
  PETSC_LIBRARIES
  PETSC_INCLUDE_DIRS
  )


IF(PETSC_FOUND)

  #
  # Is petsc compiled with support for MPIUNI?
  #
  FILE(STRINGS "${PETSC_PETSCCONF_H}" PETSC_MPIUNI_STRING
    REGEX "#define.*PETSC_HAVE_MPIUNI 1")
  IF("${PETSC_MPIUNI_STRING}" STREQUAL "")
    SET(PETSC_WITH_MPIUNI FALSE)
  ELSE()
    SET(PETSC_WITH_MPIUNI TRUE)
  ENDIF()

  IF(PETSC_WITH_MPIUNI)
    #
    # If yes, add libmpiuni.so/a (if available)
    # We need to link with it on some systems where PETSc is built without
    # a real MPI and we need to handle trivial (one process) MPI
    # functionality.
    #
    FIND_LIBRARY(PETSC_LIBMPIUNI
      NAMES mpiuni
      HINTS
        ${PETSC_DIR}/${PETSC_ARCH}
      PATH_SUFFIXES lib${LIB_SUFFIX} lib64 lib
      )
    IF(NOT PETSC_LIBMPIUNI MATCHES "-NOTFOUND")
      LIST(APPEND PETSC_LIBRARIES "${PETSC_LIBMPIUNI}")
    ELSE()
      SET(PETSC_LIBMPIUNI "")
    ENDIF()
    MARK_AS_ADVANCED(PETSC_LIBMPIUNI)
  ENDIF()


  FILE(STRINGS "${PETSC_PETSCVERSION_H}" PETSC_VERSION_MAJOR_STRING
    REGEX "#define.*PETSC_VERSION_MAJOR")
  STRING(REGEX REPLACE "^.*PETSC_VERSION_MAJOR.*([0-9]+).*" "\\1"
    PETSC_VERSION_MAJOR "${PETSC_VERSION_MAJOR_STRING}"
    )

  FILE(STRINGS "${PETSC_PETSCVERSION_H}" PETSC_VERSION_MINOR_STRING
    REGEX "#define.*PETSC_VERSION_MINOR")
  STRING(REGEX REPLACE "^.*PETSC_VERSION_MINOR.*([0-9]+).*" "\\1"
    PETSC_VERSION_MINOR "${PETSC_VERSION_MINOR_STRING}"
    )

  FILE(STRINGS "${PETSC_PETSCVERSION_H}" PETSC_VERSION_SUBMINOR_STRING
    REGEX "#define.*PETSC_VERSION_SUBMINOR")
  STRING(REGEX REPLACE "^.*PETSC_VERSION_SUBMINOR.*([0-9]+).*" "\\1"
    PETSC_VERSION_SUBMINOR "${PETSC_VERSION_SUBMINOR_STRING}"
    )

  FILE(STRINGS "${PETSC_PETSCVERSION_H}" PETSC_VERSION_PATCH_STRING
    REGEX "#define.*PETSC_VERSION_PATCH")
  STRING(REGEX REPLACE "^.*PETSC_VERSION_PATCH.*([0-9]+).*" "\\1"
    PETSC_VERSION_PATCH "${PETSC_VERSION_PATCH_STRING}"
    )

  SET(PETSC_VERSION "${PETSC_VERSION_MAJOR}.${PETSC_VERSION_MINOR}.${PETSC_VERSION_SUBMINOR}")

  FILE(STRINGS "${PETSC_PETSCVERSION_H}" PETSC_RELEASE_STRING
    REGEX "#define.*PETSC_VERSION_RELEASE.*1")
  IF("${PETSC_RELEASE_STRING}" STREQUAL "")
    SET(PETSC_RELEASE FALSE)
  ELSE()
    SET(PETSC_RELEASE TRUE)
  ENDIF()

  FILE(STRINGS "${PETSC_PETSCCONF_H}" PETSC_COMPLEX_STRING
    REGEX "#define.*PETSC_USE_COMPLEX.*1")
  IF("${PETSC_COMPLEX_STRING}" STREQUAL "")
    SET(PETSC_COMPLEX FALSE)
  ELSE()
    SET(PETSC_COMPLEX TRUE)
  ENDIF()

  MARK_AS_ADVANCED(
    PETSC_LIBRARIES
    PETSC_INCLUDE_DIRS
    PETSC_DIR
    PETSC_ARCH
  )
ELSE()
  SET(PETSC_DIR "" CACHE STRING
    "An optional hint to a PETSc directory"
    )
  SET(PETSC_ARCH "" CACHE STRING
    "An optional hint to a PETSc arch"
    )
ENDIF()

#
# Unset a bunch of cached variables.
# Only PETSC_INCLUDE_DIRS should remain cached
#
UNSET(PETSC_INCLUDE_DIR_ARCH CACHE)
UNSET(PETSC_PETSCCONF_H CACHE)
UNSET(PETSC_PETSCVERSION_H CACHE)

