#
# Configuration for the netcdf library:
#

MACRO(FEATURE_NETCDF_FIND_EXTERNAL var)

  FIND_PACKAGE(NETCDF)

  IF(NETCDF_FOUND)
    SET(${var} TRUE)
  ENDIF()

ENDMACRO()


MACRO(FEATURE_NETCDF_CONFIGURE_EXTERNAL var)

  INCLUDE_DIRECTORIES(${NETCDF_INCLUDE_DIR})
  LIST(APPEND deal_ii_external_libraries ${NETCDF_LIBRARY})
  SET(HAVE_LIBNETCDF TRUE)

  SET(${var} TRUE)
ENDMACRO()


CONFIGURE_FEATURE(NETCDF)
