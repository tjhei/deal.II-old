#####
##
## Copyright (C) 2012, 2013 by the deal.II authors
##
## This file is part of the deal.II library.
##
## <TODO: Full License information>
## This file is dual licensed under QPL 1.0 and LGPL 2.1 or any later
## version of the LGPL license.
##
## Author: Matthias Maier <matthias.maier@iwr.uni-heidelberg.de>
##
#####

#
# Configuration for the netcdf library:
#

MACRO(FEATURE_METIS_FIND_EXTERNAL var)
  FIND_PACKAGE(METIS)

  IF(METIS_FOUND)
    IF(METIS_MAJOR GREATER 4)
      SET(${var} TRUE)
    ELSE()
      MESSAGE(WARNING "\n"
        )
      SET(METIS_ADDITIONAL_WARNING_STRING
        "Could not find a sufficient modern metis installation: "
        "Version 5.x required!\n"
        )
      MESSAGE(WARNING "\n" ${METIS_ADDITIONAL_WARNING_STRING} "\n")

      UNSET(METIS_LIBRARY CACHE)
      UNSET(METIS_INCLUDE_DIR CACHE)
      SET(METIS_DIR "" CACHE STRING
        "An optional hint to a metis directory"
        )
    ENDIF()
  ENDIF()
ENDMACRO()

CONFIGURE_FEATURE(METIS)
