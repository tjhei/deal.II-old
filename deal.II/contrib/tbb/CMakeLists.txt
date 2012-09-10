ADD_CUSTOM_COMMAND(
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}
  COMMAND ${CMAKE_COMMAND} -E copy_directory
            ${CMAKE_CURRENT_SOURCE_DIR}/${libtbb_directory}
            ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}
  )

ADD_CUSTOM_COMMAND(
  #
  # This makes me very, very sad:
  #
  OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}/build
  COMMAND "make"
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}
  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}
  )

ADD_CUSTOM_TARGET(tbb ALL
  DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}
          ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}/build
  )

ADD_DEPENDENCIES(deal_ii_target_dependencies tbb)

INSTALL(
  DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/${libtbb_directory}/include/tbb
  DESTINATION include
  )

INSTALL(
  CODE "FILE(GLOB libtbb_so_files ${CMAKE_CURRENT_BINARY_DIR}/${libtbb_directory}/build/*/*.so*)"
  CODE "FILE(INSTALL \${libtbb_so_files} DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)" #TODO
  )
