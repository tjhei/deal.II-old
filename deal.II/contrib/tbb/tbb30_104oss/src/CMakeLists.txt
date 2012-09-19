#
# TODO: Check whether we do the right thing on all targets.
#

INCLUDE_DIRECTORIES(
  ${CMAKE_CURRENT_SOURCE_DIR}/rml/include
  )

FILE(WRITE "${CMAKE_CURRENT_BINARY_DIR}/version_string.tmp"
  "#define __TBB_VERSION_STRINGS \"Empty\""
  )

SET(src_tbb
  rml/client/omp_dynamic_link.cpp
  rml/client/rml_omp.cpp
  rml/client/rml_tbb.cpp
  rml/server/rml_server.cpp
  tbb/arena.cpp
  tbb/cache_aligned_allocator.cpp
  tbb/concurrent_hash_map.cpp
  tbb/concurrent_monitor.cpp
  tbb/concurrent_queue.cpp
  tbb/concurrent_vector.cpp
  tbb/condition_variable.cpp
  tbb/critical_section.cpp
  tbb/dynamic_link.cpp
  tbb/governor.cpp
  tbb/itt_notify.cpp
  tbb/market.cpp
  tbb/mutex.cpp
  tbb/observer_proxy.cpp
  tbb/pipeline.cpp
  tbb/private_server.cpp
  tbb/queuing_mutex.cpp
  tbb/queuing_rw_mutex.cpp
  tbb/reader_writer_lock.cpp
  tbb/recursive_mutex.cpp
  tbb/scheduler.cpp
  tbb/spin_mutex.cpp
  tbb/spin_rw_mutex.cpp
  tbb/task.cpp
  tbb/task_group_context.cpp
  tbb/tbb_main.cpp
  tbb/tbb_misc.cpp
  tbb/tbb_statistics.cpp
  tbb/tbb_thread.cpp
  )

ADD_LIBRARY(obj_tbb OBJECT ${src_tbb})


#
# Add necessary definitions:
#
IF(NOT CMAKE_SYSTEM_NAME MATCHES "Darwin")
  ADD_FLAGS(obj_tbb_COMPILE_FLAGS "-DDO_ITT_NOTIFY")
ENDIF()

IF(CMAKE_BUILD_TYPE MATCHES "Debug")
  ADD_FLAGS(obj_tbb_COMPILE_FLAGS "-DTBB_USE_DEBUG")
ENDIF()

IF(DEAL_II_USE_MT_POSIX)
  ADD_FLAGS(obj_tbb_COMPILE_FLAGS "-DUSE_PTHREAD")
ENDIF()

IF(CMAKE_SYSTEM_NAME MATCHES "Windows")
  ADD_FLAGS(obj_tbb_COMPILE_FLAGS "-DUSE_WINTHREAD")
ENDIF()


#
# We have to disable a bunch of warnings:
#
ENABLE_IF_SUPPORTED(obj_tbb_COMPILE_FLAGS "-Wno-parentheses")
ENABLE_IF_SUPPORTED(obj_tbb_COMPILE_FLAGS "-Wno-long-long")


SET_TARGET_PROPERTIES(obj_tbb PROPERTIES
  COMPILE_FLAGS ${obj_tbb_COMPILE_FLAGS})

