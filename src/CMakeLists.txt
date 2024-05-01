include_directories(${openhipify_SOURCE_DIR}/includes)

add_clang_executable(openhipify
  defs/OpenhipifyDefs.cpp
  defs/HIPDefs.cpp
  defs/OpenCLDefs.cpp
  Arguments.cpp
  OpenHipifyKernelFA.cpp
  OpenHipifyHostFA.cpp
  OpenHipifyFA.cpp
  KernelTracking.cpp
  main.cpp
  )

target_link_libraries(openhipify
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
