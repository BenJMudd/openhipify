include_directories(${openhipify_SOURCE_DIR}/includes)

add_clang_executable(openhipify
  utils/Defs.cpp
  args/Arguments.cpp
  utils/PathUtils.cpp
  main.cpp
  )

target_link_libraries(openhipify
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
