include_directories(${openhipify_SOURCE_DIR}/includes)

add_clang_executable(openhipify
  main.cpp
  reporting/Defs.cpp
  args/Arguments.cpp
  )

target_link_libraries(openhipify
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
