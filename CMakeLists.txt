set(LLVM_LINK_COMPONENTS support)

add_clang_executable(openhipify
  src/main.cpp
  )
target_link_libraries(openhipify
  PRIVATE
  clangTooling
  clangBasic
  clangASTMatchers
  )
