#include "OpenHipifyFA.h"

void OpenHipifyFA::run(const ASTMatch::MatchFinder::MatchResult &res) {}

std::unique_ptr<clang::ASTConsumer>
OpenHipifyFA::CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);
  return m_finder->newASTConsumer();
}
