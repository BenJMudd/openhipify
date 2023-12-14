#include "OpenHipifyFA.h"
#include "utils/Defs.h"

using namespace ASTMatch;

const StringRef B_CALL_EXPR = "callExpr";

void OpenHipifyFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  auto *callExpr = res.Nodes.getNodeAs<clang::CallExpr>(B_CALL_EXPR);
  if (!callExpr)
    return;

  llvm::dbgs() << sOpenHipify << "Found callExpr!"
               << "\n";
}

std::unique_ptr<clang::ASTConsumer>
OpenHipifyFA::CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  return m_finder->newASTConsumer();
}
