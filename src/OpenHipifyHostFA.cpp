#include "OpenHipifyHostFA.h"
#include "utils/Defs.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_KERNEL_DEF = "kernelDef";

std::unique_ptr<ASTConsumer>
OpenHipifyHostFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(varDecl(isExpansionInMainFile(),
                               hasType(cxxRecordDecl(hasName("cl::Kernel"))))
                           .bind(B_KERNEL_DEF),
                       this);

  return m_finder->newASTConsumer();
}

void OpenHipifyHostFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (KernelDefinition(res))
    return;
}

bool OpenHipifyHostFA::KernelDefinition(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const VarDecl *kernelDecl = res.Nodes.getNodeAs<VarDecl>(B_KERNEL_DEF);
  if (!kernelDecl)
    return false;

  llvm::errs() << sOpenHipify << "\n";
  return true;
}