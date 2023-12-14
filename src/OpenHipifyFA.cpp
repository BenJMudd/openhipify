#include "OpenHipifyFA.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";

void OpenHipifyFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  const CallExpr *callExpr = res.Nodes.getNodeAs<CallExpr>(B_CALL_EXPR);
  if (!callExpr)
    return;

  const FunctionDecl *funcDecl = callExpr->getDirectCallee();
  if (!funcDecl)
    return;

  const DeclarationNameInfo &nameInfo = funcDecl->getNameInfo();
  std::string funcName = nameInfo.getAsString();
  if (funcName.compare("get_global_id") != 0)
    return;

  const Expr *dimensionArg = callExpr->getArg(0);
  if (!dimensionArg)
    return;

  Expr::EvalResult dimensionFold;
  const clang::ASTContext *ctx = res.Context;
  if (!dimensionArg->EvaluateAsInt(dimensionFold, *ctx))
    return;

  APSInt dimension = dimensionFold.Val.getInt();
  char hipDimension;
  if (dimension == 0)
    hipDimension = 'x';
  else if (dimension == 1)
    hipDimension = 'y';
  else if (dimension == 2)
    hipDimension = 'z';

  std::string hipReplacement("threadIdx.");
  hipReplacement += hipDimension;
  llvm::errs() << sOpenHipify
               << "Replaced expression: " << hipReplacement.c_str() << "\n";

  const SourceManager *srcManager = res.SourceManager;
  SourceLocation startLoc = callExpr->getBeginLoc();
  SourceLocation endLoc = callExpr->getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  ct::Replacement replacement(*srcManager, exprCharRange,
                              hipReplacement.c_str());
  llvm::consumeError(m_replacements.add(replacement));
}

std::unique_ptr<ASTConsumer>
OpenHipifyFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  return m_finder->newASTConsumer();
}
