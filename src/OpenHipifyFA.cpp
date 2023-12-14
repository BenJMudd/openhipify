#include "OpenHipifyFA.h"
#include "HIPDefs.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";

std::unique_ptr<ASTConsumer>
OpenHipifyFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  return m_finder->newASTConsumer();
}

void OpenHipifyFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (OpenCLFunctionCall(res))
    return;
}

bool OpenHipifyFA::OpenCLFunctionCall(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const CallExpr *callExpr = res.Nodes.getNodeAs<CallExpr>(B_CALL_EXPR);
  if (!callExpr)
    return false;

  const FunctionDecl *funcDecl = callExpr->getDirectCallee();
  if (!funcDecl)
    return false;

  const DeclarationNameInfo &nameInfo = funcDecl->getNameInfo();
  std::string funcName = nameInfo.getAsString();
  auto funcSearch = OpenCL::KERNEL_FUNC_MAP.find(funcName);
  if (funcSearch == OpenCL::KERNEL_FUNC_MAP.end())
    return false;

  // Found OpenCL function call

  switch (funcSearch->second) {
  case OpenCL::KernelFuncs::GET_GLOBAL_ID: {
    Replace_GET_GLOBAL_ID(*callExpr, res);
  } break;
  default:
    break;
  }
  return false;
}

void OpenHipifyFA::Replace_GET_GLOBAL_ID(
    const CallExpr &callExpr, const ASTMatch::MatchFinder::MatchResult &res) {
  const Expr *dimensionArg = callExpr.getArg(0);
  if (!dimensionArg)
    return;

  // Attempt to evaluate the dimension parameter. If possible we inplace
  // generate correspondent HIP code. If not we must insert a utility function.
  // For most OpenCL code this will be easily evaluated
  // TODO: generate utility function
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

  // Generate HIP replacement:
  // hipBlockDim_DIM * hipBlockIdx_DIM + hipThreadIdx_DIM
  clang::SmallString<40> hipDimensionStr;
  llvm::raw_svector_ostream hipDimOS(hipDimensionStr);
  hipDimOS << HIP::BLOCK_DIM_GENERIC << hipDimension << " * "
           << HIP::BLOCK_IDX_GENERIC << hipDimension << " + "
           << HIP::THREAD_IDX_GENERIC << hipDimension;

  const SourceManager *srcManager = res.SourceManager;
  SourceLocation startLoc = callExpr.getBeginLoc();
  SourceLocation endLoc = callExpr.getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  ct::Replacement replacement(*srcManager, exprCharRange, hipDimOS.str());
  llvm::consumeError(m_replacements.add(replacement));
}
