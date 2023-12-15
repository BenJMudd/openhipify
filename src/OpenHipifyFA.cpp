#include "OpenHipifyFA.h"
#include "HIPDefs.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";
const StringRef B_KERNEL_DECL = "kernelFuncDecl";

std::unique_ptr<ASTConsumer>
OpenHipifyFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  m_finder->addMatcher(
      functionDecl(isExpansionInMainFile(), hasAttr(attr::OpenCLKernel))
          .bind(B_KERNEL_DECL),
      this);

  return m_finder->newASTConsumer();
}

void OpenHipifyFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (OpenCLFunctionCall(res))
    return;

  if (OpenCLKernelFunctionDecl(res))
    return;
}

bool OpenHipifyFA::OpenCLKernelFunctionDecl(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const FunctionDecl *funcDecl =
      res.Nodes.getNodeAs<FunctionDecl>(B_KERNEL_DECL);
  if (!funcDecl)
    return false;

  // Replace __kernel function attribute with HIP equivalent __global__
  auto *kAttr = funcDecl->getAttr<OpenCLKernelAttr>();
  CharSourceRange kAttrRng = CharSourceRange::getTokenRange(kAttr->getRange());
  ct::Replacement replacement(*res.SourceManager, kAttrRng,
                              HIP::GLOBAL_FUNC_ATTR);
  llvm::consumeError(m_replacements.add(replacement));

  for (ParmVarDecl *param : funcDecl->parameters()) {
    TypeSourceInfo *typeSrc = param->getTypeSourceInfo();
    std::string typeStr = typeSrc->getType().getAsString();
    // LangAS addrSpace = param->getType().getAddressSpace();
    // auto openCLAddrSpaceLookup = OpenCL::OPENCL_ADDR_SPACES.find(addrSpace);
    // if (openCLAddrSpaceLookup == OpenCL::OPENCL_ADDR_SPACES.end())
    //   continue;

    // OpenCL parameter found
    for (const std::string &OPENCL_ADDR_SPACES :
         OpenCL::AddrSpace::SPACES_SET) {
      if (typeStr.find(OPENCL_ADDR_SPACES) == std::string::npos)
        continue;

      llvm::errs() << sOpenHipify << "Found attribute: " << typeStr << "\n";
    }
  }

  return true;
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
  case OpenCL::KernelFuncs::GET_GLOBAL_ID:
  case OpenCL::KernelFuncs::GET_LOCAL_ID:
  case OpenCL::KernelFuncs::GET_GROUP_ID:
  case OpenCL::KernelFuncs::GET_LOCAL_SIZE: {
    ReplaceGET_GENERIC_THREAD_ID(*callExpr, res, funcSearch->second);
  } break;
  }

  return false;
}

bool OpenHipifyFA::ReplaceGET_GENERIC_THREAD_ID(
    const clang::CallExpr &callExpr,
    const ASTMatch::MatchFinder::MatchResult &res,
    OpenCL::KernelFuncs funcIdent) {
  const Expr *dimensionArg = callExpr.getArg(0);
  if (!dimensionArg)
    return false;

  // Attempt to evaluate the dimension parameter. If possible we inplace
  // generate correspondent HIP code. If not we must insert a utility
  // function. For most OpenCL code this will be easily evaluated
  // TODO: generate utility function
  Expr::EvalResult dimensionFold;
  const clang::ASTContext *ctx = res.Context;
  if (!dimensionArg->EvaluateAsInt(dimensionFold, *ctx))
    return false;

  APSInt dimension = dimensionFold.Val.getInt();
  char hipDimension;
  if (dimension == 0)
    hipDimension = 'x';
  else if (dimension == 1)
    hipDimension = 'y';
  else if (dimension == 2)
    hipDimension = 'z';
  else {
    llvm::errs() << sOpenHipify << sErr
                 << "Out of range dimension identifier: " << dimension
                 << " at location: "
                 << dimensionArg->getExprLoc().printToString(*res.SourceManager)
                 << "\n";
    return false;
  }

  // Generate HIP replacement:
  clang::SmallString<40> hipDimensionStr;
  llvm::raw_svector_ostream hipDimOS(hipDimensionStr);
  // TODO: only add comment guard for expr at end of stmt
  hipDimOS << "(";
  switch (funcIdent) {
  case OpenCL::KernelFuncs::GET_GLOBAL_ID: {
    // hipBlockDim_DIM * hipBlockIdx_DIM + hipThreadIdx_DIM
    hipDimOS << HIP::BLOCK_DIM_GENERIC << hipDimension << " * "
             << HIP::BLOCK_IDX_GENERIC << hipDimension << " + "
             << HIP::THREAD_IDX_GENERIC << hipDimension;
  } break;
  case OpenCL::KernelFuncs::GET_LOCAL_ID: {
    // hipThreadIdx_DIM
    hipDimOS << HIP::THREAD_IDX_GENERIC << hipDimension;
  } break;
  case OpenCL::KernelFuncs::GET_GROUP_ID: {
    hipDimOS << HIP::BLOCK_IDX_GENERIC << hipDimension;
  } break;
  case OpenCL::KernelFuncs::GET_LOCAL_SIZE: {
    hipDimOS << HIP::BLOCK_DIM_GENERIC << hipDimension;
  } break;
  }
  hipDimOS << ")";
  const SourceManager *srcManager = res.SourceManager;
  SourceLocation startLoc = callExpr.getBeginLoc();
  SourceLocation endLoc = callExpr.getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  ct::Replacement replacement(*srcManager, exprCharRange, hipDimOS.str());
  llvm::consumeError(m_replacements.add(replacement));
  return true;
}