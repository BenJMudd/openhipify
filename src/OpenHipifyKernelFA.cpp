#include "OpenHipifyKernelFA.h"
#include "HIPDefs.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";
const StringRef B_KERNEL_DECL = "kernelFuncDecl";

std::unique_ptr<ASTConsumer>
OpenHipifyKernelFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
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

void OpenHipifyKernelFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (OpenCLFunctionCall(res))
    return;

  if (OpenCLKernelFunctionDecl(res))
    return;
}

bool OpenHipifyKernelFA::OpenCLKernelFunctionDecl(
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
    SourceRange paramRange = param->getSourceRange();
    // read parameter string from source text
    std::string typeStr(clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(paramRange), *res.SourceManager,
        clang::LangOptions(), nullptr));
    for (const std::string &openCLAddrSpaceStr :
         OpenCL::AddrSpace::SPACES_SET) {
      size_t addrSpaceIdx = typeStr.find(openCLAddrSpaceStr);
      if (addrSpaceIdx == std::string::npos)
        continue;

      // OpenCL parameter found, strip memeory range
      SourceLocation typeBeginLoc = param->getBeginLoc();
      SourceLocation addrSpaceBeginLoc =
          typeBeginLoc.getLocWithOffset(addrSpaceIdx);
      SourceLocation addrSpaceEndLoc = typeBeginLoc.getLocWithOffset(
          addrSpaceIdx + openCLAddrSpaceStr.size() + 1);
      CharSourceRange addrSpaceRng =
          CharSourceRange::getCharRange(addrSpaceBeginLoc, addrSpaceEndLoc);

      ct::Replacement replacement(*res.SourceManager, addrSpaceRng, {});
      llvm::consumeError(m_replacements.add(replacement));
    }
  }

  return true;
}

bool OpenHipifyKernelFA::OpenCLFunctionCall(
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
  case OpenCL::KernelFuncs::BARRIER: {
    ReplaceBARRIER(*callExpr, res);
  } break;
  }

  return false;
}

bool OpenHipifyKernelFA::ReplaceBARRIER(
    const clang::CallExpr &callExpr,
    const ASTMatch::MatchFinder::MatchResult &res) {
  const Expr *fenceTypeFlag = callExpr.getArg(0);
  if (!fenceTypeFlag)
    return false;

  Expr::EvalResult fenceTypeEval;
  const clang::ASTContext *ctx = res.Context;
  // TODO: add aux function for false
  if (!fenceTypeFlag->EvaluateAsInt(fenceTypeEval, *ctx))
    return false;

  APSInt fenceType = fenceTypeEval.Val.getInt();
  const SourceManager *SM = res.SourceManager;
  SourceLocation startLoc = callExpr.getBeginLoc();
  SourceLocation endLoc = callExpr.getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  bool insertNewExpr = false;
  if ((fenceType & OpenCL::CLK_LOCAL_MEM_FENCE) != 0) {
    ct::Replacement replacement(*SM, exprCharRange, HIP::THREAD_FENCE_BLOCK);
    llvm::consumeError(m_replacements.add(replacement));
    insertNewExpr = true;
  }
  if ((fenceType & OpenCL::CLK_GLOBAL_MEM_FENCE) != 0) {
    ct::Replacement replacement;
    // If source contains both a local and a global fence, we must insert
    // another statement for this. In OpenCL, this is done via bit
    // flags, whereas in HIP this is done via separate function calls
    if (insertNewExpr) {
      // Lex to the end of the line
      const char *barrierBuf = SM->getCharacterData(callExpr.getEndLoc());
      const char *endBuf =
          SM->getCharacterData(SM->getLocForEndOfFile(SM->getMainFileID()));
      Lexer lex(callExpr.getEndLoc(), clang::LangOptions(), barrierBuf,
                barrierBuf, endBuf);

      clang::Token tok;
      lex.LexFromRawLexer(tok);
      while (tok.isNot(clang::tok::semi)) {
        lex.LexFromRawLexer(tok);
      }

      // End of the line found, insert a new instruciton
      std::string hipRepl = HIP::THREAD_FENCE;
      hipRepl += ';';
      replacement = ct::Replacement(*SM, tok.getEndLoc(), 0, hipRepl);
    } else {
      replacement = ct::Replacement(*SM, exprCharRange, HIP::THREAD_FENCE);
    }
    llvm::consumeError(m_replacements.add(replacement));
  }
  return true;
}

bool OpenHipifyKernelFA::ReplaceGET_GENERIC_THREAD_ID(
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

  switch (funcIdent) {
  case OpenCL::KernelFuncs::GET_GLOBAL_ID: {
    // hipBlockDim_DIM * hipBlockIdx_DIM + hipThreadIdx_DIM

    // Lex next token to see if it's a semi colon. If not, we
    // guard the insertion as statements after can result in
    // change in execution. E.g. get_global_id(0) * 2 would
    // wrongly translate to hipBlockDim_x * hipBlockIdx_x + hipThreadIdx_x * 2
    // without guard, incorrectly.
    Token nextTok;
    LangOptions LO;
    SourceLocation nextTokLoc = Lexer::getLocForEndOfToken(
        callExpr.getEndLoc(), 0, *res.SourceManager, LO);
    Lexer::getRawToken(nextTokLoc, nextTok, *res.SourceManager, LO, true);

    bool guardHipInsertion = nextTok.getKind() != tok::semi;

    if (guardHipInsertion)
      hipDimOS << "(";

    hipDimOS << HIP::BLOCK_DIM_GENERIC << hipDimension << " * "
             << HIP::BLOCK_IDX_GENERIC << hipDimension << " + "
             << HIP::THREAD_IDX_GENERIC << hipDimension;

    if (guardHipInsertion)
      hipDimOS << ")";
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
  default:
    break;
  }

  const SourceManager *srcManager = res.SourceManager;
  SourceLocation startLoc = callExpr.getBeginLoc();
  SourceLocation endLoc = callExpr.getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  ct::Replacement replacement(*srcManager, exprCharRange, hipDimOS.str());
  llvm::consumeError(m_replacements.add(replacement));
  return true;
}