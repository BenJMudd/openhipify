#include "OpenHipifyHostFA.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";

std::unique_ptr<ASTConsumer>
OpenHipifyHostFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  return m_finder->newASTConsumer();
}

void OpenHipifyHostFA::EndSourceFileAction() {
  const SourceManager &SM = getCompilerInstance().getSourceManager();
  ASTContext &astCtx = getCompilerInstance().getASTContext();
  m_kernelTracker.Finalise(SM);

  for (const auto &[kernelDecl, kInfo] : m_kernelTracker.GetKernelInfo()) {
    // args are indexed in
    std::vector<std::string> args;
    std::vector<bool> argsUse;
    args.resize(kInfo.args.size());
    argsUse.resize(kInfo.args.size());
    std::fill(argsUse.begin(), argsUse.end(), false);

    size_t numArgs = 0;

    // Set after the first launch command has been processed
    bool argsFinalised = false;

    auto argIter = kInfo.args.begin();
    auto launchIter = kInfo.launches.begin();
    while (argIter != kInfo.args.end() && launchIter != kInfo.launches.end()) {
      const CallExpr *setArgExpr = *argIter;
      const CallExpr *launchKernelExpr = *launchIter;
      unsigned argPos = SM.getFileOffset(setArgExpr->getBeginLoc());
      unsigned launchPos = SM.getFileOffset(launchKernelExpr->getBeginLoc());
      if (argPos < launchPos) {
        // Handle setting argument case
        // Extract arg number
        const Expr *argPosExpr = setArgExpr->getArg(1);
        Expr::EvalResult argPosEval;
        if (!argPosExpr->EvaluateAsInt(argPosEval, astCtx)) {
          llvm::errs()
              << sOpenHipify << sErr
              << "Unable to parse kernel argument position at position: "
              << argPosExpr->getExprLoc().printToString(SM);
          argIter++;
          continue;
        }

        uint64_t argPos = argPosEval.Val.getInt().getExtValue();

        // Extract text for kernel argument
        const Expr *kernelArgExpr = setArgExpr->getArg(3);
        CharSourceRange kernelArgRng =
            CharSourceRange::getTokenRange(kernelArgExpr->getSourceRange());
        std::string kernelArgStr(
            Lexer::getSourceText(kernelArgRng, SM, LangOptions(), nullptr));

        // Track
        if (argPos > args.size()) {
          // Extract text for whole of kernel
          CharSourceRange setKArgStr =
              CharSourceRange::getTokenRange(setArgExpr->getSourceRange());
          std::string setKernelArgStr(
              Lexer::getSourceText(setKArgStr, SM, LangOptions(), nullptr));
          llvm::errs() << sOpenHipify << sErr << "clSetKernelArg expression: \'"
                       << setKernelArgStr << "\' at location: "
                       << setArgExpr->getExprLoc().printToString(SM)
                       << " references argument number: " << argPos
                       << " where the kernel contains at maximum only "
                       << args.size() << " argument(s).";
          argIter++;
          continue;
        }
        args[argPos] = kernelArgStr;
        argsUse[argPos] = true;
        if (argPos > numArgs) {
          numArgs = argPos;
        }

        argIter++;
      } else {
        // Handle launching kernel case

        launchIter++;
      }
    }
  }

  llvm::errs() << sOpenHipify << "End of file action"
               << "\n";
}

void OpenHipifyHostFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (FunctionCall(res))
    return;
}

bool OpenHipifyHostFA::FunctionCall(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const CallExpr *callExpr = res.Nodes.getNodeAs<CallExpr>(B_CALL_EXPR);
  if (!callExpr)
    return false;

  const FunctionDecl *funcDecl = callExpr->getDirectCallee();
  if (!funcDecl)
    return false;

  const DeclarationNameInfo &nameInfo = funcDecl->getNameInfo();
  std::string funcName = nameInfo.getAsString();
  auto funcSearch = OpenCL::HOST_FUNC_MAP.find(funcName);
  if (funcSearch == OpenCL::HOST_FUNC_MAP.end())
    return false;

  auto iter = OpenCL::HOST_MEM_FUNCS.find(funcSearch->second);
  if (iter != OpenCL::HOST_MEM_FUNCS.end()) {
    // Memory related function found
    HandleMemoryFunctionCall(callExpr, *iter);
  }

  iter = OpenCL::HOST_KERNEL_FUNCS.find(funcSearch->second);
  if (iter != OpenCL::HOST_KERNEL_FUNCS.end()) {
    // Kernel related function found
    HandleKernelFunctionCall(callExpr, *iter);
  }

  return false;
}

bool OpenHipifyHostFA::HandleMemoryFunctionCall(const CallExpr *callExpr,
                                                OpenCL::HostFuncs func) {

  switch (func) {
  case OpenCL::HostFuncs::clCreateBuffer: {
    return ReplaceCreateBuffer(callExpr);
  } break;
  case OpenCL::HostFuncs::clEnqueueWriteBuffer: {
    return ReplaceEnqueWriteBuffer(callExpr);
  }
  default: {
  } break;
  }

  return false;
}

bool OpenHipifyHostFA::ReplaceCreateBuffer(const CallExpr *callExpr) {
  ASTContext &astCtx = getCompilerInstance().getASTContext();
  SourceManager &SM = getCompilerInstance().getSourceManager();

  auto callExprParIter = astCtx.getParents(*callExpr).begin();
  // Grab parent of callExpr
  // TODO: Support other cases than just vardecl, e.g. binary expression
  const VarDecl *varDecl;
  varDecl = callExprParIter->get<VarDecl>();
  if (!varDecl)
    return false;

  // Renaming the type from cl_mem -> void*
  SourceLocation typeBeginLoc = varDecl->getBeginLoc();
  std::string typeStr = varDecl->getTypeSourceInfo()->getType().getAsString();
  ct::Replacement typeReplacement(SM, typeBeginLoc, typeStr.length(),
                                  HIP::VOID_PTR);
  llvm::consumeError(m_replacements.add(typeReplacement));

  // end vardecl type with ; to split call into two statements:
  //
  // cl_mem mem = clCreateBuffer(...);
  //
  // void* mem;
  // hipMalloc((void**) &mem, ...);
  //
  // TODO: come up with a better way to do this

  // Finding SourceLocation for: cl_mem mem = clCreateBuffer(...);
  //                                        ^
  const char *varDeclStartBuf = SM.getCharacterData(varDecl->getBeginLoc());
  const char *fileEndBuf =
      SM.getCharacterData(SM.getLocForEndOfFile(SM.getMainFileID()));
  Lexer lex(typeBeginLoc, clang::LangOptions(), varDeclStartBuf,
            varDeclStartBuf, fileEndBuf);

  clang::Token tok;
  lex.LexFromRawLexer(tok);
  while (tok.isNot(clang::tok::equal)) {
    lex.LexFromRawLexer(tok);
  }

  // Range created from equal token and start of function call
  // cl_mem mem = clCreateBuffer(...);
  //            ^^^
  CharSourceRange binaryExprRng = CharSourceRange::getTokenRange(
      tok.getLocation(), callExpr->getBeginLoc());

  std::string splitExpr;
  llvm::raw_string_ostream splitExprStr(splitExpr);
  splitExprStr << HIP::EOL << HIP::MALLOC;

  // Final replacement for splitting we change the function call as well
  // cl_mem mem = clCreateBuffer(...);
  // cl_mem mem ; hipMalloc(...);
  ct::Replacement binaryExprRepl(SM, binaryExprRng, splitExprStr.str());
  llvm::consumeError(m_replacements.add(binaryExprRepl));

  // Argument replacement
  // size of buffer argument extracted
  const Expr *bufSize = callExpr->getArg(2);

  CharSourceRange bufSizeSrcRng =
      CharSourceRange::getTokenRange(bufSize->getSourceRange());
  std::string bufSizeExprStr(
      Lexer::getSourceText(bufSizeSrcRng, SM, LangOptions(), nullptr));

  // Name of mem variable extracted
  std::string varName = varDecl->getNameAsString();

  // Whole argument replacement
  SourceLocation argStart = callExpr->getArg(0)->getExprLoc();
  SourceLocation argEnd = callExpr->getEndLoc();
  CharSourceRange argRng = CharSourceRange::getCharRange(argStart, argEnd);

  std::string newArgs;
  llvm::raw_string_ostream newArgsStr(newArgs);
  newArgsStr << HIP::VOID_PTR_PTR_CAST << "&" << varName << ","
             << bufSizeExprStr;
  ct::Replacement argsRepl(SM, argRng, newArgsStr.str());
  llvm::consumeError(m_replacements.add(argsRepl));

  return true;
}

// TODO: Handle error return for clCreateWriteBuffer
bool OpenHipifyHostFA::ReplaceEnqueWriteBuffer(const CallExpr *callExpr) {
  ASTContext &astCtx = getCompilerInstance().getASTContext();
  SourceManager &SM = getCompilerInstance().getSourceManager();

  // Replace function name with memcpy
  SourceLocation funcNameLoc = callExpr->getBeginLoc();
  std::string funcNameStr =
      callExpr->getDirectCallee()->getNameInfo().getName().getAsString();
  ct::Replacement nameReplacement(SM, funcNameLoc, funcNameStr.length(),
                                  HIP::MEMCPY);
  llvm::consumeError(m_replacements.add(nameReplacement));

  // Extract args at series src, dst, offset, size
  const Expr *argDst = callExpr->getArg(1);
  const Expr *argSrc = callExpr->getArg(5);
  const Expr *argOffset = callExpr->getArg(3);
  const Expr *argSize = callExpr->getArg(4);

  // Extract string for src, dst, size
  auto ExprToStr = [&](const Expr &expr) -> std::string {
    CharSourceRange rng = CharSourceRange::getTokenRange(expr.getSourceRange());
    return std::string(Lexer::getSourceText(rng, SM, LangOptions(), nullptr));
  };
  std::string argDstStr = ExprToStr(*argDst);
  std::string argSrcStr = ExprToStr(*argSrc);
  std::string argSizeStr = ExprToStr(*argSize);

  // Evaluate offset
  std::string argDstWithOff;
  llvm::raw_string_ostream argDstWithOffStr(argDstWithOff);
  Expr::EvalResult offsetEval;
  if (argOffset->EvaluateAsInt(offsetEval, astCtx)) {
    APSInt offset = offsetEval.Val.getInt();
    if (offset == 0) {
      // Offset is set to 0, no need to modify dest buffer
      argDstWithOffStr << argDstStr;
    } else {
      // Offset is non zero, dest buffer must be modified
      argDstWithOffStr << argDstStr << "+" << offset;
    }
  } else {
    // Offset is not evaluateable, we must append the dest pointer
    // with the offset
    std::string argOffsetStr = ExprToStr(*argOffset);
    argDstWithOffStr << "(" << argDstStr << ")+(" << argOffsetStr << ")";
  }

  // Construct new arguments
  std::string hipMemcpyArgs;
  llvm::raw_string_ostream hipMemcpyArgsStr(hipMemcpyArgs);
  hipMemcpyArgsStr << argDstWithOffStr.str() << "," << argSrcStr << ","
                   << argSizeStr << "," << HIP::MEMCPY_HOST_DEVICE;

  // remove args, and replace
  SourceLocation argStart = callExpr->getArg(0)->getExprLoc();
  SourceLocation argEnd = callExpr->getEndLoc();
  CharSourceRange argRng = CharSourceRange::getCharRange(argStart, argEnd);
  ct::Replacement argsRepl(SM, argRng, hipMemcpyArgsStr.str());
  llvm::consumeError(m_replacements.add(argsRepl));

  return true;
}

bool OpenHipifyHostFA::HandleKernelFunctionCall(const CallExpr *callExpr,
                                                OpenCL::HostFuncs func) {
  switch (func) {
  case OpenCL::HostFuncs::clSetKernelArg: {
    return TrackKernelSetArg(callExpr);
  } break;
  case OpenCL::HostFuncs::clEnqueueNDRangeKernel: {
    return TrackKernelLaunch(callExpr);
  }
  case OpenCL::HostFuncs::clCreateKernel: {
    return TrackKernelCreate(callExpr);
  }
  default: {
  } break;
  }

  return false;
}

bool OpenHipifyHostFA::TrackKernelSetArg(const CallExpr *callExpr) {
  const ValueDecl *kernelDecl;
  if (!ExtractKernelDeclFromArg(callExpr, 0, &kernelDecl)) {
    return false;
  }

  m_kernelTracker.InsertArg(kernelDecl, callExpr);
  return true;
}

bool OpenHipifyHostFA::TrackKernelLaunch(const clang::CallExpr *callExpr) {
  const ValueDecl *kernelDecl;
  if (!ExtractKernelDeclFromArg(callExpr, 1, &kernelDecl)) {
    return false;
  }

  m_kernelTracker.InsertLaunch(kernelDecl, callExpr);
  return true;
}

bool OpenHipifyHostFA::TrackKernelCreate(const clang::CallExpr *callExpr) {
  ASTContext &astCtx = getCompilerInstance().getASTContext();
  SourceManager &SM = getCompilerInstance().getSourceManager();

  auto callExprParIter = astCtx.getParents(*callExpr).begin();
  // Grab kernel declaration
  const VarDecl *kernelDecl = callExprParIter->get<VarDecl>();
  if (!kernelDecl)
    return false;

  const Expr *kernelNameExpr = callExpr->getArg(1)->IgnoreCasts();
  const clang::StringLiteral *kernelName =
      dyn_cast<clang::StringLiteral>(kernelNameExpr);
  if (!kernelName) {
    llvm::errs() << sOpenHipify << sErr << "kernel function name at location: "
                 << kernelNameExpr->getExprLoc().printToString(SM)
                 << " cannot be parsed.";
    return false;
  }

  std::string kernelStr(kernelName->getString());
  // TODO: involve tracking with cl_program to resolve ambiguity if two
  // kernels have the same name in different files

  m_kernelTracker.InsertName(kernelDecl, kernelStr);
  return true;
}

bool OpenHipifyHostFA::ExtractKernelDeclFromArg(
    const clang::CallExpr *callExpr, size_t argIndex,
    const clang::ValueDecl **kernelDecl) {
  SourceManager &SM = getCompilerInstance().getSourceManager();

  const Expr *arg1 = callExpr->getArg(argIndex)->IgnoreCasts();
  const DeclRefExpr *kernelRef = dyn_cast<DeclRefExpr>(arg1);
  if (!kernelRef) {
    llvm::errs()
        << sOpenHipify << sErr
        << "kernel argument at: " << arg1->getExprLoc().printToString(SM)
        << " is not a variable reference. This is currently unsupported.";
    return false;
  }

  *kernelDecl = kernelRef->getDecl();
  if (!*kernelDecl) {
    llvm::errs() << sOpenHipify << sErr << "kernel reference at: "
                 << kernelRef->getBeginLoc().printToString(SM)
                 << " is not a variable reference.";
    return false;
  }

  return true;
}
