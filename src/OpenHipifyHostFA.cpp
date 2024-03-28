#include "OpenHipifyHostFA.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/WithColor.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";
const StringRef B_VAR_DECL = "varDecl";
#define ERR_BOLD_STR llvm::WithColor(llvm::errs(), raw_ostream::WHITE, true)

std::unique_ptr<ASTConsumer>
OpenHipifyHostFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  SM = &getCompilerInstance().getSourceManager();
  AST = &getCompilerInstance().getASTContext();

  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  m_finder->addMatcher(varDecl(isExpansionInMainFile()).bind(B_VAR_DECL), this);

  return m_finder->newASTConsumer();
}

void OpenHipifyHostFA::EndSourceFileAction() {
  m_kernelTracker.Finalise(*SM);
  std::set<std::string> kernelFilesUsed;

  for (const auto &[kernelDecl, kInfo] : m_kernelTracker.GetKernelInfo()) {
    // args are indexed in
    // [..(arg expression string, has addr op stripped)]
    struct ArgInfo {
      ArgInfo(std::string str, bool stripped, bool cast)
          : argStr(str), isAddrOpStripped(stripped), toCast(cast) {}
      ArgInfo() : isAddrOpStripped(false), toCast(false) {}

      std::string argStr;
      bool isAddrOpStripped;
      bool toCast;
    };

    std::vector<ArgInfo> args;
    std::vector<bool> argsUse;
    std::vector<bool> argTypesToCast;
    args.resize(kInfo.args.size());
    argsUse.resize(kInfo.args.size());
    std::fill(argsUse.begin(), argsUse.end(), false);
    std::fill(argTypesToCast.begin(), argTypesToCast.end(), false);

    size_t numArgs = 0;

    // Set after the first launch command has been processed
    bool argsFinalised = false;

    // retrive kernel definition (if possible)
    const KernelDefinition *kDef = nullptr;
    auto kFuncMapIter = m_kernelFuncMap.find(kInfo.funcName);
    if (kFuncMapIter != m_kernelFuncMap.end()) {
      kDef = &kFuncMapIter->second;
    }

    auto StripAddrOfOp = [&](const Expr *expr,
                             bool &isAddrStripped) -> std::string {
      // Test if & is used to describe arg
      isAddrStripped = false;
      const UnaryOperator *unaryOp =
          dyn_cast<UnaryOperator>(expr->IgnoreCasts());
      if (unaryOp && unaryOp->getOpcode() == UO_AddrOf) {
        // Found & prepend
        isAddrStripped = true;
        return ExprToStr(unaryOp->getSubExpr());
      } else {
        return ExprToStr(expr);
      }
    };

    auto HandlLaunchExpr = [&](const CallExpr *launchKernelExpr) {
      if (!argsFinalised) {
        auto EnsureArguments = [&]() -> bool {
          for (size_t i = 0; i < numArgs; ++i) {
            if (!argsUse[i]) {
              ERR_BOLD_STR << sOpenHipify;
              llvm::WithColor(llvm::errs(), raw_ostream::RED, true) << sErr;
              ERR_BOLD_STR << "Kernel '" << kInfo.funcName
                           << "' launch accepts " << numArgs
                           << " arguments, but argument at index " << i
                           << " has not been set."
                           << "\n";

              // Extra kernel error messagin
              std::string kernelNameExtrInfo;
              llvm::raw_string_ostream kernelNameExtrInfoStr(
                  kernelNameExtrInfo);
              kernelNameExtrInfoStr << "kernel: " << kInfo.funcName;
              if (kDef) {
                // kernel def is included in translation pass
                kernelNameExtrInfoStr << " (" << kDef->fileName
                                      << ", l:" << kDef->defLine
                                      << " c:" << kDef->defCol << ")";
              } else {
                kernelNameExtrInfoStr << " (untracked)";
              }

              PrettyError(
                  {launchKernelExpr->getArg(1)->getBeginLoc(),
                   launchKernelExpr->getArg(2)->getBeginLoc().getLocWithOffset(
                       -1)},
                  raw_ostream::RED, kernelNameExtrInfoStr.str());
              return false;
            }
          }

          return true;
        };

        if (!EnsureArguments()) {
          return;
        }

        args.resize(numArgs);
        argsFinalised = true;
      }

      // Handle launching kernel case
      // Extract arg 4,5 for dimensions
      const Expr *numBlocksExpr = launchKernelExpr->getArg(4);
      const Expr *blockSizeExpr = launchKernelExpr->getArg(5);
      bool isNumBlocksAddrStripped, isBlockSizeAddrStripped;

      std::string numBlocksStr =
          StripAddrOfOp(numBlocksExpr, isNumBlocksAddrStripped);
      std::string blockSizeStr =
          StripAddrOfOp(blockSizeExpr, isBlockSizeAddrStripped);

      // Replace function name with hip equivalent
      SourceLocation funcNameLoc = launchKernelExpr->getBeginLoc();
      ct::Replacement nameReplacement(
          *SM, funcNameLoc, OpenCL::CL_ENQUEUE_NDRANGE_BUFFER.length(),
          HIP::LAUNCHKERNELGGL);
      llvm::consumeError(m_replacements.add(nameReplacement));

      // Construct new args
      std::string launchKernelArgs;
      llvm::raw_string_ostream launchKernelArgsStr(launchKernelArgs);
      launchKernelArgsStr << kInfo.funcName << ","
                          << "dim3(";
      if (isNumBlocksAddrStripped) {
        launchKernelArgsStr << numBlocksStr << "),";
      } else {
        launchKernelArgsStr << "*(" << numBlocksStr << ")),";
      }

      launchKernelArgsStr << "dim3(";
      if (isBlockSizeAddrStripped) {
        launchKernelArgsStr << blockSizeStr << "),";
      } else {
        launchKernelArgsStr << "*(" << blockSizeStr << ")),";
      }

      launchKernelArgsStr << "0,0";

      // Append extracted args
      for (size_t argIdx = 0; argIdx < args.size(); ++argIdx) {
        launchKernelArgsStr << ",";
        if (args[argIdx].toCast) {
          if (kDef) {
            std::string typeToCast = kDef->argTypes[argIdx];
            launchKernelArgsStr << "(" << typeToCast << ")";
          } else {
            ERR_BOLD_STR << sOpenHipify;
            llvm::WithColor(llvm::errs(), raw_ostream::YELLOW, true) << sWarn;
            ERR_BOLD_STR << "Unable to generate cast for cl_mem argument '"
                         << args[argIdx].argStr << "' for kernel '"
                         << kInfo.funcName
                         << "' during launch. Include the definition in the "
                            "transpilation "
                            "process to generate cast.\n";
            // Extra kernel error messagin
            std::string kernelNameExtrInfo;
            llvm::raw_string_ostream kernelNameExtrInfoStr(kernelNameExtrInfo);
            kernelNameExtrInfoStr << "kernel: " << kInfo.funcName
                                  << " (untracked)";

            PrettyError(
                {launchKernelExpr->getArg(1)->getBeginLoc(),
                 launchKernelExpr->getArg(2)->getBeginLoc().getLocWithOffset(
                     -1)},
                raw_ostream::YELLOW, kernelNameExtrInfoStr.str());
          }
        }

        if (args[argIdx].isAddrOpStripped) {
          launchKernelArgsStr << args[argIdx].argStr;
        } else {
          launchKernelArgsStr << "*(" << args[argIdx].argStr << ")";
        }
      }

      // remove args, and replace
      SourceLocation argStart = launchKernelExpr->getArg(0)->getExprLoc();
      SourceLocation argEnd = launchKernelExpr->getEndLoc();
      CharSourceRange argRng = CharSourceRange::getCharRange(argStart, argEnd);
      ct::Replacement argsRepl(*SM, argRng, launchKernelArgsStr.str());
      llvm::consumeError(m_replacements.add(argsRepl));

      // Track kernel launch for future include file generation
      if (kDef) {
        std::map<std::string, std::string> &includeDefs =
            m_kernelIncludeTracker[kDef->fileName];
        auto includeIter = includeDefs.find(kInfo.funcName);
        if (includeIter == includeDefs.end()) {
          includeDefs[kInfo.funcName] = kDef->functionDef;
        }

        kernelFilesUsed.insert(kDef->fileName);
      }
    };

    auto HandleArgExpr = [&](const CallExpr *setArgExpr) { // Extract arg number
      const Expr *argPosExpr = setArgExpr->getArg(1);
      Expr::EvalResult argPosEval;
      if (!argPosExpr->EvaluateAsInt(argPosEval, *AST)) {
        ERR_BOLD_STR << sOpenHipify;
        llvm::WithColor(llvm::errs(), raw_ostream::YELLOW, true) << sWarn;
        ERR_BOLD_STR
            << "Unable to parse kernel argument position, skipping...\n";

        PrettyError({argPosExpr->getBeginLoc(),
                     setArgExpr->getArg(2)->getBeginLoc().getLocWithOffset(-1)},
                    raw_ostream::YELLOW);
        return;
      }

      uint64_t argPos = argPosEval.Val.getInt().getExtValue();
      if (kDef && argPos >= kDef->argTypes.size()) {
        ERR_BOLD_STR << sOpenHipify;
        llvm::WithColor(llvm::errs(), raw_ostream::RED, true) << sErr;
        ERR_BOLD_STR << "Argument index '" << argPos
                     << "' out of range, Kernel '" << kInfo.funcName
                     << "' accepts " << kDef->argTypes.size()
                     << " parameters.\n";

        // Extra kernel error messagin
        std::string kernelNameExtrInfo;
        llvm::raw_string_ostream kernelNameExtrInfoStr(kernelNameExtrInfo);
        kernelNameExtrInfoStr << "kernel: " << kInfo.funcName << " ("
                              << kDef->fileName << ", l:" << kDef->defLine
                              << " c:" << kDef->defCol << ")";

        PrettyError({setArgExpr->getArg(0)->getBeginLoc(),
                     setArgExpr->getArg(2)->getBeginLoc().getLocWithOffset(-1)},
                    raw_ostream::RED, kernelNameExtrInfoStr.str());
      } else if ((argsFinalised && argPos >= numArgs) ||
                 (argPos >= args.size())) {
        ERR_BOLD_STR << sOpenHipify;
        llvm::WithColor(llvm::errs(), raw_ostream::RED, true) << sErr;
        ERR_BOLD_STR << "Argument index '" << argPos
                     << "' out of range, Kernel '" << kInfo.funcName
                     << "' accepts at maximum " << numArgs << " parameters.\n";
        // Extra kernel error messagin
        std::string kernelNameExtrInfo;
        llvm::raw_string_ostream kernelNameExtrInfoStr(kernelNameExtrInfo);
        kernelNameExtrInfoStr << "kernel: " << kInfo.funcName << " (untracked)";

        PrettyError({setArgExpr->getArg(0)->getBeginLoc(),
                     setArgExpr->getArg(2)->getBeginLoc().getLocWithOffset(-1)},
                    raw_ostream::RED, kernelNameExtrInfoStr.str());
        return;
      }

      // Extract text for kernel argument
      const Expr *kernelArgExpr = setArgExpr->getArg(3);

      bool toCast = false;
      const IdentifierInfo *kernelArgTypeIdent =
          kernelArgExpr->IgnoreCasts()->getType().getBaseTypeIdentifier();
      if (kernelArgTypeIdent) {
        // Extracted type, test if it is a cl_mem type.
        // If so, we need to track this so we can cast the use of this to it's
        // kernel type if available
        std::string typeStr(kernelArgTypeIdent->getName());
        if (typeStr == OpenCL::CL_MEM_UNDERLYING) {
          // cl_mem type, track
          toCast = true;
        }
      }

      // Test if & is used to describe arg
      bool isAddrStripped = false;
      std::string kernelArgStr = StripAddrOfOp(kernelArgExpr, isAddrStripped);

      args[argPos] = ArgInfo(kernelArgStr, isAddrStripped, toCast);
      argsUse[argPos] = true;
      if (argPos + 1 > numArgs) {
        numArgs = argPos + 1;
      }

      // Remove expression
      RemoveExprFromSource(setArgExpr);
    };

    auto argIter = kInfo.args.begin();
    auto launchIter = kInfo.launches.begin();
    const Stmt *baseScopeStmt = nullptr;
    const Expr *baseScopeExpr = nullptr;
    while (1) {
      bool isAllArgsProcessed = argIter == kInfo.args.end();
      bool isAllLaunchesProcessed = launchIter == kInfo.launches.end();
      if (isAllArgsProcessed && isAllLaunchesProcessed) {
        break;
      }

      // No previous arguments processed, record the current scope
      // for the kernel
      if (!baseScopeStmt) {
        auto argScopeNode = AST->getParents(*((*argIter)->IgnoreCasts()));
        baseScopeStmt = argScopeNode.begin()->get<Stmt>();
        baseScopeExpr = *argIter;
      }

      auto CheckScope = [&](const CallExpr *expr) -> bool {
        auto exprScopeNode = AST->getParents(*(expr->IgnoreCasts()));
        auto *exprScopeStmt = exprScopeNode.begin()->get<Stmt>();
        if (exprScopeStmt != baseScopeStmt) {
          ERR_BOLD_STR << sOpenHipify;
          llvm::WithColor(llvm::errs(), raw_ostream::YELLOW, true) << sWarn;
          ERR_BOLD_STR
              << "Kernel function called at a different scope from "
                 "initial use. This can result in inconsistent kernel launch "
                 "generation, removing and skipping...\n";
          llvm::errs() << "Initial use:\n";
          PrettyError(baseScopeExpr->getSourceRange(), raw_ostream::GREEN);
          llvm::errs() << "Current use:\n";
          PrettyError(expr->getSourceRange(), raw_ostream::YELLOW);
          RemoveExprFromSource(expr);
          return false;
        }

        return true;
      };
      // TODO: finish
      // To write about for diss, can go on about scope tracking for kernel
      // launching

      if (isAllArgsProcessed) {
        // Handle dangling launches
        if (!CheckScope(*launchIter)) {
          launchIter++;
          continue;
        }

        HandlLaunchExpr(*launchIter);
        launchIter++;
        continue;
      }

      if (isAllLaunchesProcessed) {
        // Handle dangling args, i.e. remove them from source
        if (!CheckScope(*argIter)) {
          argIter++;
          continue;
        }

        RemoveExprFromSource(*argIter);
        argIter++;
        continue;
      }

      const CallExpr *setArgExpr = *argIter;
      const CallExpr *launchKernelExpr = *launchIter;
      unsigned argPos = SM->getFileOffset(setArgExpr->getBeginLoc());
      unsigned launchPos = SM->getFileOffset(launchKernelExpr->getBeginLoc());
      if (argPos < launchPos) {
        if (!CheckScope(setArgExpr)) {
          argIter++;
          continue;
        }

        HandleArgExpr(setArgExpr);
        argIter++;
      } else {
        if (!CheckScope(launchKernelExpr)) {
          launchIter++;
          continue;
        }

        HandlLaunchExpr(launchKernelExpr);
        launchIter++;
      }
    }
  }

  // insert #include for used kernel files
  std::string includes;
  llvm::raw_string_ostream includesStr(includes);
  includesStr << sOpenHipifyGenerated;
  // include hip runtime
  includesStr << "#include \"hip/hip_runtime.h\"\n\n";

  for (std::string trackedInclude : kernelFilesUsed) {
    includesStr << "#include \"" << trackedInclude << ".hpp\"\n";
  }

  includesStr << sOpenHipifyGeneratedEnd;

  SourceLocation includeInsertLoc =
      SM->getLocForStartOfFile(SM->getMainFileID());
  ct::Replacement IncludeRepl(*SM, includeInsertLoc, 0, includesStr.str());
  llvm::consumeError(m_replacements.add(IncludeRepl));
}

void OpenHipifyHostFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (FunctionCall(res))
    return;

  if (VariableDeclaration(res))
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
    return true;
  }

  iter = OpenCL::HOST_KERNEL_FUNCS.find(funcSearch->second);
  if (iter != OpenCL::HOST_KERNEL_FUNCS.end()) {
    // Kernel related function found
    HandleKernelFunctionCall(callExpr, *iter);
    return true;
  }

  iter = OpenCL::HOST_REDUNDANT_FUNCS.find(funcSearch->second);
  if (iter != OpenCL::HOST_REDUNDANT_FUNCS.end()) {
    // Kernel related function found
    HandleRedundantFunctionCall(callExpr);
    return true;
  }

  return false;
}

bool OpenHipifyHostFA::VariableDeclaration(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const VarDecl *varDecl = res.Nodes.getNodeAs<VarDecl>(B_VAR_DECL);
  if (!varDecl)
    return false;

  const IdentifierInfo *typeIdentifier =
      varDecl->getType().getBaseTypeIdentifier();
  if (!typeIdentifier)
    return false;

  std::string varType(typeIdentifier->getName());
  auto iter = OpenCL::CL_TYPES.find(varType);
  if (iter != OpenCL::CL_TYPES.end()) {
    // Remove statement containing redundant opencl types
    // TODO: This is obviously an awful way of doing this, maybe do some
    // analysis???
    RemoveDeclFromSource(varDecl);
    return true;
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
    return ReplaceEnqueBuffer(callExpr, false);
  } break;
  case OpenCL::HostFuncs::clEnqueueReadBuffer: {
    return ReplaceEnqueBuffer(callExpr, true);
  } break;
  case OpenCL::HostFuncs::clReleaseMemObject: {
    ReplaceReleaseMemObject(callExpr);
  } break;
  default: {
  } break;
  }

  return false;
}

bool OpenHipifyHostFA::ReplaceCreateBuffer(const CallExpr *callExpr) {
  const auto callExprParIter = AST->getParents(*callExpr).begin();
  // Grab parent of callExpr
  // TODO: Support other cases than just vardecl, e.g. binary expression
  const BinaryOperator *binOp;
  binOp = callExprParIter->get<BinaryOperator>();
  if (binOp)
    return ReplaceCreateBufferBinOp(callExpr, binOp);

  const VarDecl *varDecl;
  varDecl = callExprParIter->get<VarDecl>();
  if (varDecl)
    return ReplaceCreateBufferVarDecl(callExpr, varDecl);

  return false;
}

bool OpenHipifyHostFA::ReplaceCreateBufferVarDecl(
    const clang::CallExpr *cBufExpr, const clang::VarDecl *varDecl) {
  // Renaming the type from cl_mem -> void*
  SourceLocation typeBeginLoc = varDecl->getBeginLoc();
  std::string typeStr = varDecl->getTypeSourceInfo()->getType().getAsString();
  ct::Replacement typeReplacement(*SM, typeBeginLoc, typeStr.length(),
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
  SourceLocation equalLoc =
      LexForTokenLocation(varDecl->getBeginLoc(), clang::tok::equal);
  std::string splitExpr;
  llvm::raw_string_ostream splitExprStr(splitExpr);
  splitExprStr << HIP::EOL;

  // cl_mem mem = clCreateBuffer(...);
  // cl_mem mem ; clCreateBuffer(...);
  ct::Replacement binaryExprRepl(*SM, equalLoc, 1, splitExprStr.str());
  llvm::consumeError(m_replacements.add(binaryExprRepl));
  ReplaceCreateBufferArguments(cBufExpr, varDecl->getNameAsString());
  return true;
}

bool OpenHipifyHostFA::ReplaceCreateBufferBinOp(
    const CallExpr *cBufExpr, const clang::BinaryOperator *binOp) {

  const DeclRefExpr *varRef = dyn_cast<DeclRefExpr>(binOp->getLHS());
  if (!varRef)
    return false;

  const ValueDecl *varDecl = varRef->getDecl();
  if (!varDecl)
    return false;

  std::string varDeclStr = DeclToStr(varDecl);
  size_t clmemidx = varDeclStr.find(OpenCL::CL_MEM);
  if (clmemidx == std::string::npos)
    return false;

  SourceLocation beginDeclLoc =
      varDecl->getBeginLoc().getLocWithOffset(clmemidx);
  auto varTypeIt = m_varTypeRenameLocs.find(beginDeclLoc.getHashValue());
  if (varTypeIt != m_varTypeRenameLocs.end()) {
    return true;
  }

  m_varTypeRenameLocs.insert(beginDeclLoc.getHashValue());
  ct::Replacement typeRepl(*SM, beginDeclLoc, OpenCL::CL_MEM.length(),
                           HIP::VOID_PTR);
  llvm::consumeError(m_replacements.add(typeRepl));

  // Replace LHS of binop
  SourceLocation binOpBeginLoc = binOp->getLHS()->getExprLoc();
  SourceLocation binOpEndLoc = binOp->getRHS()->getExprLoc();
  CharSourceRange binOpRng =
      CharSourceRange::getCharRange(binOpBeginLoc, binOpEndLoc);
  ct::Replacement binOpRepl(*SM, binOpRng, "");
  llvm::consumeError(m_replacements.add(binOpRepl));

  std::string varName = varRef->getNameInfo().getName().getAsString();
  ReplaceCreateBufferArguments(cBufExpr, varName);
  return true;
}

void OpenHipifyHostFA::ReplaceCreateBufferArguments(
    const clang::CallExpr *callExpr, std::string varName) {
  // size of buffer argument extracted
  const Expr *bufSize = callExpr->getArg(2);
  std::string bufSizeExprStr = ExprToStr(bufSize);

  // Whole argument replacement
  SourceLocation argStart = callExpr->getExprLoc();
  SourceLocation argEnd = callExpr->getEndLoc();
  // Can be shortened
  CharSourceRange argRng = CharSourceRange::getCharRange(argStart, argEnd);

  std::string newArgs;
  llvm::raw_string_ostream newArgsStr(newArgs);
  newArgsStr << HIP::MALLOC << "(" << HIP::VOID_PTR_PTR_CAST << "&" << varName
             << "," << bufSizeExprStr;
  ct::Replacement argsRepl(*SM, argRng, newArgsStr.str());
  llvm::consumeError(m_replacements.add(argsRepl));
}

// TODO: Handle error return for clCreateWriteBuffer
bool OpenHipifyHostFA::ReplaceEnqueBuffer(const CallExpr *callExpr,
                                          bool isRead) {
  // Replace function name with memcpy
  RewriteFuncName(callExpr, HIP::MEMCPY);

  // Extract args at series src, dst, offset, size
  const Expr *argDst = callExpr->getArg(1);
  const Expr *argSrc = callExpr->getArg(5);
  const Expr *argOffset = callExpr->getArg(3);
  const Expr *argSize = callExpr->getArg(4);

  // Extract string for src, dst, size
  std::string argDstStr = ExprToStr(argDst);
  std::string argSrcStr = ExprToStr(argSrc);
  std::string argSizeStr = ExprToStr(argSize);

  // Evaluate offset
  std::string argDstWithOff;
  llvm::raw_string_ostream argDstWithOffStr(argDstWithOff);
  Expr::EvalResult offsetEval;
  if (argOffset->EvaluateAsInt(offsetEval, *AST)) {
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
    std::string argOffsetStr = ExprToStr(argOffset);
    argDstWithOffStr << argDstStr << "+(" << argOffsetStr << ")";
  }

  // Construct new arguments
  std::string hipMemcpyArgs;
  llvm::raw_string_ostream hipMemcpyArgsStr(hipMemcpyArgs);
  if (isRead) {
    hipMemcpyArgsStr << argSrcStr << "," << argDstWithOffStr.str();
  } else {
    hipMemcpyArgsStr << argDstWithOffStr.str() << "," << argSrcStr;
  }

  hipMemcpyArgsStr << "," << argSizeStr << ",";

  if (isRead) {
    hipMemcpyArgsStr << HIP::MEMCPY_DEVICE_HOST;
  } else {
    hipMemcpyArgsStr << HIP::MEMCPY_HOST_DEVICE;
  }

  // remove args, and replace
  SourceLocation argStart = callExpr->getArg(0)->getExprLoc();
  SourceLocation argEnd = callExpr->getEndLoc();
  CharSourceRange argRng = CharSourceRange::getCharRange(argStart, argEnd);
  ct::Replacement argsRepl(*SM, argRng, hipMemcpyArgsStr.str());
  llvm::consumeError(m_replacements.add(argsRepl));

  return true;
}

bool OpenHipifyHostFA::ReplaceReleaseMemObject(
    const clang::CallExpr *callExpr) {
  RewriteFuncName(callExpr, HIP::FREE);
  return false;
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

bool OpenHipifyHostFA::HandleRedundantFunctionCall(
    const clang::CallExpr *callExpr) {
  // Need to test if it has return value

  auto callExprParIter = AST->getParents(*callExpr).begin();
  // Grab kernel declaration
  const VarDecl *varDecl = callExprParIter->get<VarDecl>();
  if (varDecl) {
    RemoveDeclFromSource(varDecl);
  } else {
    RemoveExprFromSource(callExpr);
  }

  // Add removal for binary exporession

  return true;
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
  auto callExprParIter = AST->getParents(*callExpr).begin();
  // Grab kernel declaration
  const VarDecl *kernelDecl = callExprParIter->get<VarDecl>();
  if (!kernelDecl)
    return false;

  const Expr *kernelNameExpr = callExpr->getArg(1)->IgnoreCasts();
  const clang::StringLiteral *kernelName =
      dyn_cast<clang::StringLiteral>(kernelNameExpr);
  if (!kernelName) {
    llvm::errs() << sOpenHipify << sErr << "kernel function name at location: "
                 << kernelNameExpr->getExprLoc().printToString(*SM)
                 << " cannot be parsed."
                 << "\n";
    return false;
  }

  std::string kernelStr(kernelName->getString());
  // TODO: involve tracking with cl_program to resolve ambiguity if two
  // kernels have the same name in different files

  m_kernelTracker.InsertName(kernelDecl, kernelStr);

  // Remove kernel create from source
  RemoveDeclFromSource(kernelDecl);
  return true;
}

bool OpenHipifyHostFA::ExtractKernelDeclFromArg(
    const clang::CallExpr *callExpr, size_t argIndex,
    const clang::ValueDecl **kernelDecl) {
  const Expr *arg1 = callExpr->getArg(argIndex)->IgnoreCasts();
  const DeclRefExpr *kernelRef = dyn_cast<DeclRefExpr>(arg1);
  if (!kernelRef) {
    llvm::errs()
        << sOpenHipify << sErr
        << "kernel argument at: " << arg1->getExprLoc().printToString(*SM)
        << " is not a variable reference. This is currently unsupported."
        << "\n";
    return false;
  }

  *kernelDecl = kernelRef->getDecl();
  if (!*kernelDecl) {
    llvm::errs() << sOpenHipify << sErr << "kernel reference at: "
                 << kernelRef->getBeginLoc().printToString(*SM)
                 << " is not a variable reference."
                 << "\n";
    return false;
  }

  return true;
}

void OpenHipifyHostFA::RewriteFuncName(const clang::CallExpr *callExpr,
                                       std::string newName) {
  SourceLocation funcNameLoc = callExpr->getBeginLoc();
  std::string funcNameStr =
      callExpr->getDirectCallee()->getNameInfo().getName().getAsString();
  ct::Replacement nameReplacement(*SM, funcNameLoc, funcNameStr.length(),
                                  newName);
  llvm::consumeError(m_replacements.add(nameReplacement));
}

void OpenHipifyHostFA::RemoveDeclFromSource(const clang::Decl *decl) {
  SourceRange exprRng = decl->getSourceRange();
  RemoveStmtRangeFromSource(exprRng);
}

void OpenHipifyHostFA::RemoveExprFromSource(const clang::Expr *expr) {
  SourceRange exprRng = expr->getSourceRange();
  RemoveStmtRangeFromSource(exprRng);
}

void OpenHipifyHostFA::RemoveStmtRangeFromSource(SourceRange rng) {
  SourceLocation exprEndLoc =
      LexForTokenLocation(rng.getEnd(), clang::tok::semi).getLocWithOffset(1);

  CharSourceRange fullExprRng =
      CharSourceRange::getCharRange(rng.getBegin(), exprEndLoc);
  ct::Replacement exprRepl(*SM, fullExprRng, "");
  llvm::consumeError(m_replacements.add(exprRepl));
}

std::string OpenHipifyHostFA::ExprToStr(const clang::Expr *expr) {
  CharSourceRange rng = CharSourceRange::getTokenRange(expr->getSourceRange());
  return std::string(Lexer::getSourceText(rng, *SM, LangOptions(), nullptr));
}

std::string OpenHipifyHostFA::DeclToStr(const clang::Decl *decl) {
  CharSourceRange rng = CharSourceRange::getTokenRange(decl->getSourceRange());
  return std::string(Lexer::getSourceText(rng, *SM, LangOptions(), nullptr));
}

clang::SourceLocation
OpenHipifyHostFA::LexForTokenLocation(clang::SourceLocation beginLoc,
                                      clang::tok::TokenKind tokType) {
  const char *startBuf = SM->getCharacterData(beginLoc);
  const char *fileEndBuf =
      SM->getCharacterData(SM->getLocForEndOfFile(SM->getMainFileID()));
  Lexer lex(beginLoc, clang::LangOptions(), startBuf, startBuf, fileEndBuf);

  clang::Token tok;
  lex.LexFromRawLexer(tok);
  while (tok.isNot(tokType)) {
    lex.LexFromRawLexer(tok);
  }

  return tok.getLocation();
}

void OpenHipifyHostFA::PrettyError(clang::SourceRange loc,
                                   raw_ostream::Colors underlineCol,
                                   std::string extraInfo) {
  StringRef fileName = getCurrentFile();
  fileName.consume_front("/tmp/");
  size_t index = fileName.rfind('-');
  fileName = fileName.take_front(index);

  llvm::errs() << "<" << fileName << ">"
               << "\n";

  unsigned startLineNum = SM->getSpellingLineNumber(loc.getBegin());
  // Get previous line
  auto LineSourceLoc = [&](unsigned line) -> SourceLocation {
    return SM->translateLineCol(SM->getMainFileID(), line, 1);
  };
  SourceLocation prevLineStart = LineSourceLoc(startLineNum - 1);
  SourceLocation curLineStart = LineSourceLoc(startLineNum);
  SourceLocation nextLineStart = LineSourceLoc(startLineNum + 1);
  SourceLocation nextLineEnd = LineSourceLoc(startLineNum + 2);

  auto LineText = [&](SourceLocation start, SourceLocation end) {
    CharSourceRange rng = CharSourceRange::getTokenRange(start, end);
    return std::string(Lexer::getSourceText(rng, *SM, LangOptions(), nullptr));
  };

  std::string prevLineStr = LineText(prevLineStart, curLineStart);
  std::string curLineStr = LineText(curLineStart, nextLineStart);
  std::string nextLineStr = LineText(nextLineStart, nextLineEnd);

  auto printLine = [&](std::string str, unsigned line) {
    llvm::errs() << line << "| " << str;
  };

  printLine(prevLineStr, startLineNum - 1);
  printLine(curLineStr, startLineNum);

  unsigned exprStartCol = SM->getSpellingColumnNumber(loc.getBegin());
  unsigned exprEndCol = SM->getSpellingColumnNumber(loc.getEnd());
  std::string curColNumString = std::to_string(startLineNum);
  for (unsigned i = 0; i < curColNumString.size(); i++) {
    llvm::errs() << " ";
  }
  llvm::errs() << "| ";
  for (unsigned i = 1; i < exprStartCol; i++) {
    llvm::errs() << " ";
  }
  for (unsigned i = exprStartCol; i < exprEndCol; i++) {
    llvm::WithColor(llvm::errs(), underlineCol, true) << "^";
  }

  if (extraInfo.size() > 0) {
    llvm::WithColor(llvm::errs(), underlineCol, true) << " <--- " << extraInfo;
  }

  llvm::errs() << "\n";
  printLine(nextLineStr, startLineNum + 1);
  llvm::errs() << "\n";
}
