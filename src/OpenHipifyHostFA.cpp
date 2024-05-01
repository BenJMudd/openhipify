#include "OpenHipifyHostFA.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/WithColor.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";
const StringRef B_VAR_DECL = "varDecl";
const StringRef B_DECL_STMT_CULL = "declStmt";
const StringRef B_VAR_DECL_CULL = "varDeclCull";
const StringRef B_BIN_OP_REF = "binOpRef";
const StringRef B_ERROR = "bError";
#define ERR_BOLD_STR llvm::WithColor(llvm::errs(), raw_ostream::WHITE, true)

std::unique_ptr<ASTConsumer>
OpenHipifyHostFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  SM = &getCompilerInstance().getSourceManager();
  AST = &getCompilerInstance().getASTContext();

  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  // Removal of redundant assignment expresion
  m_finder->addMatcher(
      binaryOperator(
          isAssignmentOperator(), isExpansionInMainFile(),
          hasLHS(declRefExpr(anyOf(hasType(asString(OpenCL::CL_PROGRAM)),
                                   hasType(asString(OpenCL::CL_DEVICE_ID)),
                                   hasType(asString(OpenCL::CL_CONTEXT))))))

          .bind(B_BIN_OP_REF),
      this);

  // match on error statements (e.g. ret == CL_SUCCESS)
  m_finder->addMatcher(
      binaryOperator(isComparisonOperator(), isExpansionInMainFile(),
                     hasLHS(ignoringImpCasts(
                         declRefExpr(hasType(asString(OpenCL::CL_INT))))))
          .bind(B_ERROR),
      this);

  // removal of redundant variable declarations
  m_finder->addMatcher(declStmt(isExpansionInMainFile(),
                                hasDescendant(varDecl(anyOf(
                                    hasType(asString(OpenCL::CL_CONTEXT)),
                                    hasType(asString(OpenCL::CL_PROGRAM)),
                                    hasType(asString(OpenCL::CL_KERNEL)),
                                    hasType(asString(OpenCL::CL_DEVICE_ID)),
                                    hasType(asString(OpenCL::CL_PLATFORM_ID)),
                                    hasType(asString(OpenCL::CL_COMMAND_QUEUE)),
                                    hasType(asString(OpenCL::CL_UINT)),
                                    hasType(asString(OpenCL::CL_INT))))))
                           .bind(B_DECL_STMT_CULL),
                       this);

  return m_finder->newASTConsumer();
}

void OpenHipifyHostFA::EndSourceFileAction() {
  m_kernelTracker.Finalise(*SM);
  std::set<std::string> kernelFilesUsed;

  for (const auto &[kernelDecl, kInfo] : m_kernelTracker.GetKernelInfo()) {
    // args are indexed in
    // [..(arg expression string, has addr op stripped)]

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

    auto HandlLaunchExpr = [&](KernelLaunch kLaunch) {
      const CallExpr *launchKernelExpr = kLaunch.first;
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
      if (kLaunch.second == OpenCL::HostFuncs::clEnqueueNDRangeKernel) {
        GenerateNDKernelLaunch(launchKernelExpr, kInfo, kDef, args,
                               kernelFilesUsed);
      } else if (kLaunch.second == OpenCL::HostFuncs::clEnqueueTask) {
        GenerateEnqueueTaskLaunch(launchKernelExpr, kInfo, kDef, args,
                                  kernelFilesUsed);
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
      std::string kernelArgStr;
      bool isAddrStripped = StripAddressOfVar(kernelArgExpr, kernelArgStr);

      args[argPos] = ArgInfo(kernelArgStr, isAddrStripped, toCast);
      argsUse[argPos] = true;
      if (argPos + 1 > numArgs) {
        numArgs = argPos + 1;
      }

      // Remove expression
      // test if binary
      const Expr *rmvExpr = GetBinaryExprParenOrSelf(setArgExpr);
      RemoveExprFromSource(rmvExpr);
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
        baseScopeStmt = SearchParentScope(*argIter);
        // auto argScopeNode = AST->getParents(*((*argIter)->IgnoreCasts()));
        // baseScopeStmt = argScopeNode.begin()->get<Stmt>();
        baseScopeExpr = *argIter;
      }

      auto CheckScope = [&](const Expr *expr) -> bool {
        auto *exprScopeStmt = SearchParentScope(expr);
        if (exprScopeStmt != baseScopeStmt) {
          ERR_BOLD_STR << sOpenHipify;
          llvm::WithColor(llvm::errs(), raw_ostream::YELLOW, true) << sWarn;
          ERR_BOLD_STR << "Kernel function called at a different scope from "
                          "initial use.This can result in inconsistent kernel "
                          "launch generation, removing and skipping...\n ";
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
        if (!CheckScope(launchIter->first)) {
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
      KernelLaunch kLaunch = *launchIter;
      const CallExpr *launchKernelExpr = kLaunch.first;
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

        HandlLaunchExpr(kLaunch);
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

  if (DeclarationStmt(res))
    return;

  if (BinaryOpDeclRef(res))
    return;

  if (ErrorComparison(res))
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

  iter = OpenCL::HOST_GENERIC_FUNCS.find(funcSearch->second);
  if (iter != OpenCL::HOST_GENERIC_FUNCS.end()) {
    // generic function found
    HandleGenericFunctionCall(callExpr, *iter);
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
  // const VarDecl *varDecl = res.Nodes.getNodeAs<VarDecl>(B_VAR_DECL);
  // if (!varDecl)
  //   return false;

  // const IdentifierInfo *typeIdentifier =
  //     varDecl->getType().getBaseTypeIdentifier();
  // if (!typeIdentifier)
  //   return false;

  // std::string varType(typeIdentifier->getName());
  // auto iter = OpenCL::CL_TYPES.find(varType);
  // if (iter != OpenCL::CL_TYPES.end()) {
  //   // Remove statement containing redundant opencl types
  //   // TODO: This is obviously an awful way of doing this, maybe do some
  //   // analysis???
  //   llvm::errs() << sOpenHipify << "Removing...";
  //   PrettyError(varDecl->getSourceRange(), raw_ostream::YELLOW);
  //   RemoveDeclFromSource(varDecl);
  //   return true;
  // }
  // return false;
  const VarDecl *varDecl = res.Nodes.getNodeAs<VarDecl>(B_VAR_DECL_CULL);
  if (!varDecl)
    return false;
  // look for uses in matcher maybe?
  return true;
}

bool OpenHipifyHostFA::DeclarationStmt(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const DeclStmt *declToCull = res.Nodes.getNodeAs<DeclStmt>(B_DECL_STMT_CULL);
  if (!declToCull) {
    return false;
  }

  std::string type;
  if (ExtractType(*(declToCull->decl_begin()), type) &&
      type == OpenCL::CL_INT) {
    // Rename cl_int to hipError_t for use in future error cases
    SourceLocation typeBeginLoc = declToCull->getBeginLoc();
    auto slIter = m_clIntRenames.find(typeBeginLoc);
    if (slIter == m_clIntRenames.end()) {
      // Type not renamed
      m_clIntRenames.insert(typeBeginLoc);
      ct::Replacement errRepl(*SM, typeBeginLoc, OpenCL::CL_INT.length(),
                              HIP::ERROR);
      llvm::consumeError(m_replacements.add(errRepl));
    }
    if (!declToCull->isSingleDecl()) {
      llvm::errs() << sOpenHipify << sWarn
                   << "Multiple cl_int declarations not supported currently\n";
      return true;
    }

    ct::Replacement succInclusion(*SM, declToCull->getEndLoc(), 0,
                                  " = hipSuccess");
    llvm::consumeError(m_replacements.add(succInclusion));
    return true;
  }

  RemoveStmtRangeFromSource(declToCull->getSourceRange());
  return true;
}

bool OpenHipifyHostFA::BinaryOpDeclRef(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const clang::BinaryOperator *binOp =
      res.Nodes.getNodeAs<clang::BinaryOperator>(B_BIN_OP_REF);
  if (!binOp) {
    return false;
  }

  RemoveExprFromSource(binOp);
  return true;
}

bool OpenHipifyHostFA::ErrorComparison(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const clang::BinaryOperator *binOp =
      res.Nodes.getNodeAs<clang::BinaryOperator>(B_ERROR);
  if (!binOp) {
    return false;
  }

  const Expr *rhs = binOp->getRHS();
  Expr::EvalResult rhsEval;

  if (rhs->EvaluateAsInt(rhsEval, *AST)) {
    APSInt errCase = rhsEval.Val.getInt();
    if (errCase == OpenCL::CL_SUCCESS_VAL) {
      SourceLocation beginLoc = SM->getExpansionLoc(rhs->getExprLoc());
      ct::Replacement rhsRepl(*SM, beginLoc, OpenCL::CL_SUCCESS.length(),
                              HIP::HIP_SUCCESS);
      llvm::consumeError(m_replacements.add(rhsRepl));
    }
  } else {
    // TODO: better error
    llvm::errs() << sOpenHipify << sErr << "Unable to evalute rhs\n";
    return true;
  }

  return true;
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
  const Expr *errExpr = callExpr->getArg(4);
  const Expr *errNullTest =
      errExpr->IgnoreCasts()->IgnoreParens()->IgnoreCasts();
  bool errTest = true;
  std::string errStr;
  std::string strippedErr;
  if (dyn_cast<IntegerLiteral>(errNullTest)) {
    errTest = false;
  } else {
    if (StripAddressOfVar(errExpr, strippedErr)) {
      errStr = strippedErr;
    } else {
      errStr = "*(" + ExprToStr(errExpr) + ")";
    }
  }

  const auto callExprParIter = AST->getParents(*callExpr).begin();
  // Grab parent of callExpr
  // TODO: Support other cases than just vardecl, e.g. binary expression
  const BinaryOperator *binOp;
  binOp = callExprParIter->get<BinaryOperator>();
  if (binOp)
    return ReplaceCreateBufferBinOp(callExpr, binOp, errTest, errStr);

  const VarDecl *varDecl;
  varDecl = callExprParIter->get<VarDecl>();
  if (varDecl)
    return ReplaceCreateBufferVarDecl(callExpr, varDecl, errTest, errStr);

  return false;
}

bool OpenHipifyHostFA::ReplaceCreateBufferVarDecl(
    const clang::CallExpr *cBufExpr, const clang::VarDecl *varDecl, bool errVar,
    const std::string &errVarStr) {
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
  if (errVar) {
    splitExprStr << errVarStr << "=";
  }

  // cl_mem mem = clCreateBuffer(...);
  // cl_mem mem ; clCreateBuffer(...);
  ct::Replacement binaryExprRepl(*SM, equalLoc, 1, splitExprStr.str());
  llvm::consumeError(m_replacements.add(binaryExprRepl));
  ReplaceCreateBufferArguments(cBufExpr, varDecl->getNameAsString());
  return true;
}

bool OpenHipifyHostFA::ReplaceCreateBufferBinOp(
    const CallExpr *cBufExpr, const clang::BinaryOperator *binOp, bool errVar,
    const std::string &errVarStr) {
  const DeclRefExpr *varRef = dyn_cast<DeclRefExpr>(binOp->getLHS());
  if (!varRef)
    return false;

  const ValueDecl *valDecl = varRef->getDecl();
  if (!valDecl)
    return false;

  const VarDecl *varDecl = dyn_cast<VarDecl>(valDecl);
  if (!varDecl)
    return false;

  std::string varName = varRef->getNameInfo().getName().getAsString();
  std::string declStr = DeclToStr(varDecl);
  const char *searchStr = strstr(declStr.c_str(), varName.c_str());
  while (searchStr) {
    char nextChar = *(searchStr + varName.length());
    if (isalpha(nextChar)) {
      searchStr = strstr(searchStr + 1, varName.c_str());
      continue;
    }

    break;
  }

  if (!searchStr) {
    return false;
  }
  unsigned offset = searchStr - declStr.c_str();

  // prefixing with pointer type
  ct::Replacement ptrRepl(*SM, varDecl->getBeginLoc().getLocWithOffset(offset),
                          0, "*");
  llvm::consumeError(m_replacements.add(ptrRepl));

  std::string varDeclStr = DeclToStr(varDecl);
  size_t clmemidx = varDeclStr.find(OpenCL::CL_MEM);
  if (clmemidx == std::string::npos)
    return false;

  SourceLocation beginDeclLoc =
      varDecl->getBeginLoc().getLocWithOffset(clmemidx);
  auto varTypeIt = m_varTypeRenameLocs.find(beginDeclLoc.getHashValue());
  if (varTypeIt == m_varTypeRenameLocs.end()) {
    m_varTypeRenameLocs.insert(beginDeclLoc.getHashValue());
    ct::Replacement typeRepl(*SM, beginDeclLoc, OpenCL::CL_MEM.length(),
                             HIP::VOID);
    llvm::consumeError(m_replacements.add(typeRepl));
  }

  // Replace LHS of binop
  SourceLocation binOpBeginLoc = binOp->getLHS()->getExprLoc();
  SourceLocation binOpEndLoc = binOp->getRHS()->getExprLoc();
  CharSourceRange binOpRng =
      CharSourceRange::getCharRange(binOpBeginLoc, binOpEndLoc);
  std::string binOpReplStr = "";
  if (errVar) {
    binOpReplStr = errVarStr + "=";
  }

  ct::Replacement binOpRepl(*SM, binOpRng, binOpReplStr);
  llvm::consumeError(m_replacements.add(binOpRepl));

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
  ReplaceCBuffPiggyBack(callExpr, varName, bufSizeExprStr);
}

void OpenHipifyHostFA::ReplaceCBuffPiggyBack(const clang::CallExpr *callExpr,
                                             std::string hostBuf,
                                             std::string bytes) {
  // Test if writebuffer is piggybacked onto the call
  const Expr *bufExpr = callExpr->getArg(3)->IgnoreParens()->IgnoreCasts();
  auto *bufRef = dyn_cast<DeclRefExpr>(bufExpr);
  if (!bufRef) {
    // No piggyback copy, return
    return;
  }

  std::string destBuf = ExprToStr(bufRef);
  std::string pBack;
  llvm::raw_string_ostream pBackStr(pBack);
  pBackStr << HIP::MEMCPY << "(" << hostBuf << "," << destBuf << "," << bytes
           << "," << HIP::MEMCPY_HOST_DEVICE << ");";
  SourceLocation endLoc =
      LexForTokenLocation(callExpr->getEndLoc(), clang::tok::semi)
          .getLocWithOffset(1);
  ct::Replacement pRepl(*SM, endLoc, 0, pBackStr.str());
  llvm::consumeError(m_replacements.add(pRepl));
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

  // test if error is checked

  EnsureBinExprIsAssign(callExpr);
  return true;
}

bool OpenHipifyHostFA::ReplaceReleaseMemObject(
    const clang::CallExpr *callExpr) {
  RewriteFuncName(callExpr, HIP::FREE);
  EnsureBinExprIsAssign(callExpr);
  return false;
}

bool OpenHipifyHostFA::HandleKernelFunctionCall(const CallExpr *callExpr,
                                                OpenCL::HostFuncs func) {
  switch (func) {
  case OpenCL::HostFuncs::clSetKernelArg: {
    return TrackKernelSetArg(callExpr);
  } break;
  case OpenCL::HostFuncs::clEnqueueTask:
  case OpenCL::HostFuncs::clEnqueueNDRangeKernel: {
    return TrackKernelLaunch(callExpr, func);
  } break;
  case OpenCL::HostFuncs::clCreateKernel: {
    return TrackKernelCreate(callExpr);
  } break;
  default: {
  } break;
  }

  return false;
}

bool OpenHipifyHostFA::HandleGenericFunctionCall(
    const clang::CallExpr *callExpr, OpenCL::HostFuncs func) {
  // match on clgetkernelworkgroup, send to aux func
  switch (func) {
  case OpenCL::HostFuncs::clGetCWGInfo: {
    return ReplaceGetKWGGeneric(*callExpr);
  } break;
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
    return true;
  }

  const BinaryOperator *binOp;
  binOp = callExprParIter->get<BinaryOperator>();
  if (binOp) {
    RemoveExprFromSource(binOp);
    return true;
  }

  RemoveExprFromSource(callExpr);
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

bool OpenHipifyHostFA::TrackKernelLaunch(const clang::CallExpr *callExpr,
                                         OpenCL::HostFuncs func) {
  const ValueDecl *kernelDecl;
  if (!ExtractKernelDeclFromArg(callExpr, 1, &kernelDecl)) {
    return false;
  }

  m_kernelTracker.InsertLaunch(kernelDecl, callExpr, func);
  return true;
}

bool OpenHipifyHostFA::TrackKernelCreate(const clang::CallExpr *callExpr) {
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

  // Grab kernel declaration
  auto callExprParIter = AST->getParents(*callExpr).begin();
  const VarDecl *kernelDecl = callExprParIter->get<VarDecl>();
  if (kernelDecl) {
    m_kernelTracker.InsertName(kernelDecl, kernelStr);
    RemoveDeclFromSource(kernelDecl);
  } else {
    const BinaryOperator *binOp;
    binOp = callExprParIter->get<BinaryOperator>();
    if (binOp)
      return TrackKernelCreateBinop(callExpr, binOp, kernelStr);
  }

  return false;
}

bool OpenHipifyHostFA::TrackKernelCreateBinop(
    const clang::CallExpr *callExpr, const clang::BinaryOperator *binOp,
    std::string kernelName) {
  const DeclRefExpr *varRef = dyn_cast<DeclRefExpr>(binOp->getLHS());
  if (!varRef)
    return false;

  const ValueDecl *valueDecl = varRef->getDecl();
  if (!valueDecl)
    return false;

  const VarDecl *kernelDecl = valueDecl->getPotentiallyDecomposedVarDecl();
  if (!kernelDecl)
    return false;

  m_kernelTracker.InsertName(kernelDecl, kernelName);
  RemoveExprFromSource(binOp);
  return true;
}

void OpenHipifyHostFA::GenerateNDKernelLaunch(
    const clang::CallExpr *launchKernelExpr,
    const KernelLaunchTracker::KernelInfo &kInfo, const KernelDefinition *kDef,
    const std::vector<ArgInfo> &args, std::set<std::string> &kernelFilesUsed) {
  // Handle launching kernel case
  // Extract arg 4,5 for dimensions
  const Expr *numBlocksExpr = launchKernelExpr->getArg(4);
  const Expr *blockSizeExpr = launchKernelExpr->getArg(5);

  std::string numBlocksStr;
  bool isNumBlocksAddrStripped = StripAddressOfVar(numBlocksExpr, numBlocksStr);
  std::string blockSizeStr;
  bool isBlockSizeAddrStripped = StripAddressOfVar(blockSizeExpr, blockSizeStr);
  GenerateGenericKernelLaunch(launchKernelExpr, numBlocksStr, blockSizeStr,
                              isNumBlocksAddrStripped, isBlockSizeAddrStripped,
                              kInfo, kDef, args, kernelFilesUsed);
}

void OpenHipifyHostFA::GenerateEnqueueTaskLaunch(
    const clang::CallExpr *launchKernelExpr,
    const KernelLaunchTracker::KernelInfo &kInfo, const KernelDefinition *kDef,
    const std::vector<ArgInfo> &args, std::set<std::string> &kernelFilesUsed) {
  GenerateGenericKernelLaunch(launchKernelExpr, "1", "1", true, true, kInfo,
                              kDef, args, kernelFilesUsed);
}

void OpenHipifyHostFA::GenerateGenericKernelLaunch(
    const clang::CallExpr *launchKernelExpr, const std::string &numBlocks,
    const std::string &blockSize, bool nBlockAddrStripped,
    bool sBlockAddrStripped, const KernelLaunchTracker::KernelInfo &kInfo,
    const KernelDefinition *kDef, const std::vector<ArgInfo> &args,
    std::set<std::string> &kernelFilesUsed) {
  // Replace function name with hip equivalent
  SourceLocation funcNameBLoc =
      GetBinaryExprParenOrSelf(launchKernelExpr)->getBeginLoc();
  SourceLocation funcNameELoc =
      LexForTokenLocation(funcNameBLoc, clang::tok::l_paren);
  CharSourceRange nameReplRng =
      CharSourceRange::getCharRange(funcNameBLoc, funcNameELoc);

  ct::Replacement nameReplacement(*SM, nameReplRng, HIP::LAUNCHKERNELGGL);
  llvm::consumeError(m_replacements.add(nameReplacement));

  // Construct new args
  std::string launchKernelArgs;
  llvm::raw_string_ostream launchKernelArgsStr(launchKernelArgs);
  launchKernelArgsStr << kInfo.funcName << ","
                      << "dim3(";
  if (nBlockAddrStripped) {
    launchKernelArgsStr << numBlocks << "),";
  } else {
    launchKernelArgsStr << "*(" << numBlocks << ")),";
  }

  launchKernelArgsStr << "dim3(";
  if (sBlockAddrStripped) {
    launchKernelArgsStr << blockSize << "),";
  } else {
    launchKernelArgsStr << "*(" << blockSize << ")),";
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
        kernelNameExtrInfoStr << "kernel: " << kInfo.funcName << " (untracked)";

        PrettyError(
            {launchKernelExpr->getArg(1)->getBeginLoc(),
             launchKernelExpr->getArg(2)->getBeginLoc().getLocWithOffset(-1)},
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
}

bool OpenHipifyHostFA::ReplaceGetKWGGeneric(const clang::CallExpr &callExpr) {
  // Extract 3rd argument, evaluate, generate switch statement and blast into
  // next function
  const Expr *paramExpr = callExpr.getArg(2);
  if (!paramExpr)
    return false;

  Expr::EvalResult paramEval;
  // TODO: add aux function for false
  if (!paramExpr->EvaluateAsInt(paramEval, *AST))
    return false;

  // generate initialisation of HIP props (could use scope stuff)

  std::string HIPPropRepl;
  llvm::raw_string_ostream replStr(HIPPropRepl);

  bool propsExists = false;
  for (auto *initScope : m_dPropScopes) {
    if (IsInScope(callExpr, *initScope)) {
      propsExists = true;
      break;
    }
  }

  if (!propsExists) {
    m_dPropScopes.emplace_back(SearchParentScope(&callExpr));
    replStr << HIP::INIT_D_PROPS;
  }

  APSInt paramFlag = paramEval.Val.getInt();
  if (paramFlag == OpenCL::CL_KERNEL_WORK_GROUP_SIZE) {
    // use hip prop obj
    const Expr *retVarExpr = callExpr.getArg(4);
    if (!retVarExpr)
      return false;

    std::string retVar;
    StripAddressOfVar(retVarExpr, retVar);
    replStr << retVar << "=" << HIP::PROPS_OBJ << "."
            << HIP::PROPS_MTHREAD_P_BLOCK << ";";
  }

  // Replacement
  const Expr *replExpr = GetBinaryExprParenOrSelf(&callExpr);
  SourceLocation endLoc =
      LexForTokenLocation(replExpr->getEndLoc(), clang::tok::semi)
          .getLocWithOffset(1);
  CharSourceRange replRng =
      CharSourceRange::getCharRange({replExpr->getBeginLoc(), endLoc});
  ct::Replacement repl(*SM, replRng, replStr.str());
  llvm::consumeError(m_replacements.add(repl));

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

const Stmt *OpenHipifyHostFA::SearchParentScope(const clang::Stmt *base) {
  const Stmt *cur = base;
  while (cur) {
    auto argScopeNode = AST->getParents(*cur);
    const Stmt *baseScopeStmt = argScopeNode.begin()->get<Stmt>();
    if (!baseScopeStmt) {
      return nullptr;
    }

    if (auto *baseScope = dyn_cast<CompoundStmt>(baseScopeStmt)) {
      return baseScope;
    }
    cur = baseScopeStmt;
  }

  return nullptr;
}

bool OpenHipifyHostFA::IsInScope(const clang::Stmt &base,
                                 const clang::Stmt &tScope) {
  const Stmt *cur = &base;
  while (cur) {
    const Stmt *curScope = SearchParentScope(cur);
    if (!curScope) {
      return false;
    }

    if (curScope == &tScope) {
      return true;
    }

    cur = curScope;
  }

  return false;
}

const clang::BinaryOperator *
OpenHipifyHostFA::GetBinaryExprParent(const clang::Expr *base) {
  auto parentNode = AST->getParents(*base);
  return parentNode.begin()->get<clang::BinaryOperator>();
}

const clang::Expr *
OpenHipifyHostFA::GetBinaryExprParenOrSelf(const clang::Expr *base) {
  const Expr *binOp = GetBinaryExprParent(base);
  if (binOp) {
    return binOp;
  }

  return base;
}

void OpenHipifyHostFA::RemoveBinExprIfPossible(const clang::Expr *base) {
  const Expr *parent = GetBinaryExprParenOrSelf(base);
  if (parent == base) {
    return;
  }

  SourceLocation binStart = parent->getBeginLoc();
  SourceLocation binEnd = base->getBeginLoc();
  CharSourceRange binRng = CharSourceRange::getCharRange(binStart, binEnd);
  ct::Replacement binRepl(*SM, binRng, "");
  llvm::consumeError(m_replacements.add(binRepl));
}

void OpenHipifyHostFA::EnsureBinExprIsAssign(const clang::Expr *base) {
  const BinaryOperator *binOp = GetBinaryExprParent(base);
  if (!binOp) {
    return;
  }

  if (binOp->getOpcode() == BO_Assign)
    return;

  const Expr *lhs = binOp->getLHS();
  SourceLocation binStart = lhs->getExprLoc();
  SourceLocation binEnd = binOp->getRHS()->getExprLoc();
  CharSourceRange binRng = CharSourceRange::getCharRange(binStart, binEnd);
  ct::Replacement binRepl(*SM, binRng, ExprToStr(lhs) + "=");
  llvm::consumeError(m_replacements.add(binRepl));
}

bool OpenHipifyHostFA::StripAddressOfVar(const Expr *var, std::string &ret) {
  // Test if & is used to describe arg
  const UnaryOperator *unaryOp = dyn_cast<UnaryOperator>(var->IgnoreCasts());
  if (unaryOp && unaryOp->getOpcode() == UO_AddrOf) {
    // Found & prepend
    ret = ExprToStr(unaryOp->getSubExpr());
    return true;
  } else {
    ret = ExprToStr(var);
    return false;
  }

  return false;
}

std::string OpenHipifyHostFA::ExprToStr(const clang::Expr *expr) {
  CharSourceRange rng = CharSourceRange::getTokenRange(expr->getSourceRange());
  return std::string(Lexer::getSourceText(rng, *SM, LangOptions(), nullptr));
}

bool OpenHipifyHostFA::ExtractType(const clang::Decl *decl, std::string &ret) {
  std::string declStr = DeclToStr(decl);
  size_t spaceIdx = declStr.find(' ');
  if (spaceIdx == std::string::npos) {
    return false;
  }

  ret = declStr.substr(0, spaceIdx);
  return true;
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
