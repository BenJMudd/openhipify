#pragma once

#include "HIPDefs.h"
#include "OpenClDefs.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Core/Replacement.h"

using namespace llvm;
namespace ct = clang::tooling;
namespace ASTMatch = clang::ast_matchers;

class OpenHipifyKernelFA : public clang::ASTFrontendAction,
                           public ASTMatch::MatchFinder::MatchCallback {
  using MatchFinderPtr = std::unique_ptr<ASTMatch::MatchFinder>;

public:
  explicit OpenHipifyKernelFA(ct::Replacements &replacements)
      : clang::ASTFrontendAction(), m_replacements(replacements) {}

private:
  void run(const ASTMatch::MatchFinder::MatchResult &res) override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override;

  bool OpenCLFunctionCall(const ASTMatch::MatchFinder::MatchResult &res);
  bool OpenCLKernelFunctionDecl(const ASTMatch::MatchFinder::MatchResult &res);

  void InsertAuxFunction(const clang::SourceManager &srcManager,
                         clang::CharSourceRange funcNameRng,
                         HIP::AUX_FUNC_ID func);

  bool
  ReplaceGET_GENERIC_THREAD_ID(const clang::CallExpr &callExpr,
                               const ASTMatch::MatchFinder::MatchResult &res,
                               OpenCL::KernelFuncs funcIdent);
  bool ReplaceBARRIER(const clang::CallExpr &callExpr,
                      const ASTMatch::MatchFinder::MatchResult &res);

  std::set<HIP::AUX_FUNC_ID> m_auxFunctions;

  MatchFinderPtr m_finder;
  ct::Replacements &m_replacements;
};