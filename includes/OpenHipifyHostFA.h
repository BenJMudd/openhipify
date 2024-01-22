#pragma once

#include "OpenClDefs.h"
#include "OpenHipifyKernelFA.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Core/Replacement.h"

using namespace llvm;
namespace ct = clang::tooling;
namespace ASTMatch = clang::ast_matchers;

class OpenHipifyHostFA : public clang::ASTFrontendAction,
                         public ASTMatch::MatchFinder::MatchCallback {
  using MatchFinderPtr = std::unique_ptr<ASTMatch::MatchFinder>;

public:
  explicit OpenHipifyHostFA(ct::Replacements &replacements,
                            OpenHipifyKernelFA::KernelFuncMap &kFuncMap)
      : clang::ASTFrontendAction(), m_kernelFuncMap(kFuncMap),
        m_replacements(replacements) {}

private:
  void run(const ASTMatch::MatchFinder::MatchResult &res) override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override;

  bool FunctionCall(const ASTMatch::MatchFinder::MatchResult &res);

  bool HandleMemoryFunctionCall(const clang::CallExpr *callExpr,
                                OpenCL::HostFuncs func);

  OpenHipifyKernelFA::KernelFuncMap &m_kernelFuncMap;

  MatchFinderPtr m_finder;
  ct::Replacements &m_replacements;
};