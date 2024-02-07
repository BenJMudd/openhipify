#pragma once

#include "KernelTracking.h"
#include "OpenClDefs.h"
#include "OpenHipifyKernelFA.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/CompilerInstance.h"
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
  void EndSourceFileAction() override;

  bool FunctionCall(const ASTMatch::MatchFinder::MatchResult &res);

  bool HandleMemoryFunctionCall(const clang::CallExpr *callExpr,
                                OpenCL::HostFuncs func);
  bool HandleKernelFunctionCall(const clang::CallExpr *callExpr,
                                OpenCL::HostFuncs func);

  // Memory function call replacements
  bool ReplaceCreateBuffer(const clang::CallExpr *callExpr);
  bool ReplaceEnqueWriteBuffer(const clang::CallExpr *callExpr);

  // Kernel function call replacements
  bool TrackKernelSetArg(const clang::CallExpr *callExpr);
  bool TrackKernelLaunch(const clang::CallExpr *callExpr);
  bool TrackKernelCreate(const clang::CallExpr *callExpr);

  bool ExtractKernelDeclFromArg(const clang::CallExpr *callExpr,
                                size_t argIndex,
                                const clang::ValueDecl **kernelDecl);

  std::string ExprToStr(const clang::Expr *expr);
  clang::SourceLocation LexForTokenLocation(clang::SourceLocation beginLoc,
                                            clang::tok::TokenKind tokType);

  OpenHipifyKernelFA::KernelFuncMap &m_kernelFuncMap;

  ct::Replacements &m_replacements;
  const clang::SourceManager *SM;
  clang::ASTContext *AST;
  MatchFinderPtr m_finder;
  KernelTracker m_kernelTracker;
};