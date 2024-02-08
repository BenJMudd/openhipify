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
                            std::map<std::string, KernelDefinition> &kFuncMap)
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
  bool HandleRedundantFunctionCall(const clang::CallExpr *callExpr);

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

  void RemoveDeclFromSource(const clang::Decl *decl);
  void RemoveExprFromSource(const clang::Expr *decl);
  void RemoveStmtRangeFromSource(clang::SourceRange rng);

  std::string ExprToStr(const clang::Expr *expr);
  std::string DeclToStr(const clang::Decl *decl);
  clang::SourceLocation LexForTokenLocation(clang::SourceLocation beginLoc,
                                            clang::tok::TokenKind tokType);

  std::map<std::string, KernelDefinition> &m_kernelFuncMap;

  ct::Replacements &m_replacements;
  const clang::SourceManager *SM;
  clang::ASTContext *AST;
  MatchFinderPtr m_finder;
  KernelLaunchTracker m_kernelTracker;
};