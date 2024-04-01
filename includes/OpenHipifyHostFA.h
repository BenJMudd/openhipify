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
  // file name -> (kernel name -> kernel definition)
  using KernelIncludeTracker =
      std::map<std::string, std::map<std::string, std::string>>;

  explicit OpenHipifyHostFA(
      ct::Replacements &replacements,
      std::map<std::string, const KernelDefinition> &kFuncMap,
      KernelIncludeTracker &kernelIncludeTracker)
      : clang::ASTFrontendAction(), m_kernelFuncMap(kFuncMap),
        m_replacements(replacements),
        m_kernelIncludeTracker(kernelIncludeTracker) {}

private:
  void run(const ASTMatch::MatchFinder::MatchResult &res) override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override;
  void EndSourceFileAction() override;

  bool FunctionCall(const ASTMatch::MatchFinder::MatchResult &res);
  bool VariableDeclaration(const ASTMatch::MatchFinder::MatchResult &res);
  bool DeclarationStmt(const ASTMatch::MatchFinder::MatchResult &res);
  bool BinaryOpDeclRef(const ASTMatch::MatchFinder::MatchResult &res);

  bool HandleMemoryFunctionCall(const clang::CallExpr *callExpr,
                                OpenCL::HostFuncs func);
  bool HandleKernelFunctionCall(const clang::CallExpr *callExpr,
                                OpenCL::HostFuncs func);
  bool HandleGenericFunctionCall(const clang::CallExpr *callExpr,
                                 OpenCL::HostFuncs func);
  bool HandleRedundantFunctionCall(const clang::CallExpr *callExpr);

  // Memory function call replacements
  bool ReplaceCreateBuffer(const clang::CallExpr *callExpr);
  bool ReplaceCreateBufferVarDecl(const clang::CallExpr *cBufExpr,
                                  const clang::VarDecl *varDecl);
  bool ReplaceCreateBufferBinOp(const clang::CallExpr *cBufExpr,
                                const clang::BinaryOperator *binOp);
  void ReplaceCreateBufferArguments(const clang::CallExpr *callExpr,
                                    std::string varName);
  void ReplaceCBuffPiggyBack(const clang::CallExpr *callExpr,
                             std::string hostBuf, std::string bytes);
  bool ReplaceEnqueBuffer(const clang::CallExpr *callExpr, bool isRead);
  bool ReplaceReleaseMemObject(const clang::CallExpr *callExpr);

  // Kernel function call replacements
  bool TrackKernelSetArg(const clang::CallExpr *callExpr);
  bool TrackKernelLaunch(const clang::CallExpr *callExpr);
  bool TrackKernelCreate(const clang::CallExpr *callExpr);
  bool TrackKernelCreateBinop(const clang::CallExpr *callExpr,
                              const clang::BinaryOperator *binOp,
                              std::string kernelName);

  // Generic function call replacements
  bool ReplaceGetKWGGeneric(const clang::CallExpr &callExpr);

  bool ExtractKernelDeclFromArg(const clang::CallExpr *callExpr,
                                size_t argIndex,
                                const clang::ValueDecl **kernelDecl);

  void RewriteFuncName(const clang::CallExpr *callExpr, std::string newName);
  void RemoveDeclFromSource(const clang::Decl *decl);
  void RemoveExprFromSource(const clang::Expr *decl);
  void RemoveStmtRangeFromSource(clang::SourceRange rng);

  const clang::Stmt *SearchParentScope(const clang::Stmt *base);
  bool IsInScope(const clang::Stmt &base, const clang::Stmt &tScope);
  const clang::Expr *GetBinaryExprParenOrSelf(const clang::Expr *base);

  bool StripAddressOfVar(const clang::Expr *var, std::string &ret);
  std::string ExprToStr(const clang::Expr *expr);
  std::string DeclToStr(const clang::Decl *decl);
  clang::SourceLocation LexForTokenLocation(clang::SourceLocation beginLoc,
                                            clang::tok::TokenKind tokType);

  void PrettyError(clang::SourceRange loc,
                   llvm::raw_ostream::Colors underlineCol,
                   std::string extraInfo = "");

  // kernel name -> kernel def
  std::map<std::string, const KernelDefinition> &m_kernelFuncMap;
  std::vector<const clang::Stmt *> m_dPropScopes;

  std::set<unsigned> m_varTypeRenameLocs;
  ct::Replacements &m_replacements;
  const clang::SourceManager *SM;
  clang::ASTContext *AST;
  MatchFinderPtr m_finder;
  KernelLaunchTracker m_kernelTracker;
  KernelIncludeTracker &m_kernelIncludeTracker;
};