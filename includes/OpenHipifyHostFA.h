#pragma once

#include "KernelTracking.h"
#include "OpenClDefs.h"
#include "OpenHipifyFA.h"
#include "OpenHipifyKernelFA.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/CompilerInstance.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Core/Replacement.h"

using namespace llvm;
namespace ct = clang::tooling;
namespace ASTMatch = clang::ast_matchers;

class OpenHipifyHostFA : public OpenHipifyFA {
public:
  // file name -> (kernel name -> kernel definition)
  using KernelIncludeTracker =
      std::map<std::string, std::map<std::string, std::string>>;
  using KernelLaunch = std::pair<const clang::CallExpr *, OpenCL::HostFuncs>;

  explicit OpenHipifyHostFA(
      ct::Replacements &replacements,
      std::map<std::string, const KernelDefinition> &kFuncMap,
      KernelIncludeTracker &kernelIncludeTracker)
      : OpenHipifyFA(replacements, kFuncMap),
        m_kernelIncludeTracker(kernelIncludeTracker) {}

private:
  struct ArgInfo {
    ArgInfo(std::string str, bool stripped, bool cast)
        : argStr(str), isAddrOpStripped(stripped), toCast(cast) {}
    ArgInfo() : isAddrOpStripped(false), toCast(false) {}

    std::string argStr;
    bool isAddrOpStripped;
    bool toCast;
  };

  void run(const ASTMatch::MatchFinder::MatchResult &res) override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override;
  void EndSourceFileAction() override;

  bool FunctionCall(const ASTMatch::MatchFinder::MatchResult &res);
  bool VariableDeclaration(const ASTMatch::MatchFinder::MatchResult &res);
  bool DeclarationStmt(const ASTMatch::MatchFinder::MatchResult &res);
  bool BinaryOpDeclRef(const ASTMatch::MatchFinder::MatchResult &res);
  bool ErrorComparison(const ASTMatch::MatchFinder::MatchResult &res);

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
                                  const clang::VarDecl *varDecl, bool errVar,
                                  const std::string &errVarStr);
  bool ReplaceCreateBufferBinOp(const clang::CallExpr *cBufExpr,
                                const clang::BinaryOperator *binOp, bool errVar,
                                const std::string &errVarStr);
  void ReplaceCreateBufferArguments(const clang::CallExpr *callExpr,
                                    std::string varName);
  void ReplaceCBuffPiggyBack(const clang::CallExpr *callExpr,
                             std::string hostBuf, std::string bytes);
  bool ReplaceEnqueBuffer(const clang::CallExpr *callExpr, bool isRead);
  bool ReplaceReleaseMemObject(const clang::CallExpr *callExpr);

  // Kernel function call replacements
  bool TrackKernelSetArg(const clang::CallExpr *callExpr);
  bool TrackKernelLaunch(const clang::CallExpr *callExpr,
                         OpenCL::HostFuncs func);
  bool TrackKernelCreate(const clang::CallExpr *callExpr);
  bool TrackKernelCreateBinop(const clang::CallExpr *callExpr,
                              const clang::BinaryOperator *binOp,
                              std::string kernelName);

  void GenerateNDKernelLaunch(const clang::CallExpr *launchKernelExpr,
                              const KernelLaunchTracker::KernelInfo &kInfo,
                              const KernelDefinition *kDef,
                              const std::vector<ArgInfo> &args,
                              std::set<std::string> &kernelFilesUsed);
  void GenerateEnqueueTaskLaunch(const clang::CallExpr *launchKernelExpr,
                                 const KernelLaunchTracker::KernelInfo &kInfo,
                                 const KernelDefinition *kDef,
                                 const std::vector<ArgInfo> &args,
                                 std::set<std::string> &kernelFilesUsed);
  void GenerateGenericKernelLaunch(
      const clang::CallExpr *launchKernelExpr, const std::string &numBlocks,
      const std::string &blockSize, bool nBlockAddrStripped,
      bool sBlockAddrStripped, const KernelLaunchTracker::KernelInfo &kInfo,
      const KernelDefinition *kDef, const std::vector<ArgInfo> &args,
      std::set<std::string> &kernelFilesUsed);

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
  const clang::BinaryOperator *GetBinaryExprParent(const clang::Expr *base);
  const clang::Expr *GetBinaryExprParenOrSelf(const clang::Expr *base);
  void RemoveBinExprIfPossible(const clang::Expr *base);
  void EnsureBinExprIsAssign(const clang::Expr *base);

  bool StripAddressOfVar(const clang::Expr *var, std::string &ret);
  std::string ExprToStr(const clang::Expr *expr);
  bool ExtractType(const clang::Decl *decl, std::string &ret);
  std::string DeclToStr(const clang::Decl *decl);
  clang::SourceLocation LexForTokenLocation(clang::SourceLocation beginLoc,
                                            clang::tok::TokenKind tokType);

  std::vector<const clang::Stmt *> m_dPropScopes;
  std::set<clang::SourceLocation> m_clIntRenames;

  std::set<unsigned> m_varTypeRenameLocs;
  clang::ASTContext *AST;
  KernelLaunchTracker m_kernelTracker;
  KernelIncludeTracker &m_kernelIncludeTracker;
};