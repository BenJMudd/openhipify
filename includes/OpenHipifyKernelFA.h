#pragma once
#include "HIPDefs.h"
#include "KernelTracking.h"
#include "OpenClDefs.h"
#include "OpenHipifyFA.h"
#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Core/Replacement.h"

using namespace llvm;
namespace ct = clang::tooling;
namespace ASTMatch = clang::ast_matchers;

class OpenHipifyKernelFA : public OpenHipifyFA {

public:
  explicit OpenHipifyKernelFA(
      ct::Replacements &replacements,
      std::map<std::string, const KernelDefinition> &kFuncMap)
      : OpenHipifyFA(replacements, kFuncMap) {}

private:
  void run(const ASTMatch::MatchFinder::MatchResult &res) override;

  std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override;
  void EndSourceFileAction() override;

  bool OpenCLFunctionCall(const ASTMatch::MatchFinder::MatchResult &res);
  bool OpenCLKernelFunctionDecl(const ASTMatch::MatchFinder::MatchResult &res);
  bool OpenCLAsTypeExpr(const ASTMatch::MatchFinder::MatchResult &res);

  void AppendKernelFuncMap(const clang::FunctionDecl &funcDecl,
                           const std::vector<ct::Replacement> &replacements);

  void InsertAuxFunction(clang::CharSourceRange funcNameRng,
                         HIP::AUX_FUNC_ID func);

  bool ReplaceWithAuxFunction(const clang::FunctionDecl &funcDecl,
                              OpenCL::KernelFuncs funcIdent);

  bool
  ReplaceGET_GENERIC_THREAD_ID(const clang::CallExpr &callExpr,
                               const ASTMatch::MatchFinder::MatchResult &res,
                               OpenCL::KernelFuncs funcIdent);
  bool ReplaceBARRIER(const clang::CallExpr &callExpr,
                      const ASTMatch::MatchFinder::MatchResult &res);

  std::set<HIP::AUX_FUNC_ID> m_auxFunctions;
};