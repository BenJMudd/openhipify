#pragma once

#include "clang/ASTMatchers/ASTMatchFinder.h"
#include "clang/Frontend/FrontendAction.h"
#include "clang/Tooling/Core/Replacement.h"
struct KernelDefinition;

using namespace llvm;
namespace ct = clang::tooling;
namespace ASTMatch = clang::ast_matchers;

class OpenHipifyFA : public clang::ASTFrontendAction,
                     public ASTMatch::MatchFinder::MatchCallback {
  using MatchFinderPtr = std::unique_ptr<ASTMatch::MatchFinder>;

public:
  explicit OpenHipifyFA(ct::Replacements &replacements,
                        std::map<std::string, const KernelDefinition> &kFuncMap)
      : clang::ASTFrontendAction(), m_kernelFuncMap(kFuncMap),
        m_replacements(replacements) {}

protected:
  virtual void run(const ASTMatch::MatchFinder::MatchResult &res) override = 0;

  virtual std::unique_ptr<clang::ASTConsumer>
  CreateASTConsumer(clang::CompilerInstance &CI, StringRef InFile) override = 0;

  virtual void EndSourceFileAction() override = 0;

  void PrettyError(clang::SourceRange loc,
                   llvm::raw_ostream::Colors underlineCol,
                   std::string extraInfo = "");

  // kernel name -> kernel def
  std::map<std::string, const KernelDefinition> &m_kernelFuncMap;

  MatchFinderPtr m_finder;
  ct::Replacements &m_replacements;

  const clang::SourceManager *SM;
};