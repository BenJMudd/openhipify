#pragma once

#include "defs/OpenCLDefs.h"
#include "clang/AST/Expr.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include <string>

struct KernelDefinition {
  KernelDefinition() {}
  KernelDefinition(std::string funcDef, std::string fName, unsigned line,
                   unsigned col, std::vector<std::string> args)
      : functionDef(funcDef), fileName(fName), defLine(line), defCol(col),
        argTypes(args) {}
  std::string functionDef;
  std::string fileName;
  unsigned defLine;
  unsigned defCol;
  std::vector<std::string> argTypes;
};

class KernelLaunchTracker {
public:
  struct KernelInfo {
    std::string funcName;
    std::vector<const clang::CallExpr *> args;
    std::vector<std::pair<const clang::CallExpr *, OpenCL::HostFuncs>> launches;
  };
  KernelLaunchTracker() {}

  void InsertArg(const clang::ValueDecl *kernelDecl,
                 const clang::CallExpr *callExpr);
  void InsertLaunch(const clang::ValueDecl *kernelDecl,
                    const clang::CallExpr *callExpr,
                    OpenCL::HostFuncs funcType);
  void InsertName(const clang::ValueDecl *kernelDecl, std::string kernelName);

  void Finalise(const clang::SourceManager &SM);

  const std::map<const clang::ValueDecl *, KernelInfo> &GetKernelInfo() {
    return m_tracker;
  }

private:
  using KLaunch = std::pair<const clang::CallExpr *, OpenCL::HostFuncs>;
  std::map<const clang::ValueDecl *, KernelInfo> m_tracker;
};