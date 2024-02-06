#include "clang/AST/Expr.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/SourceManager.h"
#include <string>

class KernelTracker {
public:
  struct KernelInfo {
    std::string funcName;
    std::vector<const clang::CallExpr *> args;
    std::vector<const clang::CallExpr *> launches;
  };

  KernelTracker() {}

  void InsertArg(const clang::ValueDecl *kernelDecl,
                 const clang::CallExpr *callExpr);
  void InsertLaunch(const clang::ValueDecl *kernelDecl,
                    const clang::CallExpr *callExpr);
  void InsertName(const clang::ValueDecl *kernelDecl, std::string kernelName);

  void Finalise(const clang::SourceManager &SM);

  const std::map<const clang::ValueDecl *, KernelInfo> &GetKernelInfo() {
    return m_tracker;
  }

private:
  std::map<const clang::ValueDecl *, KernelInfo> m_tracker;
};