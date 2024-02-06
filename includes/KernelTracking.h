#include "clang/AST/Expr.h"
#include "clang/Basic/SourceLocation.h"
#include <string>

class KernelTracker {
public:
  KernelTracker() {}

  void InsertArg(const clang::ValueDecl *kernelDecl,
                 const clang::CallExpr *callExpr);
  void InsertLaunch(const clang::ValueDecl *kernelDecl,
                    const clang::CallExpr *callExpr);
  void InsertName(const clang::ValueDecl *kernelDecl, std::string kernelName);

  void Finalise();

private:
  struct KernelInfo {
    using LocationInfoVec =
        std::vector<std::pair<clang::SourceLocation, const clang::CallExpr *>>;
    std::string funcName;
    LocationInfoVec args;
    LocationInfoVec launches;
  };

  std::map<const clang::ValueDecl *, KernelInfo> m_tracker;
};