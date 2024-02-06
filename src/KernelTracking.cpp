#include "KernelTracking.h"

void KernelTracker::InsertArg(const clang::ValueDecl *kernelDecl,
                              const clang::CallExpr *argExpr) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.args.emplace_back(argExpr->getBeginLoc(), argExpr);
}

void KernelTracker::InsertLaunch(const clang::ValueDecl *kernelDecl,
                                 const clang::CallExpr *launchExpr) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.launches.emplace_back(launchExpr->getBeginLoc(), launchExpr);
}

void KernelTracker::InsertName(const clang::ValueDecl *kernelDecl,
                               std::string kernelName) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.funcName = kernelName;
}
