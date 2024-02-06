#include "KernelTracking.h"

void KernelTracker::InsertArg(const clang::ValueDecl *kernelDecl,
                              const clang::CallExpr *callExpr) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.args.emplace_back(callExpr->getBeginLoc(), callExpr);
}
