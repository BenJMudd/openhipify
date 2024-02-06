#include "KernelTracking.h"

using namespace clang;

void KernelTracker::InsertArg(const ValueDecl *kernelDecl,
                              const CallExpr *argExpr) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.args.emplace_back(argExpr);
}

void KernelTracker::InsertLaunch(const ValueDecl *kernelDecl,
                                 const CallExpr *launchExpr) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.launches.emplace_back(launchExpr);
}

void KernelTracker::InsertName(const ValueDecl *kernelDecl,
                               std::string kernelName) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.funcName = kernelName;
}

void KernelTracker::Finalise(const SourceManager &SM) {
  // Need to sort arg and launch lists in SourceLocationOrder
  // TODO: this is obviously an awful way to do this, maybe think of something
  // with a braincell
  auto SortCallExpr = [&](const CallExpr *lhs, const CallExpr *rhs) {
    unsigned lhsOffset = SM.getFileOffset(lhs->getBeginLoc());
    unsigned rhsOffset = SM.getFileOffset(rhs->getBeginLoc());
    return lhsOffset < rhsOffset;
  };

  for (auto &[kernelDecl, kInfo] : m_tracker) {
    std::sort(kInfo.args.begin(), kInfo.args.end(), SortCallExpr);
    std::sort(kInfo.launches.begin(), kInfo.launches.end(), SortCallExpr);
  }
}
