#include "KernelTracking.h"

using namespace clang;

void KernelLaunchTracker::InsertArg(const ValueDecl *kernelDecl,
                                    const CallExpr *argExpr) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.args.emplace_back(argExpr);
}

void KernelLaunchTracker::InsertLaunch(const ValueDecl *kernelDecl,
                                       const CallExpr *launchExpr,
                                       OpenCL::HostFuncs funcType) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.launches.emplace_back(launchExpr, funcType);
}

void KernelLaunchTracker::InsertName(const ValueDecl *kernelDecl,
                                     std::string kernelName) {
  KernelInfo &kernel = m_tracker[kernelDecl];
  kernel.funcName = kernelName;
}

void KernelLaunchTracker::Finalise(const SourceManager &SM) {
  // Need to sort arg and launch lists in SourceLocationOrder
  // TODO: this is obviously an awful way to do this, maybe think of something
  // with a braincell
  auto SortCallExpr = [&](const CallExpr *lhs, const CallExpr *rhs) {
    unsigned lhsOffset = SM.getFileOffset(lhs->getBeginLoc());
    unsigned rhsOffset = SM.getFileOffset(rhs->getBeginLoc());
    return lhsOffset < rhsOffset;
  };

  auto SortKernelLaunch = [&](KLaunch lhs, KLaunch rhs) {
    unsigned lhsOffset = SM.getFileOffset(lhs.first->getBeginLoc());
    unsigned rhsOffset = SM.getFileOffset(rhs.first->getBeginLoc());
    return lhsOffset < rhsOffset;
  };

  for (auto &[kernelDecl, kInfo] : m_tracker) {
    std::sort(kInfo.args.begin(), kInfo.args.end(), SortCallExpr);
    std::sort(kInfo.launches.begin(), kInfo.launches.end(), SortKernelLaunch);
  }
}
