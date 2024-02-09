#pragma once

#include "OpenHipifyKernelFA.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Tooling.h"

namespace ct = clang::tooling;

class OpenHipifyHostFAFactory : public ct::FrontendActionFactory {
public:
  explicit OpenHipifyHostFAFactory(
      ct::Replacements &replacements,
      std::map<std::string, const KernelDefinition> &kFuncMap,
      OpenHipifyHostFA::KernelIncludeTracker &kernelIncludeTracker)
      : ct::FrontendActionFactory(), m_replacements(replacements),
        m_kernelFuncMap(kFuncMap),
        m_kernelIncludeTracker(kernelIncludeTracker) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<OpenHipifyHostFA>(m_replacements, m_kernelFuncMap,
                                              m_kernelIncludeTracker);
  }

private:
  ct::Replacements &m_replacements;
  std::map<std::string, const KernelDefinition> &m_kernelFuncMap;
  OpenHipifyHostFA::KernelIncludeTracker &m_kernelIncludeTracker;
};
