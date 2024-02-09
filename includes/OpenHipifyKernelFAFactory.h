#pragma once

#include "OpenHipifyKernelFA.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Tooling.h"

namespace ct = clang::tooling;

class OpenHipifyKernelFAFactory : public ct::FrontendActionFactory {
public:
  explicit OpenHipifyKernelFAFactory(
      ct::Replacements &replacements,
      std::map<std::string, const KernelDefinition> &kFuncMap)
      : ct::FrontendActionFactory(), m_replacements(replacements),
        m_kernelFuncMap(kFuncMap) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<OpenHipifyKernelFA>(m_replacements,
                                                m_kernelFuncMap);
  }

private:
  ct::Replacements &m_replacements;
  std::map<std::string, const KernelDefinition> &m_kernelFuncMap;
};
