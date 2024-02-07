#pragma once

#include "OpenHipifyKernelFA.h"
#include "clang/Tooling/Core/Replacement.h"
#include "clang/Tooling/Tooling.h"

namespace ct = clang::tooling;

// T -> type of Frontend Action
template <typename T>
class OpenHipifyFAFactory : public ct::FrontendActionFactory {
public:
  explicit OpenHipifyFAFactory(
      ct::Replacements &replacements,
      std::map<std::string, KernelDefinition> &kFuncMap)
      : ct::FrontendActionFactory(), m_replacements(replacements),
        m_kernelFuncMap(kFuncMap) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<T>(m_replacements, m_kernelFuncMap);
  }

private:
  ct::Replacements &m_replacements;
  std::map<std::string, KernelDefinition> &m_kernelFuncMap;
};
