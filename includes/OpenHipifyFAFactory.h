#pragma once

#include "clang/Tooling/Tooling.h"

namespace ct = clang::tooling;

// T -> type of Frontend Action
template <typename T>
class OpenHipifyFAFactory : public ct::FrontendActionFactory {
public:
  explicit OpenHipifyFAFactory(ct::Replacements &replacements)
      : ct::FrontendActionFactory(), m_replacements(replacements) {}

  std::unique_ptr<clang::FrontendAction> create() override {
    return std::make_unique<T>(m_replacements);
  }

private:
  ct::Replacements &m_replacements;
};
