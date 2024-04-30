#pragma once

#include "clang/Tooling/CommonOptionsParser.h"

namespace cl = llvm::cl;
extern cl::OptionCategory OpenHipifyToolTemplateCategory;
extern cl::opt<bool> NoKernelArgProtection;