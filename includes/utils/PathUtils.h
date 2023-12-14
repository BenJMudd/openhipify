#pragma once
#include "llvm/ADT/SmallVector.h"
#include <string.h>
namespace OpenHipify::Path {
bool GenerateTempDuplicateFile(const std::string &file, const std::string &ext,
                               llvm::SmallVectorImpl<char> &tmpFile);

}