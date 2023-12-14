#include "utils/PathUtils.h"
#include "utils/Defs.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Path.h"
using namespace llvm;
namespace fs = sys::fs;

namespace OpenHipify::Path {
bool GenerateTempDuplicateFile(const std::string &file, const std::string &ext,
                               SmallVectorImpl<char> &tmpFile) {
  SmallString<256> fileAbsPath;
  std::error_code err = fs::real_path(file, fileAbsPath, true);
  if (err) {
    llvm::errs() << sOpenHipify << sErr << err.message()
                 << ": source file: " << file << "\n";
    return false;
  }

  StringRef kernelFileName = sys::path::filename(fileAbsPath);
  err = fs::createTemporaryFile(kernelFileName, ext, tmpFile);
  if (err) {
    llvm::errs() << sOpenHipify << sErr << err.message()
                 << ": source file: " << file << "\n";
    return false;
  }
  err = fs::copy_file(file, tmpFile);
  if (err) {
    llvm::errs() << sOpenHipify << sErr << err.message()
                 << ": while copying: " << fileAbsPath << " to " << tmpFile
                 << "\n";
    return false;
  }

  return true;
}

} // namespace OpenHipify::Path