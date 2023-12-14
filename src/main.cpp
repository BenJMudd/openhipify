#include "args/Arguments.h"
#include "utils/Defs.h"
#include "utils/PathUtils.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include <sstream>

using namespace llvm;
using namespace OpenHipify;
namespace ct = clang::tooling;
namespace fs = sys::fs;

bool SortSourceFilePaths(const std::vector<std::string> &srcList,
                         std::vector<std::string> &kernelSrcList,
                         std::vector<std::string> &hostSrcList) {
  auto errMsg = [&](const std::string &file) -> std::string {
    std::stringstream error;
    error << sOpenHipify << sErr << "input file \"" << file
          << "\" is a non-compatable file type."
          << "\n";
    return error.str();
  };
  for (const std::string &file : srcList) {
    size_t dotIdx = file.rfind('.');
    if (dotIdx == std::string::npos) {
      llvm::errs() << errMsg(file);
      return false;
    }
    std::string fileType = file.substr(dotIdx);
    if (fileType == ".cl") {
      kernelSrcList.push_back(file);
    } else if (fileType == ".cpp") {
      hostSrcList.push_back(file);
    } else {
      llvm::errs() << errMsg(file);
      return false;
    }
  }

  return true;
} // namespace
  // llvm::sys::fs'boolSortSourceFilePaths(conststd::vector<std::string>&srcList,std::vector<std::string>&kernelSrcList,std::vector<std::string>&hostSrcList)

int main(int argc, const char **argv) {
  auto cop = ct::CommonOptionsParser::create(
      argc, argv, OpenHipifyToolTemplateCategory, llvm::cl::ZeroOrMore);
  if (!cop) {
    llvm::errs() << "\n" << sOpenHipify << sErr << cop.takeError() << "\n";
    return 1;
  }

  ct::CommonOptionsParser &optParser = cop.get();
  std::vector<std::string> srcFiles = optParser.getSourcePathList();
  if (srcFiles.empty()) {
    llvm::errs() << sOpenHipify << sErr << "Must specify at least 1 source file"
                 << "\n";
    return 1;
  }

  // separate kernel and host files. kernel files need to be parsed
  // first for host file kernel launch rewrites
  std::vector<std::string> kernelFiles;
  std::vector<std::string> hostFiles;
  if (!SortSourceFilePaths(srcFiles, kernelFiles, hostFiles))
    return 1;

  std::error_code err;
  for (const auto &kernelFile : kernelFiles) {
    // generate a temporary file to work on in case of runtime
    // errors, as we do not want to corrupt the input file
    SmallString<256> tmpFile;
    if (!Path::GenerateTempDuplicateFile(kernelFile, tmpFile)) {
      continue;
    }

    // Do refactoring

    // copy the temporary file including rewrites to designated
    // target file
    std::string destFile = kernelFile + ".hip";
    err = fs::copy_file(tmpFile, destFile);
    if (err) {
      llvm::errs() << sOpenHipify << sErr << err.message() << ": while copying "
                   << tmpFile << " to " << destFile << "\n";
      continue;
    }
    fs::remove(tmpFile);
  }

  return 0;
} // namespace clang::toolingintmain(intargc,charconst**argv)
