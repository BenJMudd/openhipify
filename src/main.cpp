#include "args/Arguments.h"
#include "utils/Defs.h"
#include "clang/Tooling/CommonOptionsParser.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"
#include <sstream>
using namespace llvm;
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

  std::vector<std::string> kernelFiles;
  std::vector<std::string> hostFiles;
  if (!SortSourceFilePaths(srcFiles, kernelFiles, hostFiles))
    return 1;
  std::error_code err;
  for (const auto &kernelFile : kernelFiles) {
    SmallString<256> fileAbsPath;
    err = fs::real_path(kernelFile, fileAbsPath, true);
    if (err) {
      llvm::errs() << sOpenHipify << sErr << err.message()
                   << ": source file: " << kernelFile << "\n";
    }

    SmallString<256> tmpFile;
    StringRef kernelFileName = sys::path::filename(fileAbsPath);
    err = fs::createTemporaryFile(kernelFileName, "hip", tmpFile);
    if (err) {
      llvm::errs() << sOpenHipify << sErr << err.message()
                   << ": source file: " << kernelFile << "\n";
    }
    err = fs::copy_file(kernelFile, tmpFile);
    if (err) {
      llvm::errs() << sOpenHipify << sErr << err.message()
                   << ": while copying: " << fileAbsPath << " to " << tmpFile
                   << "\n";
    }
  }

  return 0;
} // namespace clang::toolingintmain(intargc,charconst**argv)
