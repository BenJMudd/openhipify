#include "KernelTracking.h"
#include "OpenHipifyHostFA.h"
#include "OpenHipifyHostFAFactory.h"
#include "OpenHipifyKernelFA.h"
#include "OpenHipifyKernelFAFactory.h"
#include "args/Arguments.h"
#include "utils/Defs.h"
#include "utils/PathUtils.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"

#include <fstream>
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
    } else if (fileType == ".c") {
      hostSrcList.push_back(file);
    } else {
      llvm::errs() << errMsg(file);
      return false;
    }
  }

  return true;
} // namespace
  // llvm::sys::fs'boolSortSourceFilePaths(conststd::vector<std::string>&srcList,std::vector<std::string>&kernelSrcList,std::vector<std::string>&hostSrcList)

void ProcessFile(const std::string &file, ct::CommonOptionsParser &optParser,
                 std::map<std::string, const KernelDefinition> &kFuncMap,
                 OpenHipifyHostFA::KernelIncludeTracker *kTracker,
                 bool isKernel) {
  std::error_code err;

  // generate a temporary file to work on in case of runtime
  // errors, as we do not want to corrupt the input file
  SmallString<256> tmpFile;
  if (!Path::GenerateTempDuplicateFile(file, isKernel ? "cl" : "c", tmpFile)) {
    return;
  }
  std::string tmpFileStr = std::string(tmpFile.c_str());

  // Do refactoring
  ct::RefactoringTool refactoringTool(optParser.getCompilations(), tmpFileStr);
  ct::Replacements &replacements =
      refactoringTool.getReplacements()[tmpFileStr];

  [[maybe_unused]] int ret;
  if (isKernel) {
    OpenHipifyKernelFAFactory FAFactory(replacements, kFuncMap);
    ret = refactoringTool.runAndSave(&FAFactory);
  } else {
    OpenHipifyHostFAFactory FAFactory(replacements, kFuncMap, *kTracker);
    ret = refactoringTool.runAndSave(&FAFactory);
  }

  // copy the temporary file including rewrites to designated
  // target file
  std::string destFile = file + ".cpp";
  err = fs::copy_file(tmpFile, destFile);
  if (err) {
    llvm::errs() << sOpenHipify << sErr << err.message() << ": while copying "
                 << tmpFile << " to " << destFile << "\n";
    return;
  }
  fs::remove(tmpFile);
}

void GenerateHeaderFiles(OpenHipifyHostFA::KernelIncludeTracker &kTracker) {
  for (auto &[kernelFileName, kernelDefs] : kTracker) {
    std::ofstream headerFile(kernelFileName + ".h");
    headerFile << "#pragma once\n\n";
    for (auto &[kName, kDef] : kernelDefs) {
      headerFile << kDef << "\n";
    }
    headerFile.close();
  }
}

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

  std::map<std::string, const KernelDefinition> kernelFuncMap;
  for (const auto &kernelFile : kernelFiles) {
    ProcessFile(kernelFile, optParser, kernelFuncMap, nullptr, true);
  }

  OpenHipifyHostFA::KernelIncludeTracker kTracker;
  for (const auto &hostFile : hostFiles) {
    ProcessFile(hostFile, optParser, kernelFuncMap, &kTracker, false);
  }

  GenerateHeaderFiles(kTracker);

  return 0;
} // namespace clang::toolingintmain(intargc,charconst**argv)
