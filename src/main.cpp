
#include "Arguments.h"
#include "KernelTracking.h"
#include "OpenHipifyFAFactory.h"
#include "OpenHipifyHostFA.h"
#include "OpenHipifyKernelFA.h"
#include "defs/OpenhipifyDefs.h"

#include "clang/Tooling/CommonOptionsParser.h"
#include "clang/Tooling/Refactoring.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/FileSystem.h"

#include <filesystem>
#include <fstream>
#include <sstream>

using namespace llvm;
namespace ct = clang::tooling;
namespace fs = sys::fs;
namespace stdfs = std::filesystem;

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

  // We move kernel files (.cl) before host files (.c)
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
}

void ProcessFile(const std::string &file, ct::CommonOptionsParser &optParser,
                 std::map<std::string, const KernelDefinition> &kFuncMap,
                 OpenHipifyHostFA::KernelIncludeTracker *kTracker,
                 bool isKernel) {
  // generate a temporary file to work on in case of runtime
  // errors, as we do not want to corrupt the input file
  std::error_code err;
  SmallString<256> tmpFile;
  if (!GenerateTempDuplicateFile(file, isKernel ? "cl" : "c", tmpFile)) {
    return;
  }
  std::string tmpFileStr = std::string(tmpFile.c_str());

  // Do refactoring
  ct::RefactoringTool refactoringTool(optParser.getCompilations(), tmpFileStr);

  // arguments include
  std::string curDir(stdfs::current_path());
  std::string curDirIncludeArg = curDir;
  refactoringTool.appendArgumentsAdjuster(ct::getInsertArgumentAdjuster(
      curDirIncludeArg.c_str(), ct::ArgumentInsertPosition::BEGIN));
  refactoringTool.appendArgumentsAdjuster(
      ct::getInsertArgumentAdjuster("-I", ct::ArgumentInsertPosition::BEGIN));

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
    // Generation of header file
    std::ofstream headerFile(kernelFileName + ".hpp");
    headerFile << sOpenHipifyGenerated;
    headerFile << "#pragma once\n\n";
    for (auto &[kName, kDef] : kernelDefs) {
      headerFile << kDef << "\n";
    }
    headerFile << sOpenHipifyGeneratedEnd;
    headerFile.close();

    // Appending include of header to kernel definition
    std::string kernelTransFile(kernelFileName + ".cpp");
    std::fstream kernelDefFile(kernelTransFile);
    // take first two lines (gen by hip, and hip runtime)
    std::stringstream prependedKernelDefFile;
    for (int i = 0; i < 2; ++i) {
      if (kernelDefFile.good()) {
        std::string line;
        getline(kernelDefFile, line);
        prependedKernelDefFile << line << "\n";
      }
    }

    prependedKernelDefFile << "\n#include \"" << kernelFileName + ".hpp"
                           << "\"\n";
    prependedKernelDefFile << kernelDefFile.rdbuf();
    kernelDefFile.close();
    kernelDefFile.open(kernelTransFile,
                       std::fstream::out | std::fstream::trunc);
    kernelDefFile << prependedKernelDefFile.rdbuf() << "\n";
    kernelDefFile.close();
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
