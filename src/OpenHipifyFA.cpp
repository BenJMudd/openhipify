#include "OpenHipifyFA.h"

#include "clang/AST/PrettyPrinter.h"
#include "clang/Basic/LangOptions.h"

#include "llvm/Support/WithColor.h"

using namespace ASTMatch;
using namespace clang;
void OpenHipifyFA::PrettyError(clang::SourceRange loc,
                               llvm::raw_ostream::Colors underlineCol,
                               std::string extraInfo) {

  StringRef fileName = getCurrentFile();
  fileName.consume_front("/tmp/");
  size_t index = fileName.rfind('-');
  fileName = fileName.take_front(index);

  llvm::errs() << "<" << fileName << ">"
               << "\n";

  unsigned startLineNum = SM->getSpellingLineNumber(loc.getBegin());
  // Get previous line
  auto LineSourceLoc = [&](unsigned line) -> SourceLocation {
    return SM->translateLineCol(SM->getMainFileID(), line, 1);
  };
  SourceLocation prevLineStart = LineSourceLoc(startLineNum - 1);
  SourceLocation curLineStart = LineSourceLoc(startLineNum);
  SourceLocation nextLineStart = LineSourceLoc(startLineNum + 1);
  SourceLocation nextLineEnd = LineSourceLoc(startLineNum + 2);

  auto LineText = [&](SourceLocation start, SourceLocation end) {
    CharSourceRange rng = CharSourceRange::getTokenRange(start, end);
    return std::string(Lexer::getSourceText(rng, *SM, LangOptions(), nullptr));
  };

  std::string prevLineStr = LineText(prevLineStart, curLineStart);
  std::string curLineStr = LineText(curLineStart, nextLineStart);
  std::string nextLineStr = LineText(nextLineStart, nextLineEnd);

  auto printLine = [&](std::string str, unsigned line) {
    llvm::errs() << line << "| " << str;
  };

  printLine(prevLineStr, startLineNum - 1);
  printLine(curLineStr, startLineNum);

  unsigned exprStartCol = SM->getSpellingColumnNumber(loc.getBegin());
  unsigned exprEndCol = SM->getSpellingColumnNumber(loc.getEnd());
  std::string curColNumString = std::to_string(startLineNum);
  for (unsigned i = 0; i < curColNumString.size(); i++) {
    llvm::errs() << " ";
  }
  llvm::errs() << "| ";
  for (unsigned i = 1; i < exprStartCol; i++) {
    llvm::errs() << " ";
  }
  for (unsigned i = exprStartCol; i < exprEndCol; i++) {
    llvm::WithColor(llvm::errs(), underlineCol, true) << "^";
  }

  if (extraInfo.size() > 0) {
    llvm::WithColor(llvm::errs(), underlineCol, true) << " <--- " << extraInfo;
  }

  llvm::errs() << "\n";
  printLine(nextLineStr, startLineNum + 1);
  llvm::errs() << "\n";
}
