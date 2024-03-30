#include "OpenHipifyKernelFA.h"
#include "utils/Defs.h"
#include "clang/AST/PrettyPrinter.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Frontend/CompilerInstance.h"
#include "llvm/Support/WithColor.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";
const StringRef B_KERNEL_DECL = "kernelFuncDecl";
const StringRef B_AS_TYPE_EXPR = "asTypeExpr";

std::unique_ptr<ASTConsumer>
OpenHipifyKernelFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);
  SM = &getCompilerInstance().getSourceManager();

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  m_finder->addMatcher(asTypeExpr(isExpansionInMainFile()).bind(B_AS_TYPE_EXPR),
                       this);
  m_finder->addMatcher(
      functionDecl(isExpansionInMainFile(), hasAttr(attr::OpenCLKernel))
          .bind(B_KERNEL_DECL),
      this);

  return m_finder->newASTConsumer();
}

void OpenHipifyKernelFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (OpenCLFunctionCall(res))
    return;

  if (OpenCLKernelFunctionDecl(res))
    return;

  if (OpenCLAsTypeExpr(res))
    return;
}

void OpenHipifyKernelFA::EndSourceFileAction() {
  std::string prepend;
  llvm::raw_string_ostream prependStr(prepend);
  prependStr << sOpenHipifyGenerated;
  // include hip runtime
  prependStr << "#include \"hip/hip_runtime.h\"\n";
  // Inserting auxiliary functions
  if (!m_auxFunctions.empty()) {
    prependStr << "\n";
    // Concatonate into singular string for a single insert
    for (HIP::AUX_FUNC_ID auxFuncId : m_auxFunctions) {
      auto funcToInsert = HIP::AUX_FUNC_MAP.find(auxFuncId);
      const std::string &funcBody = funcToInsert->second.second;
      prependStr << funcBody;
    }
  }
  prependStr << sOpenHipifyGeneratedEnd;

  SourceManager &srcManager = getCompilerInstance().getSourceManager();
  SourceLocation prependLoc =
      srcManager.getLocForStartOfFile(srcManager.getMainFileID());
  ct::Replacement prependRepl(srcManager, prependLoc, 0, prependStr.str());
  llvm::consumeError(m_replacements.add(prependRepl));
}

bool OpenHipifyKernelFA::OpenCLKernelFunctionDecl(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const FunctionDecl *funcDecl =
      res.Nodes.getNodeAs<FunctionDecl>(B_KERNEL_DECL);
  if (!funcDecl)
    return false;

  std::vector<ct::Replacement> functionReplacements;

  // Replace __kernel function attribute with HIP equivalent __global__
  auto *kAttr = funcDecl->getAttr<OpenCLKernelAttr>();
  CharSourceRange kAttrRng = CharSourceRange::getTokenRange(kAttr->getRange());
  ct::Replacement replacement(*res.SourceManager, kAttrRng,
                              HIP::GLOBAL_FUNC_ATTR);
  llvm::consumeError(m_replacements.add(replacement));

  for (ParmVarDecl *param : funcDecl->parameters()) {
    SourceRange paramRange = param->getSourceRange();
    // read parameter string from source text
    std::string typeStr(clang::Lexer::getSourceText(
        clang::CharSourceRange::getTokenRange(paramRange), *res.SourceManager,
        clang::LangOptions(), nullptr));
    for (const std::string &openCLAddrSpaceStr :
         OpenCL::AddrSpace::SPACES_SET) {
      size_t addrSpaceIdx = typeStr.find(openCLAddrSpaceStr);
      if (addrSpaceIdx == std::string::npos)
        continue;

      // OpenCL parameter found, strip memeory range
      SourceLocation typeBeginLoc = param->getBeginLoc();
      SourceLocation addrSpaceBeginLoc =
          typeBeginLoc.getLocWithOffset(addrSpaceIdx);
      SourceLocation addrSpaceEndLoc = typeBeginLoc.getLocWithOffset(
          addrSpaceIdx + openCLAddrSpaceStr.size() + 1);
      CharSourceRange addrSpaceRng =
          CharSourceRange::getCharRange(addrSpaceBeginLoc, addrSpaceEndLoc);

      ct::Replacement replacement(*res.SourceManager, addrSpaceRng, {});
      functionReplacements.push_back(replacement);

      llvm::consumeError(m_replacements.add(replacement));
    }
  }

  AppendKernelFuncMap(*funcDecl, functionReplacements);
  return true;
}

bool OpenHipifyKernelFA::OpenCLAsTypeExpr(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const AsTypeExpr *asTypeExpr =
      res.Nodes.getNodeAs<AsTypeExpr>(B_AS_TYPE_EXPR);
  if (!asTypeExpr)
    return false;

  // wrap whole expr in brackets
  // replace function call name
  ParenExpr *parenExpr = dyn_cast<ParenExpr>(asTypeExpr->getSrcExpr());
  if (!parenExpr) {
    llvm::errs() << "Something wrong!\n";
    return false;
  }
  // begin loc of func call is asTypeExpr->getBeginLoc() // location of start of
  // inner brackets parenExpr->getSubExpr()->getExprLoc()
  // <- spelling loc
  // location previously with 1 less is parenExpr->getSubExpr()->getBeginLoc()
  // <- spelling loc

  // location at end (minus 2 for some reason) is
  // parenExpr->getSubExpr->getEndLoc()
  const Expr *subExpr = parenExpr->getSubExpr();
  SourceLocation bSub = subExpr->getBeginLoc();
  SourceLocation eSub = subExpr->getEndLoc();

  FullSourceLoc fbSub(bSub, *SM);
  FullSourceLoc feSub(eSub, *SM);

  // End location points to the wrong location, we lex for it manually here
  size_t bracketIdx = 0;
  SourceLocation typeExprStart = SM->getExpansionLoc(asTypeExpr->getBeginLoc());
  SourceLocation typeExprEnd = SM->getExpansionLoc(asTypeExpr->getEndLoc());
  const char *startBuf = SM->getCharacterData(typeExprEnd);
  const char *fileEndBuf =
      SM->getCharacterData(SM->getLocForEndOfFile(SM->getMainFileID()));
  Lexer lex(typeExprEnd, clang::LangOptions(), startBuf, startBuf, fileEndBuf);

  clang::Token tok;
  SourceLocation rParenLoc;
  SourceLocation lParenLoc;
  lex.LexFromRawLexer(tok);
  // TODO: Better escape clause
  while (1) {
    if (tok.is(clang::tok::r_paren)) {
      bracketIdx--;
      if (bracketIdx == 0) {
        rParenLoc = tok.getLocation();
        break;
      }
    } else if (tok.is(clang::tok::l_paren)) {
      if (bracketIdx == 0) {
        lParenLoc = tok.getLocation();
      }
      bracketIdx++;
    }

    lex.LexFromRawLexer(tok);
  }
  // range of inside expression
  // PrettyError({fbSub.getSpellingLoc(), rParenLoc}, raw_ostream::RED);

  CharSourceRange callCharRng =
      CharSourceRange::getTokenRange(typeExprStart, lParenLoc);

  clang::SmallString<40> castStr;
  llvm::raw_svector_ostream castOS(castStr);
  castOS << "((";
  QualType castType = asTypeExpr->getType();
  std::string typeStr = castType.getAsString();
  if (typeStr == "float") {
    castOS << "float";
  } else if (typeStr == "double") {
    castOS << "double";
  } else {
    llvm::errs() << sOpenHipify << sErr << "Cannot deduce type.\n";
    return false; // TODO: Better err msg
  }
  castOS << ")(";

  ct::Replacement callRepl = ct::Replacement(*SM, callCharRng, castOS.str());
  llvm::consumeError(m_replacements.add(callRepl));
  ct::Replacement parenRepl = ct::Replacement(*SM, rParenLoc, 0, ")");
  llvm::consumeError(m_replacements.add(parenRepl));
  return true;
}

bool OpenHipifyKernelFA::OpenCLFunctionCall(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const CallExpr *callExpr = res.Nodes.getNodeAs<CallExpr>(B_CALL_EXPR);
  if (!callExpr)
    return false;

  const FunctionDecl *funcDecl = callExpr->getDirectCallee();
  if (!funcDecl)
    return false;

  const DeclarationNameInfo &nameInfo = funcDecl->getNameInfo();
  std::string funcName = nameInfo.getAsString();
  auto funcSearch = OpenCL::KERNEL_FUNC_MAP.find(funcName);
  if (funcSearch == OpenCL::KERNEL_FUNC_MAP.end())
    return false;

  // Found OpenCL function call
  switch (funcSearch->second) {
  case OpenCL::KernelFuncs::GET_GLOBAL_ID:
  case OpenCL::KernelFuncs::GET_LOCAL_ID:
  case OpenCL::KernelFuncs::GET_GROUP_ID:
  case OpenCL::KernelFuncs::GET_LOCAL_SIZE: {
    ReplaceGET_GENERIC_THREAD_ID(*callExpr, res, funcSearch->second);
  } break;
  case OpenCL::KernelFuncs::BARRIER: {
    ReplaceBARRIER(*callExpr, res);
  } break;
  }

  return true;
}

void OpenHipifyKernelFA::AppendKernelFuncMap(
    const FunctionDecl &funcDecl,
    const std::vector<ct::Replacement> &replacements) {
  StringRef fileName = getCurrentFile();
  fileName.consume_front("/tmp/");
  size_t index = fileName.rfind('-');
  fileName = fileName.take_front(index);

  const SourceManager &srcManager = getCompilerInstance().getSourceManager();
  SourceRange funcDeclRange(funcDecl.getTypeSpecStartLoc(),
                            funcDecl.getTypeSpecEndLoc());

  // reading the function declaration
  LangOptions LO;
  CharSourceRange charSrcRng = CharSourceRange::getCharRange(funcDeclRange);
  std::string funcDeclStrRaw(Lexer::getSourceText(charSrcRng, srcManager, LO));

  // Applying replacements
  unsigned baseOffset =
      srcManager.getFileOffset(funcDecl.getTypeSpecStartLoc());
  size_t offset = 0;
  std::string funcDeclStr = "__global__ ";
  for (auto &replacement : replacements) {
    funcDeclStr += funcDeclStrRaw.substr(offset, replacement.getOffset() -
                                                     offset - baseOffset);
    funcDeclStr += replacement.getReplacementText();
    offset = replacement.getOffset() - baseOffset + replacement.getLength();
  }
  funcDeclStr += funcDeclStrRaw.substr(offset);
  funcDeclStr += ");";

  std::vector<std::string> funcArgs;
  for (ParmVarDecl *param : funcDecl.parameters()) {
    std::string paramStr = param->getOriginalType().getAsString();
    for (const std::string &openCLAddrSpaceStr :
         OpenCL::AddrSpace::SPACES_SET) {
      size_t addrSpaceIdx = paramStr.find(openCLAddrSpaceStr);
      if (addrSpaceIdx != std::string::npos) {
        paramStr.erase(addrSpaceIdx, openCLAddrSpaceStr.length());
        break;
      }
    }
    funcArgs.push_back(paramStr);
  }

  // TODO: Handle error where there are 2 kernels of the same name
  unsigned int lineNumber =
      srcManager.getSpellingLineNumber(funcDecl.getTypeSpecStartLoc());
  unsigned int colNumber =
      srcManager.getSpellingColumnNumber(funcDecl.getTypeSpecStartLoc());
  m_kernelFuncMap.insert(
      {funcDecl.getName().str(),
       {funcDeclStr, fileName.str(), lineNumber, colNumber, funcArgs}});
}

void OpenHipifyKernelFA::InsertAuxFunction(const SourceManager &srcManager,
                                           CharSourceRange funcNameRng,
                                           HIP::AUX_FUNC_ID func) {

  auto funcToInsert = HIP::AUX_FUNC_MAP.find(func);
  if (funcToInsert == HIP::AUX_FUNC_MAP.end())
    return;

  const std::string &funcName = funcToInsert->second.first;

  // Rename the original function call to the auxiliary function
  ct::Replacement replacementName(srcManager, funcNameRng, funcName);
  llvm::consumeError(m_replacements.add(replacementName));
  m_auxFunctions.insert(func);
  return;
}

bool OpenHipifyKernelFA::ReplaceBARRIER(
    const clang::CallExpr &callExpr,
    const ASTMatch::MatchFinder::MatchResult &res) {
  const Expr *fenceTypeFlag = callExpr.getArg(0);
  if (!fenceTypeFlag)
    return false;

  Expr::EvalResult fenceTypeEval;
  const clang::ASTContext *ctx = res.Context;
  // TODO: add aux function for false
  if (!fenceTypeFlag->EvaluateAsInt(fenceTypeEval, *ctx))
    return false;

  APSInt fenceType = fenceTypeEval.Val.getInt();
  const SourceManager *SM = res.SourceManager;
  SourceLocation startLoc = callExpr.getBeginLoc();
  SourceLocation endLoc = callExpr.getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  bool insertNewExpr = false;
  if ((fenceType & OpenCL::CLK_LOCAL_MEM_FENCE) != 0) {
    ct::Replacement replacement(*SM, exprCharRange, HIP::THREAD_FENCE_BLOCK);
    llvm::consumeError(m_replacements.add(replacement));
    insertNewExpr = true;
  }
  if ((fenceType & OpenCL::CLK_GLOBAL_MEM_FENCE) != 0) {
    ct::Replacement replacement;
    // If source contains both a local and a global fence, we must insert
    // another statement for this. In OpenCL, this is done via bit
    // flags, whereas in HIP this is done via separate function calls
    if (insertNewExpr) {
      // Lex to the end of the line
      const char *barrierBuf = SM->getCharacterData(callExpr.getEndLoc());
      const char *endBuf =
          SM->getCharacterData(SM->getLocForEndOfFile(SM->getMainFileID()));
      Lexer lex(callExpr.getEndLoc(), clang::LangOptions(), barrierBuf,
                barrierBuf, endBuf);

      clang::Token tok;
      lex.LexFromRawLexer(tok);
      while (tok.isNot(clang::tok::semi)) {
        lex.LexFromRawLexer(tok);
      }

      // End of the line found, insert a new instruciton
      std::string hipRepl = HIP::THREAD_FENCE;
      hipRepl += ';';
      replacement = ct::Replacement(*SM, tok.getEndLoc(), 0, hipRepl);
    } else {
      replacement = ct::Replacement(*SM, exprCharRange, HIP::THREAD_FENCE);
    }
    llvm::consumeError(m_replacements.add(replacement));
  }
  return true;
}

bool OpenHipifyKernelFA::ReplaceGET_GENERIC_THREAD_ID(
    const clang::CallExpr &callExpr,
    const ASTMatch::MatchFinder::MatchResult &res,
    OpenCL::KernelFuncs funcIdent) {
  const Expr *dimensionArg = callExpr.getArg(0);
  if (!dimensionArg)
    return false;

  // Attempt to evaluate the dimension parameter. If possible we inplace
  // generate correspondent HIP code. If not we must insert a utility
  // function. For most OpenCL code this will be easily evaluated
  Expr::EvalResult dimensionFold;
  const clang::ASTContext *ctx = res.Context;
  if (!dimensionArg->EvaluateAsInt(dimensionFold, *ctx)) {
    // Can't fold, insert and call an auxiliary function
    const FunctionDecl &funcDecl = *callExpr.getDirectCallee();
    SourceRange srcRange = funcDecl.getNameInfo().getSourceRange();
    CharSourceRange nameRange = CharSourceRange::getTokenRange(srcRange);
    auto auxFunc = HIP::OPENCL_HIP_AUX_FUNC_MAP.find(funcIdent);
    InsertAuxFunction(*res.SourceManager, nameRange, auxFunc->second);
    return true;
  }

  APSInt dimension = dimensionFold.Val.getInt();
  char hipDimension;
  if (dimension == 0)
    hipDimension = 'x';
  else if (dimension == 1)
    hipDimension = 'y';
  else if (dimension == 2)
    hipDimension = 'z';
  else {
    llvm::errs() << sOpenHipify << sErr
                 << "Out of range dimension identifier: " << dimension
                 << " at location: "
                 << dimensionArg->getExprLoc().printToString(*res.SourceManager)
                 << "\n";
    return false;
  }

  // Generate HIP replacement:
  clang::SmallString<40> hipDimensionStr;
  llvm::raw_svector_ostream hipDimOS(hipDimensionStr);

  switch (funcIdent) {
  case OpenCL::KernelFuncs::GET_GLOBAL_ID: {
    // hipBlockDim_DIM * hipBlockIdx_DIM + hipThreadIdx_DIM

    // Lex next token to see if it's a semi colon. If not, we
    // guard the insertion as statements after can result in
    // change in execution. E.g. get_global_id(0) * 2 would
    // wrongly translate to hipBlockDim_x * hipBlockIdx_x + hipThreadIdx_x * 2
    // without guard, incorrectly.
    Token nextTok;
    LangOptions LO;
    SourceLocation nextTokLoc = Lexer::getLocForEndOfToken(
        callExpr.getEndLoc(), 0, *res.SourceManager, LO);
    Lexer::getRawToken(nextTokLoc, nextTok, *res.SourceManager, LO, true);

    bool guardHipInsertion = nextTok.getKind() != tok::semi;

    if (guardHipInsertion)
      hipDimOS << "(";

    hipDimOS << HIP::BLOCK_DIM_GENERIC << hipDimension << " * "
             << HIP::BLOCK_IDX_GENERIC << hipDimension << " + "
             << HIP::THREAD_IDX_GENERIC << hipDimension;

    if (guardHipInsertion)
      hipDimOS << ")";
  } break;
  case OpenCL::KernelFuncs::GET_LOCAL_ID: {
    // hipThreadIdx_DIM
    hipDimOS << HIP::THREAD_IDX_GENERIC << hipDimension;
  } break;
  case OpenCL::KernelFuncs::GET_GROUP_ID: {
    hipDimOS << HIP::BLOCK_IDX_GENERIC << hipDimension;
  } break;
  case OpenCL::KernelFuncs::GET_LOCAL_SIZE: {
    hipDimOS << HIP::BLOCK_DIM_GENERIC << hipDimension;
  } break;
  default:
    break;
  }

  const SourceManager *srcManager = res.SourceManager;
  SourceLocation startLoc = callExpr.getBeginLoc();
  SourceLocation endLoc = callExpr.getEndLoc();
  CharSourceRange exprCharRange =
      CharSourceRange::getTokenRange(startLoc, endLoc);

  ct::Replacement replacement(*srcManager, exprCharRange, hipDimOS.str());
  llvm::consumeError(m_replacements.add(replacement));
  return true;
}

void OpenHipifyKernelFA::PrettyError(clang::SourceRange loc,
                                     raw_ostream::Colors underlineCol,
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
