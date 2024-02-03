#include "OpenHipifyHostFA.h"
#include "OpenClDefs.h"
#include "utils/Defs.h"
#include "clang/Frontend/CompilerInstance.h"

using namespace ASTMatch;
using namespace clang;

const StringRef B_CALL_EXPR = "callExpr";

std::unique_ptr<ASTConsumer>
OpenHipifyHostFA::CreateASTConsumer(CompilerInstance &CI, StringRef InFile) {
  m_finder.reset(new ASTMatch::MatchFinder);

  // case matching
  m_finder->addMatcher(callExpr(isExpansionInMainFile()).bind(B_CALL_EXPR),
                       this);

  return m_finder->newASTConsumer();
}

void OpenHipifyHostFA::run(const ASTMatch::MatchFinder::MatchResult &res) {
  if (FunctionCall(res))
    return;
}

bool OpenHipifyHostFA::FunctionCall(
    const ASTMatch::MatchFinder::MatchResult &res) {
  const CallExpr *callExpr = res.Nodes.getNodeAs<CallExpr>(B_CALL_EXPR);
  if (!callExpr)
    return false;

  const FunctionDecl *funcDecl = callExpr->getDirectCallee();
  if (!funcDecl)
    return false;

  const DeclarationNameInfo &nameInfo = funcDecl->getNameInfo();
  std::string funcName = nameInfo.getAsString();
  auto funcSearch = OpenCL::HOST_FUNC_MAP.find(funcName);
  if (funcSearch == OpenCL::HOST_FUNC_MAP.end())
    return false;

  auto iter = OpenCL::HOST_MEM_FUNCS.find(funcSearch->second);
  if (iter != OpenCL::HOST_MEM_FUNCS.end()) {
    // Memory related function found
    HandleMemoryFunctionCall(callExpr, *iter);
  }

  return false;
}

bool OpenHipifyHostFA::HandleMemoryFunctionCall(const CallExpr *callExpr,
                                                OpenCL::HostFuncs func) {
  ASTContext &astCtx = getCompilerInstance().getASTContext();
  SourceManager &SM = getCompilerInstance().getSourceManager();
  switch (func) {
  case OpenCL::HostFuncs::clCreateBuffer: {
    auto callExprParIter = astCtx.getParents(*callExpr).begin();

    // Grab parent of callExpr
    // TODO: Support other cases than just vardecl, e.g. binary expression
    const VarDecl *varDecl;
    varDecl = callExprParIter->get<VarDecl>();
    if (!varDecl)
      return false;

    // Renaming the type from cl_mem -> void*
    SourceLocation typeBeginLoc = varDecl->getBeginLoc();
    std::string typeStr = varDecl->getTypeSourceInfo()->getType().getAsString();
    ct::Replacement typeReplacement(SM, typeBeginLoc, typeStr.length(),
                                    HIP::VOID_PTR);
    llvm::consumeError(m_replacements.add(typeReplacement));

    // end vardecl type with ; to split call into two statements:
    //
    // cl_mem mem = clCreateBuffer(...);
    //
    // void* mem;
    // hipMalloc((void**) &mem, ...);
    //
    // TODO: come up with a better way to do this

    // Finding SourceLocation for: cl_mem mem = clCreateBuffer(...);
    //                                        ^
    const char *varDeclStartBuf = SM.getCharacterData(varDecl->getBeginLoc());
    const char *fileEndBuf =
        SM.getCharacterData(SM.getLocForEndOfFile(SM.getMainFileID()));
    Lexer lex(typeBeginLoc, clang::LangOptions(), varDeclStartBuf,
              varDeclStartBuf, fileEndBuf);

    clang::Token tok;
    lex.LexFromRawLexer(tok);
    while (tok.isNot(clang::tok::equal)) {
      lex.LexFromRawLexer(tok);
    }

    // Range created from equal token and start of function call
    // cl_mem mem = clCreateBuffer(...);
    //            ^^^
    CharSourceRange binaryExprRng = CharSourceRange::getTokenRange(
        tok.getLocation(), callExpr->getBeginLoc());

    std::string splitExpr;
    llvm::raw_string_ostream splitExprStr(splitExpr);
    splitExprStr << HIP::EOL << HIP::HIP_MALLOC;

    // Final replacement for splitting we change the function call as well
    // cl_mem mem = clCreateBuffer(...);
    // cl_mem mem ; hipMalloc(...);
    ct::Replacement binaryExprRepl(SM, binaryExprRng, splitExprStr.str());
    llvm::consumeError(m_replacements.add(binaryExprRepl));

    // Argument replacement
    // size of buffer argument extracted
    const Expr *bufSize = callExpr->getArg(2);
    if (!bufSize) {
      return false;
    }

    CharSourceRange bufSizeSrcRng =
        CharSourceRange::getTokenRange(bufSize->getSourceRange());
    std::string bufSizeExprStr(
        Lexer::getSourceText(bufSizeSrcRng, SM, LangOptions(), nullptr));

    // Name of mem variable extracted
    std::string varName = varDecl->getNameAsString();

    // Whole argument replacement
    SourceLocation argStart = callExpr->getArg(0)->getExprLoc();
    SourceLocation argEnd = callExpr->getEndLoc();
    CharSourceRange argRng = CharSourceRange::getCharRange(argStart, argEnd);

    std::string newArgs;
    llvm::raw_string_ostream newArgsStr(newArgs);
    newArgsStr << HIP::VOID_PTR_PTR_CAST << "&" << varName << ","
               << bufSizeExprStr;
    ct::Replacement argsRepl(SM, argRng, newArgsStr.str());
    llvm::consumeError(m_replacements.add(argsRepl));
  } break;

  default:
    break;
  }

  return false;
}