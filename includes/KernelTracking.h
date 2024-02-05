#include "clang/AST/Expr.h"
#include "clang/Basic/SourceLocation.h"
#include <string>

class KernelTracker {
public:
private:
  struct KernelInfo {
    using LocationInfoVector =
        std::vector<std::pair<clang::SourceLocation, std::string>>;
    std::string funcName;
    LocationInfoVector args;
    LocationInfoVector launches;
  };

  std::map<clang::DeclRefExpr *, KernelInfo> m_tracker;
};