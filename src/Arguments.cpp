#include "Arguments.h"
cl::OptionCategory
    OpenHipifyToolTemplateCategory("OpenCL to HIP transpiler options");

cl::opt<bool> NoKernelArgProtection(
    "no-kernel-arg-protection",
    cl::desc("Lowers kernel argument range errors into warnings"),
    cl::value_desc("no-kernel-arg-protection"),
    cl::cat(OpenHipifyToolTemplateCategory));