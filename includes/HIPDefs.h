#include "OpenClDefs.h"
#include <map>
#include <string>

namespace HIP {
// Kernel

const std::string GLOBAL_FUNC_ATTR = "__global__";

// Functions
const std::string BLOCK_DIM_GENERIC = "hipBlockDim_";
const std::string BLOCK_IDX_GENERIC = "hipBlockIdx_";
const std::string THREAD_IDX_GENERIC = "hipThreadIdx_";

const std::string THREAD_FENCE = "__threadfence()";
const std::string THREAD_FENCE_BLOCK = "__threadfence_block()";

// Auxiliary function fefinitions
enum class AUX_FUNCS : uint32_t { GET_GLOBAL_ID, GET_LOCAL_ID };

const std::string GET_GLOBAL_ID_DEF = "__get_global_id";
const std::string GET_GLOBAL_ID =
    "__device size_t __get_global_id(uint dim) {"
    "switch (dim) {"
    "case 0: {"
    "return hipBlockDim_x * hipBlockIdx_x + hipThreadIdx_x;"
    "} break;"
    "case 1: {"
    "return hipBlockDim_y * hipBlockIdx_y + hipThreadIdx_y;"
    "} break;"
    "case 2: {"
    "return hipBlockDim_z * hipBlockIdx_z + hipThreadIdx_z;"
    "}"
    "break;"
    "default: {"
    "return 0;"
    "} break;"
    "}"
    "}";

const std::string GET_LOCAL_ID_DEF = "__get_local_id";
const std::string GET_LOCAL_ID = "__device size_t __get_local_id(uint dim) {"
                                 "switch (dim) {"
                                 "case 0: {"
                                 "return hipThreadIdx_x;"
                                 "} break;"
                                 "case 1: {"
                                 "return hipThreadIdx_y;"
                                 "} break;"
                                 "case 2: {"
                                 "return hipThreadIdx_z;"
                                 "}"
                                 "break;"
                                 "default: {"
                                 "return 0;"
                                 "} break;"
                                 "}"
                                 "}";

const std::map<AUX_FUNCS, std::pair<std::string, std::string>> AUX_FUNC_MAP{
    {AUX_FUNCS::GET_GLOBAL_ID, {GET_GLOBAL_ID_DEF, GET_GLOBAL_ID}},
    {AUX_FUNCS::GET_LOCAL_ID, {GET_LOCAL_ID_DEF, GET_LOCAL_ID}}};

const std::map<OpenCL::KernelFuncs, AUX_FUNCS> OPENCL_HIP_AUX_FUNC_MAP{
    {OpenCL::KernelFuncs::GET_GLOBAL_ID, AUX_FUNCS::GET_GLOBAL_ID},
    {OpenCL::KernelFuncs::GET_LOCAL_ID, AUX_FUNCS::GET_LOCAL_ID}};

} // namespace HIP