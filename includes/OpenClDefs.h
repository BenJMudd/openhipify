#pragma once
#include <map>
#include <string>

namespace OpenCL {
// Kernel

// Functions
enum class KernelFuncs : uint32_t { GET_GLOBAL_ID, GET_LOCAL_ID };
const std::string GET_GLOBAL_ID = "get_global_id";
const std::string GET_LOCAL_ID = "get_local_id";
const std::map<std::string, KernelFuncs> KERNEL_FUNC_MAP{
    {GET_GLOBAL_ID, KernelFuncs::GET_GLOBAL_ID},
    {GET_LOCAL_ID, KernelFuncs::GET_LOCAL_ID}};

} // namespace OpenCL