#pragma once
namespace OpenCL {
// Kernel

// Functions
enum class KernelFuncs : uint32_t { GET_GLOBAL_ID };
const std::string GET_GLOBAL_ID = "get_global_id";
const std::map<std::string, KernelFuncs> KERNEL_FUNC_MAP{
    {GET_GLOBAL_ID, KernelFuncs::GET_GLOBAL_ID}};

} // namespace OpenCL