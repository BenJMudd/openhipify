#pragma once
#include <map>
#include <string>

namespace OpenCL {
// Kernel

// Functions
enum class KernelFuncs : uint32_t {
  GET_GLOBAL_ID,
  GET_LOCAL_ID,
  GET_GROUP_ID,
  GET_LOCAL_SIZE
};
const std::string GET_GLOBAL_ID = "get_global_id";
const std::string GET_LOCAL_ID = "get_local_id";
const std::string GET_GROUP_ID = "get_group_id";
const std::string GET_LOCAL_SIZE = "get_local_size";
const std::map<std::string, KernelFuncs> KERNEL_FUNC_MAP{
    {GET_GLOBAL_ID, KernelFuncs::GET_GLOBAL_ID},
    {GET_LOCAL_ID, KernelFuncs::GET_LOCAL_ID},
    {GET_GROUP_ID, KernelFuncs::GET_GROUP_ID},
    {GET_LOCAL_SIZE, KernelFuncs::GET_LOCAL_SIZE}};

} // namespace OpenCL