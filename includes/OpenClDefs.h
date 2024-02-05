#pragma once
#include "clang/Basic/AddressSpaces.h"
#include <map>
#include <set>
#include <string>
namespace OpenCL {
// Kernel
// Parameters
namespace AddrSpace {
const std::string GLOBAL = "__global";
const std::string CONSTANT = "__constant";
const std::string LOCAL = "__local";
const std::set<std::string> SPACES_SET{GLOBAL, CONSTANT, LOCAL};

} // namespace AddrSpace

const std::set<clang::LangAS> OPENCL_ADDR_SPACES{
    clang::LangAS::opencl_global,     clang::LangAS::opencl_local,
    clang::LangAS::opencl_constant,   clang::LangAS::opencl_private,
    clang::LangAS::opencl_generic,    clang::LangAS::opencl_global_device,
    clang::LangAS::opencl_global_host};

// Functions
enum class KernelFuncs : uint32_t {
  GET_GLOBAL_ID,
  GET_LOCAL_ID,
  GET_GROUP_ID,
  GET_LOCAL_SIZE,
  BARRIER
};

const std::string GET_GLOBAL_ID = "get_global_id";
const std::string GET_LOCAL_ID = "get_local_id";
const std::string GET_GROUP_ID = "get_group_id";
const std::string GET_LOCAL_SIZE = "get_local_size";

const std::string BARRIER = "barrier";

const std::map<std::string, KernelFuncs> KERNEL_FUNC_MAP{
    {GET_GLOBAL_ID, KernelFuncs::GET_GLOBAL_ID},
    {GET_LOCAL_ID, KernelFuncs::GET_LOCAL_ID},
    {GET_GROUP_ID, KernelFuncs::GET_GROUP_ID},
    {GET_LOCAL_SIZE, KernelFuncs::GET_LOCAL_SIZE},
    {BARRIER, KernelFuncs::BARRIER}};

// definitions
const unsigned char CLK_LOCAL_MEM_FENCE = 0x1;
const unsigned char CLK_GLOBAL_MEM_FENCE = 0x2;

// Host
enum class HostFuncs { clCreateBuffer, clEnqueueWriteBuffer };

// Memory function calls
const std::string CL_CREATE_BUFFER = "clCreateBuffer";
const std::string CL_ENQUEUE_WRITE_BUFFER = "clEnqueueWriteBuffer";
const std::set<HostFuncs> HOST_MEM_FUNCS{HostFuncs::clCreateBuffer,
                                         HostFuncs::clEnqueueWriteBuffer};

const std::map<std::string, HostFuncs> HOST_FUNC_MAP{
    {CL_CREATE_BUFFER, HostFuncs::clCreateBuffer},
    {CL_ENQUEUE_WRITE_BUFFER, HostFuncs::clEnqueueWriteBuffer}};

} // namespace OpenCL