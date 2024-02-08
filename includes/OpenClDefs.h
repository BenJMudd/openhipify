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
// Types
const std::string CL_MEM_UNDERLYING = "_cl_mem";

// Functions
enum class HostFuncs {
  clCreateBuffer,
  clEnqueueWriteBuffer,
  clSetKernelArg,
  clEnqueueNDRangeKernel,
  clCreateKernel,
  clGetPlatformIDs,
  clGetDeviceIDs,
  clCreateContext,
  clCreateCommandQueue,
  clCreateProgramWithSource,
  clBuildProgram,
  clFinish,
  clReleaseProgram,
  clReleaseKernel,
  clReleaseContext,
  clReleaseCommandQueue
};

// Memory function calls
const std::string CL_CREATE_BUFFER = "clCreateBuffer";
const std::string CL_ENQUEUE_WRITE_BUFFER = "clEnqueueWriteBuffer";
const std::set<HostFuncs> HOST_MEM_FUNCS{HostFuncs::clCreateBuffer,
                                         HostFuncs::clEnqueueWriteBuffer};

// Kernel launch function calls
const std::string CL_SET_KERNEL_ARG = "clSetKernelArg";
const std::string CL_ENQUEUE_NDRANGE_BUFFER = "clEnqueueNDRangeKernel";
const std::string CL_CREATE_KERNEL = "clCreateKernel";
const std::set<HostFuncs> HOST_KERNEL_FUNCS{HostFuncs::clSetKernelArg,
                                            HostFuncs::clEnqueueNDRangeKernel,
                                            HostFuncs::clCreateKernel};

// Redundant function calls
const std::string CL_GET_PLATFORM_IDS = "clGetPlatformIDs";
const std::string CL_GET_DEVICE_IDS = "clGetDeviceIDs";
const std::string CL_CREATE_CONTEXT = "clCreateContext";
const std::string CL_CREATE_COMMAND_QUEUE = "clCreateCommandQueue";
const std::string CL_CREATE_PROGRAM_W_SOURCE = "clCreateProgramWithSource";
const std::string CL_BUILD_PROGRAM = "clBuildProgram";
const std::string CL_FINISH = "clFinish";
const std::string CL_RELEASE_PROGRAM = "clReleaseProgram";
const std::string CL_RELEASE_KERNEL = "clReleaseKernel";
const std::string CL_RELEASE_CONTEXT = "clReleaseContext";
const std::string CL_RELEASE_COMMAND_QUEUE = "clReleaseCommandQueue";
const std::set<HostFuncs> HOST_REDUNDANT_FUNCS{
    HostFuncs::clGetPlatformIDs,
    HostFuncs::clGetDeviceIDs,
    HostFuncs::clCreateContext,
    HostFuncs::clCreateCommandQueue,
    HostFuncs::clCreateProgramWithSource,
    HostFuncs::clBuildProgram,
    HostFuncs::clFinish,
    HostFuncs::clReleaseProgram,
    HostFuncs::clReleaseKernel,
    HostFuncs::clReleaseContext,
    HostFuncs::clReleaseCommandQueue};

const std::map<std::string, HostFuncs> HOST_FUNC_MAP{
    {CL_CREATE_BUFFER, HostFuncs::clCreateBuffer},
    {CL_ENQUEUE_WRITE_BUFFER, HostFuncs::clEnqueueWriteBuffer},
    {CL_SET_KERNEL_ARG, HostFuncs::clSetKernelArg},
    {CL_ENQUEUE_NDRANGE_BUFFER, HostFuncs::clEnqueueNDRangeKernel},
    {CL_CREATE_KERNEL, HostFuncs::clCreateKernel},
    {CL_GET_PLATFORM_IDS, HostFuncs::clGetPlatformIDs},
    {CL_GET_DEVICE_IDS, HostFuncs::clGetDeviceIDs},
    {CL_CREATE_CONTEXT, HostFuncs::clCreateContext},
    {CL_CREATE_COMMAND_QUEUE, HostFuncs::clCreateCommandQueue},
    {CL_CREATE_PROGRAM_W_SOURCE, HostFuncs::clCreateProgramWithSource},
    {CL_BUILD_PROGRAM, HostFuncs::clBuildProgram},
    {CL_FINISH, HostFuncs::clFinish},
    {CL_RELEASE_PROGRAM, HostFuncs::clReleaseProgram},
    {CL_RELEASE_KERNEL, HostFuncs::clReleaseKernel},
    {CL_RELEASE_CONTEXT, HostFuncs::clReleaseContext},
    {CL_RELEASE_COMMAND_QUEUE, HostFuncs::clReleaseCommandQueue},
};

} // namespace OpenCL