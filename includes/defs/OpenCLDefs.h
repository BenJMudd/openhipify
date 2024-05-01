#pragma once

#include "clang/Basic/AddressSpaces.h"
#include <map>
#include <set>
#include <string>
namespace OpenCL {
// Kernel
// Parameters
namespace AddrSpace {
extern const std::string GLOBAL;
extern const std::string CONSTANT;
extern const std::string LOCAL;
extern const std::set<std::string> SPACES_SET;

} // namespace AddrSpace

extern const std::set<clang::LangAS> OPENCL_ADDR_SPACES;

// Functions
enum class KernelFuncs : uint32_t {
  GET_GLOBAL_ID,
  GET_LOCAL_ID,
  GET_GROUP_ID,
  GET_LOCAL_SIZE,
  GET_NUM_GROUPS,
  BARRIER,
  DOT,
  MAD
};

extern const std::string GET_GLOBAL_ID;
extern const std::string GET_LOCAL_ID;
extern const std::string GET_GROUP_ID;
extern const std::string GET_LOCAL_SIZE;
extern const std::string GET_NUM_GROUPS;

extern const std::string BARRIER;

extern const std::string DOT;
extern const std::string MAD;

extern const std::map<std::string, KernelFuncs> KERNEL_FUNC_MAP;

// definitions
extern const unsigned char CLK_LOCAL_MEM_FENCE;
extern const unsigned char CLK_GLOBAL_MEM_FENCE;

// Host
// Types
extern const std::string CL_MEM_UNDERLYING;
extern const std::string CL_MEM;
extern const std::string CL_PROGRAM;
extern const std::string CL_PLATFORM_ID;
extern const std::string CL_DEVICE_ID;
extern const std::string CL_INT;
extern const std::string CL_UINT;
extern const std::string CL_KERNEL;
extern const std::string CL_CONTEXT;
extern const std::string CL_COMMAND_QUEUE;

// values
extern const unsigned CL_SUCCESS_VAL;
extern const std::string CL_SUCCESS;
// Functions
enum class HostFuncs {
  clCreateBuffer,
  clEnqueueWriteBuffer,
  clEnqueueReadBuffer,
  clReleaseMemObject,
  clSetKernelArg,
  clEnqueueNDRangeKernel,
  clCreateKernel,
  clGetCWGInfo,
  clEnqueueTask,
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
  clReleaseCommandQueue,
  clFlush
};

// Memory function calls
extern const std::string CL_CREATE_BUFFER;
extern const std::string CL_ENQUEUE_WRITE_BUFFER;
extern const std::string CL_ENQUEUE_READ_BUFFER;
extern const std::string CL_RELEASE_MEM_OBJECT;
extern const std::set<HostFuncs> HOST_MEM_FUNCS;

// Generic function calls
extern const std::string GET_KWG_INFO;
extern const std::set<HostFuncs> HOST_GENERIC_FUNCS;

// generic definition
extern const int CL_KERNEL_WORK_GROUP_SIZE;

// Kernel launch function calls
extern const std::string CL_SET_KERNEL_ARG;
extern const std::string CL_ENQUEUE_NDRANGE_BUFFER;
extern const std::string CL_ENQUEUE_TASK;
extern const std::string CL_CREATE_KERNEL;
extern const std::set<HostFuncs> HOST_KERNEL_FUNCS;

// Redundant function calls
extern const std::string CL_GET_PLATFORM_IDS;
extern const std::string CL_GET_DEVICE_IDS;
extern const std::string CL_CREATE_CONTEXT;
extern const std::string CL_CREATE_COMMAND_QUEUE;
extern const std::string CL_CREATE_PROGRAM_W_SOURCE;
extern const std::string CL_BUILD_PROGRAM;
extern const std::string CL_FINISH;
extern const std::string CL_RELEASE_PROGRAM;
extern const std::string CL_RELEASE_KERNEL;
extern const std::string CL_RELEASE_CONTEXT;
extern const std::string CL_RELEASE_COMMAND_QUEUE;
extern const std::string CL_FLUSH;
extern const std::set<HostFuncs> HOST_REDUNDANT_FUNCS;

extern const std::map<std::string, HostFuncs> HOST_FUNC_MAP;
} // namespace OpenCL