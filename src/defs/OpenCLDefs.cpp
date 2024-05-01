#include "defs/OpenCLDefs.h"
#include "clang/Basic/AddressSpaces.h"
#include <map>
#include <set>
#include <string>
using namespace OpenCL;

// Kernel
// Parameters
const std::string OpenCL::AddrSpace::GLOBAL = "__global";
const std::string OpenCL::AddrSpace::CONSTANT = "__constant";
const std::string OpenCL::AddrSpace::LOCAL = "__local";
const std::set<std::string> OpenCL::AddrSpace::SPACES_SET{GLOBAL, CONSTANT,
                                                          LOCAL};

const std::set<clang::LangAS> OpenCL::OPENCL_ADDR_SPACES{
    clang::LangAS::opencl_global,     clang::LangAS::opencl_local,
    clang::LangAS::opencl_constant,   clang::LangAS::opencl_private,
    clang::LangAS::opencl_generic,    clang::LangAS::opencl_global_device,
    clang::LangAS::opencl_global_host};

// Functions

const std::string OpenCL::GET_GLOBAL_ID = "get_global_id";
const std::string OpenCL::GET_LOCAL_ID = "get_local_id";
const std::string OpenCL::GET_GROUP_ID = "get_group_id";
const std::string OpenCL::GET_LOCAL_SIZE = "get_local_size";
const std::string OpenCL::GET_NUM_GROUPS = "get_num_groups";

const std::string OpenCL::BARRIER = "barrier";

const std::string OpenCL::DOT = "dot";
const std::string OpenCL::MAD = "mad";

const std::map<std::string, KernelFuncs> OpenCL::KERNEL_FUNC_MAP{
    {GET_GLOBAL_ID, KernelFuncs::GET_GLOBAL_ID},
    {GET_LOCAL_ID, KernelFuncs::GET_LOCAL_ID},
    {GET_GROUP_ID, KernelFuncs::GET_GROUP_ID},
    {GET_LOCAL_SIZE, KernelFuncs::GET_LOCAL_SIZE},
    {GET_NUM_GROUPS, KernelFuncs::GET_NUM_GROUPS},
    {DOT, KernelFuncs::DOT},
    {MAD, KernelFuncs::MAD},
    {BARRIER, KernelFuncs::BARRIER}};

// definitions
const unsigned char OpenCL::CLK_LOCAL_MEM_FENCE = 0x1;
const unsigned char OpenCL::CLK_GLOBAL_MEM_FENCE = 0x2;

// Host
// Types
const std::string OpenCL::CL_MEM_UNDERLYING = "_cl_mem";
const std::string OpenCL::CL_MEM = "cl_mem";
const std::string OpenCL::CL_PROGRAM = "cl_program";
const std::string OpenCL::CL_PLATFORM_ID = "cl_platform_id";
const std::string OpenCL::CL_DEVICE_ID = "cl_device_id";
const std::string OpenCL::CL_INT = "cl_int";
const std::string OpenCL::CL_UINT = "cl_uint";
const std::string OpenCL::CL_KERNEL = "cl_kernel";
const std::string OpenCL::CL_CONTEXT = "cl_context";
const std::string OpenCL::CL_COMMAND_QUEUE = "cl_command_queue";

// values
const unsigned OpenCL::CL_SUCCESS_VAL = 0x0;
const std::string OpenCL::CL_SUCCESS = "CL_SUCCESS";

// Functions
// Memory function calls
const std::string OpenCL::CL_CREATE_BUFFER = "clCreateBuffer";
const std::string OpenCL::CL_ENQUEUE_WRITE_BUFFER = "clEnqueueWriteBuffer";
const std::string OpenCL::CL_ENQUEUE_READ_BUFFER = "clEnqueueReadBuffer";
const std::string OpenCL::CL_RELEASE_MEM_OBJECT = "clReleaseMemObject";
const std::set<HostFuncs> OpenCL::HOST_MEM_FUNCS{
    HostFuncs::clCreateBuffer,
    HostFuncs::clEnqueueWriteBuffer,
    HostFuncs::clEnqueueReadBuffer,
    HostFuncs::clReleaseMemObject,
};

// Generic function calls
const std::string OpenCL::GET_KWG_INFO = "clGetKernelWorkGroupInfo";
const std::set<HostFuncs> OpenCL::HOST_GENERIC_FUNCS{HostFuncs::clGetCWGInfo};

// generic definition
const int OpenCL::CL_KERNEL_WORK_GROUP_SIZE = 0x11B0;

// Kernel launch function calls
const std::string OpenCL::CL_SET_KERNEL_ARG = "clSetKernelArg";
const std::string OpenCL::CL_ENQUEUE_NDRANGE_BUFFER = "clEnqueueNDRangeKernel";
const std::string OpenCL::CL_ENQUEUE_TASK = "clEnqueueTask";
const std::string OpenCL::CL_CREATE_KERNEL = "clCreateKernel";
const std::set<HostFuncs> OpenCL::HOST_KERNEL_FUNCS{
    HostFuncs::clSetKernelArg, HostFuncs::clEnqueueNDRangeKernel,
    HostFuncs::clCreateKernel, HostFuncs::clEnqueueTask};

// Redundant function calls
const std::string OpenCL::CL_GET_PLATFORM_IDS = "clGetPlatformIDs";
const std::string OpenCL::CL_GET_DEVICE_IDS = "clGetDeviceIDs";
const std::string OpenCL::CL_CREATE_CONTEXT = "clCreateContext";
const std::string OpenCL::CL_CREATE_COMMAND_QUEUE = "clCreateCommandQueue";
const std::string OpenCL::CL_CREATE_PROGRAM_W_SOURCE =
    "clCreateProgramWithSource";
const std::string OpenCL::CL_BUILD_PROGRAM = "clBuildProgram";
const std::string OpenCL::CL_FINISH = "clFinish";
const std::string OpenCL::CL_RELEASE_PROGRAM = "clReleaseProgram";
const std::string OpenCL::CL_RELEASE_KERNEL = "clReleaseKernel";
const std::string OpenCL::CL_RELEASE_CONTEXT = "clReleaseContext";
const std::string OpenCL::CL_RELEASE_COMMAND_QUEUE = "clReleaseCommandQueue";
const std::string OpenCL::CL_FLUSH = "clFlush";
const std::set<HostFuncs> OpenCL::HOST_REDUNDANT_FUNCS{
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
    HostFuncs::clReleaseCommandQueue,
    HostFuncs::clFlush};

const std::map<std::string, HostFuncs> OpenCL::HOST_FUNC_MAP{
    {CL_CREATE_BUFFER, HostFuncs::clCreateBuffer},
    {CL_ENQUEUE_WRITE_BUFFER, HostFuncs::clEnqueueWriteBuffer},
    {CL_ENQUEUE_READ_BUFFER, HostFuncs::clEnqueueReadBuffer},
    {CL_RELEASE_MEM_OBJECT, HostFuncs::clReleaseMemObject},
    {CL_SET_KERNEL_ARG, HostFuncs::clSetKernelArg},
    {CL_ENQUEUE_NDRANGE_BUFFER, HostFuncs::clEnqueueNDRangeKernel},
    {CL_ENQUEUE_TASK, HostFuncs::clEnqueueTask},
    {CL_CREATE_KERNEL, HostFuncs::clCreateKernel},
    {GET_KWG_INFO, HostFuncs::clGetCWGInfo},
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
    {CL_FLUSH, HostFuncs::clFlush}};
