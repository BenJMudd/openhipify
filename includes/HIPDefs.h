#pragma once

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
enum class AUX_FUNC_ID : uint32_t {
  GET_GLOBAL_ID,
  GET_LOCAL_ID,
  GET_GROUP_ID,
  GET_LOCAL_SIZE,
  DOT,
  MAD
};

const std::string GET_GLOBAL_ID_FUNC_NAME = "__get_global_id";
const std::string GET_GLOBAL_ID_BODY =
    "__device__ size_t " + GET_GLOBAL_ID_FUNC_NAME +
    "(uint dim) {"
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
    "}\n";

const std::string GET_LOCAL_ID_FUNC_NAME = "__get_local_id";
const std::string GET_LOCAL_ID_BODY = "__device__ size_t " +
                                      GET_LOCAL_ID_FUNC_NAME +
                                      "(uint dim) {"
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
                                      "}\n";

const std::string GET_GROUP_ID_FUNC_NAME = "__get_group_id";
const std::string GET_GROUP_ID_BODY = "__device__ size_t " +
                                      GET_GROUP_ID_FUNC_NAME +
                                      "(uint dim) {"
                                      "switch (dim) {"
                                      "case 0: {"
                                      "return hipBlockIdx_x;"
                                      "} break;"
                                      "case 1: {"
                                      "return hipBlockIdx_y;"
                                      "} break;"
                                      "case 2: {"
                                      "return hipBlockIdx_z;"
                                      "}"
                                      "break;"
                                      "default: {"
                                      "return 0;"
                                      "} break;"
                                      "}"
                                      "}\n";

const std::string GET_LOCAL_SIZE_FUNC_NAME = "__get_group_id";
const std::string GET_LOCAL_SIZE_BODY = "__device__ size_t " +
                                        GET_LOCAL_SIZE_FUNC_NAME +
                                        "(uint dim) {"
                                        "switch (dim) {"
                                        "case 0: {"
                                        "return hipBlockDim_x;"
                                        "} break;"
                                        "case 1: {"
                                        "return hipBlockDim_y;"
                                        "} break;"
                                        "case 2: {"
                                        "return hipBlockDim_z;"
                                        "}"
                                        "break;"
                                        "default: {"
                                        "return 0;"
                                        "} break;"
                                        "}"
                                        "}\n";

const std::string DOT_FUNC_NAME = "__dotf";
const std::string DOT_BODY = "__device__ float " + DOT_FUNC_NAME +
                             "(float4 a, float 4b) {return a.x * b.x + a.y * "
                             "b.y + a.z * b.z + a.w * b.w}\n";

const std::string MAD_FUNC_NAME = "__mad";
const std::string MAD_BODY =
    "__device__ float " + MAD_FUNC_NAME +
    "(float a, float b, float c) {return a * b + c;}\n ";

const std::map<AUX_FUNC_ID, std::pair<std::string, std::string>> AUX_FUNC_MAP{
    {AUX_FUNC_ID::GET_GLOBAL_ID, {GET_GLOBAL_ID_FUNC_NAME, GET_GLOBAL_ID_BODY}},
    {AUX_FUNC_ID::GET_LOCAL_ID, {GET_LOCAL_ID_FUNC_NAME, GET_LOCAL_ID_BODY}},
    {AUX_FUNC_ID::GET_GROUP_ID, {GET_GROUP_ID_FUNC_NAME, GET_GROUP_ID_BODY}},
    {AUX_FUNC_ID::GET_LOCAL_SIZE,
     {GET_LOCAL_SIZE_FUNC_NAME, GET_LOCAL_SIZE_BODY}},
    {AUX_FUNC_ID::DOT, {DOT_FUNC_NAME, DOT_BODY}},
    {AUX_FUNC_ID::MAD, {MAD_FUNC_NAME, MAD_BODY}}};

const std::map<OpenCL::KernelFuncs, AUX_FUNC_ID> OPENCL_HIP_AUX_FUNC_MAP{
    {OpenCL::KernelFuncs::GET_GLOBAL_ID, AUX_FUNC_ID::GET_GLOBAL_ID},
    {OpenCL::KernelFuncs::GET_LOCAL_ID, AUX_FUNC_ID::GET_LOCAL_ID},
    {OpenCL::KernelFuncs::GET_GROUP_ID, AUX_FUNC_ID::GET_GROUP_ID},
    {OpenCL::KernelFuncs::GET_LOCAL_SIZE, AUX_FUNC_ID::GET_LOCAL_SIZE},
    {OpenCL::KernelFuncs::DOT, AUX_FUNC_ID::DOT},
    {OpenCL::KernelFuncs::MAD, AUX_FUNC_ID::MAD}};

// Host
// Functions
const std::string MALLOC = "hipMalloc";
const std::string MEMCPY = "hipMemcpy";
const std::string FREE = "hipFree";
const std::string LAUNCHKERNELGGL = "hipLaunchKernelGGL";
// Types
const std::string ERROR = "hipError_t";
const std::string VOID_PTR = "void*";
const std::string VOID = "void";
const std::string VOID_PTR_PTR_CAST = "(void**)";
// Values
const std::string MEMCPY_HOST_DEVICE = "hipMemcpyHostToDevice";
const std::string MEMCPY_DEVICE_HOST = "hipMemcpyDeviceToHost";

// Props
const std::string PROPS_OBJ = "HIP_Prop";
const std::string PROPS_MTHREAD_P_BLOCK = "maxThreadsPerBlock";

// Snippets
// Only works for the first device
const std::string INIT_D_PROPS =
    "hipDeviceProp_t HIP_Prop;hipGetDeviceProperties(&HIP_Prop, 0);";

// Misc
const std::string EOL = ";";

} // namespace HIP