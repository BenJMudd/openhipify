#pragma once

#include "defs/OpenCLDefs.h"
#include <map>
#include <string>

namespace HIP {
// Kernel

extern const std::string GLOBAL_FUNC_ATTR;

// Functions
extern const std::string BLOCK_DIM_GENERIC;
extern const std::string BLOCK_IDX_GENERIC;
extern const std::string THREAD_IDX_GENERIC;

extern const std::string THREAD_FENCE;
extern const std::string THREAD_FENCE_BLOCK;

// Auxiliary function definitions
enum class AUX_FUNC_ID : uint32_t {
  GET_GLOBAL_ID,
  GET_LOCAL_ID,
  GET_GROUP_ID,
  GET_LOCAL_SIZE,
  DOT,
  MAD
};

extern const std::string GET_GLOBAL_ID_FUNC_NAME;
extern const std::string GET_GLOBAL_ID_BODY;
extern const std::string GET_LOCAL_ID_FUNC_NAME;
extern const std::string GET_LOCAL_ID_BODY;
extern const std::string GET_GROUP_ID_FUNC_NAME;
extern const std::string GET_GROUP_ID_BODY;
extern const std::string GET_LOCAL_SIZE_FUNC_NAME;
extern const std::string GET_LOCAL_SIZE_BODY;
extern const std::string DOT_FUNC_NAME;
extern const std::string DOT_BODY;
extern const std::string MAD_FUNC_NAME;
extern const std::string MAD_BODY;

extern const std::map<AUX_FUNC_ID, std::pair<std::string, std::string>>
    AUX_FUNC_MAP;

extern const std::map<OpenCL::KernelFuncs, AUX_FUNC_ID> OPENCL_HIP_AUX_FUNC_MAP;

// Host
// Functions
extern const std::string MALLOC;
extern const std::string MEMCPY;
extern const std::string FREE;
extern const std::string LAUNCHKERNELGGL;
// Types
extern const std::string ERROR;
extern const std::string VOID_PTR;
extern const std::string VOID;
extern const std::string VOID_PTR_PTR_CAST;
// Values
extern const std::string MEMCPY_HOST_DEVICE;
extern const std::string MEMCPY_DEVICE_HOST;
extern const std::string HIP_SUCCESS;

// Props
extern const std::string PROPS_OBJ;
extern const std::string PROPS_MTHREAD_P_BLOCK;

// Snippets
// Only works for the first device
extern const std::string INIT_D_PROPS;

// Misc
extern const std::string EOL;

} // namespace HIP