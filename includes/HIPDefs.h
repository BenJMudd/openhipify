namespace HIP {
// Kernel

const std::string GLOBAL_FUNC_ATTR = "__global__";

// Functions
const std::string BLOCK_DIM_GENERIC = "hipBlockDim_";
const std::string BLOCK_IDX_GENERIC = "hipBlockIdx_";
const std::string THREAD_IDX_GENERIC = "hipThreadIdx_";

const std::string THREAD_FENCE = "__threadfence()";
const std::string THREAD_FENCE_BLOCK = "__threadfence_block()";

} // namespace HIP