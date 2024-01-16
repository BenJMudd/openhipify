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
enum class AUX_FUNCS : uint32_t { TEST_FUNC };

const std::string TEST_FUNC = "__global void test() {/*Hi*/return;}";

const std::map<AUX_FUNCS, std::string> AUX_FUNC_MAP{
    {AUX_FUNCS::TEST_FUNC, TEST_FUNC}};

} // namespace HIP