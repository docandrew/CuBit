#include "../../../userspace/mesa/anv/native_gpu_memory.h"
#include <assert.h>
#include "../../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
/* The index a test endpoint was made from. */
#define TEST_INDEX(e) ((unsigned)((e).opaque & 0xffffffffu))

void memory_c_test(void)
{
   uint64_t output = 99;
   const uint64_t reference = UINT64_C(0xffffffff00000fff);
   assert(cubit_intel_acquire_view(TEST_ENDPOINT(63), reference, 4096, 8192, 1, &output) == 0);
   assert(output == UINT64_C(0x700012345000));
   assert(cubit_intel_return_view(reference) == 0);
   output = 99;
   assert(cubit_intel_acquire_view(CUBIT_NO_ENDPOINT, reference, 4096, 8192, 1, &output) == 1);
   assert(output == 0);
   assert(cubit_intel_acquire_view(TEST_ENDPOINT(63), reference, 4096, 8192, 1, 0) == 1);
}
