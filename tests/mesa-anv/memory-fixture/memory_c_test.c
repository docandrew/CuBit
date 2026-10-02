#include "../../../userspace/mesa/anv/native_gpu_memory.h"
#include <assert.h>

void memory_c_test(void)
{
   uint64_t output = 99;
   const uint64_t reference = UINT64_C(0xffffffff00000fff);
   assert(cubit_intel_acquire_view(63, reference, 4096, 8192, 1, &output) == 0);
   assert(output == UINT64_C(0x700012345000));
   assert(cubit_intel_return_view(reference) == 0);
   output = 99;
   assert(cubit_intel_acquire_view(64, reference, 4096, 8192, 1, &output) == 1);
   assert(output == 0);
   assert(cubit_intel_acquire_view(63, reference, 4096, 8192, 1, 0) == 1);
}
