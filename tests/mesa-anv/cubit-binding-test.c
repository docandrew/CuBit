#include "cubit-binding.h"
#include <assert.h>
#include <stddef.h>
#include <stdio.h>

static void
reject(uint64_t address, uint64_t size, uint64_t offset, uint64_t bytes)
{
   struct cubit_anv_binding_range out = {1, 2, 3};
   assert(!cubit_anv_prepare_binding(address, size, offset, bytes, &out));
   assert(out.gpu_raw48 == 0 && out.bo_offset == 0 && out.bytes == 0);
}

int main(void)
{
   uint64_t grant_bytes = UINT64_MAX;
   assert(!cubit_anv_prepare_cpu_map(8192, 0, 1, NULL));
   for (uint64_t size = 1; size <= 8192; size++) {
      assert(cubit_anv_prepare_cpu_map(12288, 4096, size, &grant_bytes));
      assert(grant_bytes == (size <= 4096 ? 4096 : 8192));
      assert(grant_bytes >= size && grant_bytes - size < 4096);
   }
   const uint64_t invalid[][3] = {
      {0, 0, 1}, {4096, 0, 0}, {8192, 1, 1}, {8191, 0, 1},
      {8192, 8192, 1}, {8192, 4096, 4097}, {8192, UINT64_MAX, 1},
      {UINT64_MAX, 0, UINT64_MAX}, {UINT64_MAX - 4095, 0, UINT64_MAX}
   };
   for (unsigned i = 0; i < sizeof(invalid) / sizeof(invalid[0]); i++) {
      grant_bytes = UINT64_MAX;
      assert(!cubit_anv_prepare_cpu_map(invalid[i][0], invalid[i][1],
                                        invalid[i][2], &grant_bytes));
      assert(grant_bytes == 0);
   }
   assert(cubit_anv_prepare_cpu_map(UINT64_MAX - 4095, 0,
                                    UINT64_MAX - 4095, &grant_bytes));
   assert(grant_bytes == UINT64_MAX - 4095);
   struct cubit_anv_binding_range out;
   assert(!cubit_anv_prepare_binding(4096, 4096, 0, 4096, NULL));
   assert(cubit_anv_prepare_binding(0x200000, 16384, 4096, 8192, &out));
   assert(out.gpu_raw48 == 0x200000 && out.bo_offset == 4096 && out.bytes == 8192);
   /* All 16 upper bits must agree with bit47, in both address halves. */
   for (uint64_t upper = 0; upper <= 0xffff; upper++) {
      assert(cubit_anv_prepare_binding((upper << 48) | 4096,
                                       4096, 0, 4096, &out) == (upper == 0));
      assert(cubit_anv_prepare_binding((upper << 48) | (UINT64_C(1) << 47),
                                       4096, 0, 4096, &out) == (upper == 0xffff));
   }
   assert(cubit_anv_prepare_binding(UINT64_C(0xfffffffffffff000),
                                    4096, 0, 4096, &out));
   assert(out.gpu_raw48 == UINT64_C(0xfffffffff000));
   reject(UINT64_C(0xfffffffffffff000), 8192, 0, 8192);
   reject(0, 4096, 0, 4096);
   reject(4096, 4096, 0, 0);
   reject(4096, 4096, 8192, 4096);
   reject(4096, 4096, 4096, 4096);
   reject(4096, UINT64_MAX, UINT64_MAX - 4095, 4096);
   reject(4096, UINT64_MAX, 0, UINT64_MAX - 4095);
   for (uint64_t byte = 1; byte < 4096; byte++) {
      reject(4096 + byte, 8192, 0, 4096);
      reject(4096, 8192, byte, 4096);
      reject(4096, 8192, 0, byte);
   }
   puts("ANV binding boundary PASS: canonical bits, full pages, BO bounds, overflow; no IPC performed");
}
