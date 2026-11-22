/* Pure hosted helper test, not a GPU mapping or authority check. */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include "intel/common/intel_address.h"

int main(void)
{
   const uint64_t mask = UINT64_C(0xffffffffffff);
   const uint64_t samples[] = {0, 1, 4095, 4096,
      UINT64_C(0x7fffffffffff), UINT64_C(0x800000000000),
      UINT64_C(0xfffffffff000), mask};
   for (uint64_t upper = 0; upper <= 0xffff; ++upper) {
      for (unsigned i = 0; i < sizeof(samples)/sizeof(samples[0]); ++i) {
         uint64_t raw = samples[i];
         uint64_t input = (upper << 48) | raw;
         uint64_t canonical = raw | ((raw & (UINT64_C(1) << 47)) ? ~mask : 0);
         assert(intel_48b_address(input) == raw);
         assert(intel_canonical_address(input) == canonical);
         assert(intel_48b_address(intel_canonical_address(input)) == raw);
      }
   }
   puts("Mesa address helpers PASS: 524288 inputs (conversion, not validation)");
}
