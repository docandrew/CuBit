#include "intel/dev/intel_device_info.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>

/* Original, unmodified Mesa helper from the pinned Linux-hosted archive. */
extern bool intel_device_info_i915_update_from_masks(struct intel_device_info *,
                                                     uint32_t, uint32_t, uint32_t);
int main(void)
{
   unsetenv("INTEL_FORCE_PROBE");
   const int ids[] = {0x46d2, 0x1912};
   for (unsigned platform = 0; platform < 2; platform++) {
      struct intel_device_info base = {0};
      assert(intel_device_info_init_runtime_defaults(ids[platform], &base));
      const unsigned slices = platform == 0 ? 1 : 7;
      const unsigned subslices = platform == 0 ? 63 : 7;
      const unsigned eus = platform == 0 ? 16 : 8;
      for (unsigned s = 1; s <= slices; s++)
         for (unsigned ss = 1; ss <= subslices; ss++)
            for (unsigned eu = 1; eu <= eus; eu++) {
               struct intel_device_info reference = base, extracted = base;
               unsigned total = __builtin_popcount(s) * __builtin_popcount(ss) * eu;
               assert(intel_device_info_i915_update_from_masks(&reference, s, ss, total));
               assert(intel_device_info_update_from_masks(&extracted, s, ss, total));
               assert(memcmp(&reference, &extracted, sizeof(reference)) == 0);
            }
   }
   return 0;
}
