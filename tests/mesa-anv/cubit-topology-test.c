#include "cubit-topology.h"
#include "intel/dev/intel_device_info.h"
#include <assert.h>
#include <string.h>

int main(void)
{
   struct intel_device_info base = {0};
   base.pci_device_id = 0x46d2;
   base.platform = INTEL_PLATFORM_ADL;
   base.ver = 12;
   base.verx10 = 120;
   base.timestamp_frequency = 12345;
   memset(base.subslice_masks, 0xa5, sizeof(base.subslice_masks));
   memset(base.eu_masks, 0xa5, sizeof(base.eu_masks));
   memset(base.num_subslices, 0xa5, sizeof(base.num_subslices));
   memset(base.ppipe_subslices, 0xa5, sizeof(base.ppipe_subslices));
   base.num_slices = 99;
   base.subslice_total = 99;
   base.l3_banks = 99;
   for (unsigned dss = 0; dss < 256; dss++) {
      for (unsigned pairs = 0; pairs < 256; pairs++) {
         uint16_t eus = 0;
         for (unsigned i = 0; i < 8; i++)
            if (pairs & (1u << i)) eus |= 3u << (2 * i);
         struct intel_device_info info = base;
         bool valid = cubit_mesa_adln_topology_masks(&info, dss, eus);
         assert(valid == (dss > 0 && dss < 64 && pairs > 0));
         if (!valid) {
            assert(memcmp(&info, &base, sizeof(info)) == 0);
            continue;
         }
         assert(info.timestamp_frequency == 12345);
         assert(info.slice_masks == 1 && info.subslice_masks[0] == dss);
         assert(info.eu_subslice_stride == 2 && info.eu_slice_stride == 12);
         for (unsigned i = 0; i < sizeof(info.eu_masks); i++) {
            unsigned expected = 0;
            if (i < 12 && (dss & (1u << (i / 2))))
               expected = (eus >> (8 * (i % 2))) & 255u;
            assert(info.eu_masks[i] == expected);
         }
         /* The adapter must finalize these itself, using pinned Mesa helpers.
          * Do not repair the result in the test before checking it. */
         unsigned count = __builtin_popcount(dss);
         assert(info.num_slices == 1 && info.num_subslices[0] == count);
         assert(info.subslice_total == count);
         assert(intel_device_info_eu_total(&info) ==
                count * 2u * __builtin_popcount(pairs));
         assert(intel_device_info_get_eu_count_first_subslice(&info) ==
                2u * __builtin_popcount(pairs));
         for (unsigned p = 0; p < INTEL_DEVICE_MAX_PIXEL_PIPES; p++)
            assert(info.ppipe_subslices[p] ==
                   (p < 3 ? (unsigned)__builtin_popcount((dss >> (p * 2)) & 3) : 0));
         assert(info.l3_banks == (count >= 6 ? 8 : count > 2 ? 6 : 4));
         /* Scratch addressing uses physical IDs, not a packed DSS count.
          * In particular mask 0x20 has one DSS but an exclusive ID bound 6. */
         unsigned bound = 0;
         for (unsigned bits = dss; bits; bits >>= 1) bound++;
         assert(intel_device_info_dual_subslice_id_bound(&info) == bound);
         assert(bound >= count);

         struct intel_device_info once = info;
         assert(cubit_mesa_adln_topology_masks(&info, dss, eus));
         assert(memcmp(&info, &once, sizeof(info)) == 0);

         /* A fresh stable query can remove or add fused resources. Replacing
          * a previously valid topology must be equivalent to starting fresh;
          * no stale sparse mask or derived count may survive. */
         unsigned next_dss = 64 - dss;
         uint16_t next_eus = (uint16_t)~eus;
         if (!next_eus) next_eus = 3;
         struct intel_device_info fresh = base;
         assert(cubit_mesa_adln_topology_masks(&fresh, next_dss, next_eus));
         assert(cubit_mesa_adln_topology_masks(&info, next_dss, next_eus));
         assert(memcmp(&info, &fresh, sizeof(info)) == 0);
      }
   }
   for (unsigned bit = 0; bit < 16; bit++) {
      struct intel_device_info info = base;
      assert(!cubit_mesa_adln_topology_masks(&info, 1, 1u << bit));
      assert(memcmp(&info, &base, sizeof(info)) == 0);
   }
   for (unsigned field = 0; field < 4; field++) {
      struct intel_device_info info = base;
      switch (field) {
      case 0: info.pci_device_id = 0; break;
      case 1: info.platform = INTEL_PLATFORM_TGL; break;
      case 2: info.ver = 11; break;
      case 3: info.verx10 = 125; break;
      }
      struct intel_device_info before = info;
      assert(!cubit_mesa_adln_topology_masks(&info, 1, 0xffff));
      assert(memcmp(&info, &before, sizeof(info)) == 0);
   }
   assert(!cubit_mesa_adln_topology_masks(NULL, 1, 0xffff));
   return 0;
}
