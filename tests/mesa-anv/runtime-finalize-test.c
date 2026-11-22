#include "cubit-topology.h"
#include "intel/dev/intel_device_info.h"
#include <assert.h>
#include <string.h>
#include <stdlib.h>

int main(void)
{
   /* Exercise the real PCI defaults, not only synthetic structures. The
    * normal offline helper applies workarounds too early for discovery. */
   unsetenv("INTEL_FORCE_PROBE");
   struct intel_device_info defaults = {0};
   assert(intel_device_info_init_runtime_defaults(0x46d2, &defaults));
   assert(defaults.kmd_type == INTEL_KMD_TYPE_INVALID);
   assert(defaults.pci_device_id == 0x46d2);
   assert(defaults.platform == INTEL_PLATFORM_ADL);
   assert(defaults.verx10 == 120);
   struct intel_device_info unchanged = defaults;
   assert(!intel_device_info_init_runtime_defaults(-1, &unchanged));
   assert(memcmp(&unchanged, &defaults, sizeof(defaults)) == 0);
   assert(!intel_device_info_init_runtime_defaults(0x46d2, NULL));
   setenv("INTEL_FORCE_PROBE", "!46d2", 1);
   assert(!intel_device_info_init_runtime_defaults(0x46d2, &unchanged));
   assert(memcmp(&unchanged, &defaults, sizeof(defaults)) == 0);
   unsetenv("INTEL_FORCE_PROBE");
   for (unsigned dss = 1; dss < 64; dss++) {
      for (unsigned pairs = 1; pairs < 256; pairs++) {
         struct intel_device_info info = {0};
         info.pci_device_id = 0x46d2;
         info.platform = INTEL_PLATFORM_ADL;
         info.ver = 12;
         info.verx10 = 120;
         info.max_vs_threads = 111;
         info.max_tcs_threads = 112;
         info.max_tes_threads = 113;
         info.max_gs_threads = 114;
         info.max_wm_threads = 115;
         info.urb.max_entries[MESA_SHADER_GEOMETRY] = 2048;
         uint16_t eus = 0;
         for (unsigned p = 0; p < 8; p++)
            if (pairs & (1u << p)) eus |= 3u << (p * 2);
         struct intel_device_info actual = defaults;
         assert(cubit_mesa_adln_topology_masks(&actual, dss, eus));
         intel_device_info_finalize_runtime(&actual);
         assert(actual.kmd_type == INTEL_KMD_TYPE_INVALID);
         assert(actual.max_scratch_ids[MESA_SHADER_VERTEX] ==
                actual.max_vs_threads);
         assert(actual.urb.max_entries[MESA_SHADER_GEOMETRY] ==
                (__builtin_popcount(dss) * __builtin_popcount(pairs) * 2 <= 32
                 ? 1024 : 1536));
         assert(cubit_mesa_adln_topology_masks(&info, dss, eus));
         intel_device_info_finalize_runtime(&info);
         unsigned bound = 0;
         for (unsigned bits = dss; bits; bits >>= 1) bound++;
         assert(info.max_scratch_ids[MESA_SHADER_COMPUTE] == 128 * bound);
         assert(info.max_scratch_ids[MESA_SHADER_VERTEX] == 111);
         assert(info.max_scratch_ids[MESA_SHADER_TESS_CTRL] == 112);
         assert(info.max_scratch_ids[MESA_SHADER_TESS_EVAL] == 113);
         assert(info.max_scratch_ids[MESA_SHADER_GEOMETRY] == 114);
         assert(info.max_scratch_ids[MESA_SHADER_FRAGMENT] == 115);
         for (unsigned engine = INTEL_ENGINE_CLASS_RENDER;
              engine < sizeof(info.engine_class_prefetch) /
                       sizeof(info.engine_class_prefetch[0]); engine++)
            assert(info.engine_class_prefetch[engine] == 512);
         unsigned total = __builtin_popcount(dss) * __builtin_popcount(pairs) * 2;
         /* ADL's Wa_18012660806 first caps GS at 1536; the small-EU
          * topology workaround must run afterward and lower it to 1024. */
         assert(info.urb.max_entries[MESA_SHADER_GEOMETRY] ==
                (total <= 32 ? 1024 : 1536));
      }
   }
   return 0;
}
