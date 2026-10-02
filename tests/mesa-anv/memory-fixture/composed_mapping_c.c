#include "../../../userspace/mesa/anv/native_gpu_mapping.h"
uint32_t test_composed_mapping_c(uint32_t handle)
{
   struct cubit_cpu_mapping_tracker tracker = {.slot = 63};
   uint64_t address = 0;
   if (cubit_cpu_tracker_map(&tracker, handle, 4096, 4096, 1, &address) != 0 ||
       !address || tracker.lost)
      return 1;
   if (cubit_cpu_tracker_unmap(&tracker, handle, address, 4096, false) != 0 ||
       tracker.records[0].state != CUBIT_MAP_RETIRED || tracker.records[0].address)
      return 2;
   if (!cubit_cpu_tracker_drain(&tracker))
      return 3;
   return 0;
}
