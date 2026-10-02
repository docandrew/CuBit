#include "cubit-device-query.h"

/* Native regression called by the Ada IPC test, NOT a Mesa/Vulkan device.
 * Uses production C decoder + callback + Ada capCall across the real kernel. */
uint32_t cubit_gpu_query_smoke(uint64_t granted_slot, uint64_t empty_slot)
{
   struct cubit_gpu_native_endpoint endpoint = {empty_slot};
   struct cubit_gpu_device_snapshot data = {
      .device = 0xabcd, .pci_revision = 0x12, .dss_mask = 0x34, .eu_mask = 0x5678,
   };
   if (cubit_gpu_query_device(cubit_gpu_native_query_call, &endpoint, &data))
      return 1;
   if (data.device != 0xabcd || data.pci_revision != 0x12 ||
       data.dss_mask != 0x34 || data.eu_mask != 0x5678)
      return 2;
   endpoint.slot = granted_slot;
   /* Repetition exercises output lifetimes across both language boundaries. */
   for (unsigned i = 0; i < 32; i++) {
      if (!cubit_gpu_query_device(cubit_gpu_native_query_call, &endpoint, &data))
         return 3;
      if (data.device != 0x46d2 || data.pci_revision != 17 ||
          data.dss_mask != 1 || data.eu_mask != 0xffff)
         return 4;
   }
   return 0;
}
