/* Mesa-facing physical-device callback; no Linux syncobj or external handles. */
#include "anv_private.h"
#include "anv_cubit_sync.h"

VkResult anv_cubit_init_sync_types(struct anv_physical_device *device)
{
   /* Called during exclusive construction, never while logical devices exist.
    * Do not replace an already published provider or invalidate live pointers. */
   if (device->vk.supported_sync_types != NULL)
      return VK_ERROR_INITIALIZATION_FAILED;
   VkResult result = anv_cubit_sync_prepare();
   if (result != VK_SUCCESS) return result;
   device->cubit_binary_sync_type = anv_cubit_binary_sync_type();
   device->sync_types[0] = &anv_cubit_cpu_timeline_type;
   device->sync_types[1] = &device->cubit_binary_sync_type.sync;
   device->sync_types[2] = NULL;
   device->vk.supported_sync_types = device->sync_types;
   return VK_SUCCESS;
}
