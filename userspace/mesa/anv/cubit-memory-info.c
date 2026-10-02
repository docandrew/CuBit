#include "anv_private.h"
#include "cubit-memory-info.h"

bool
cubit_mesa_memory_budget(const struct anv_physical_device *device,
   cubit_gpu_query_call call, void *endpoint,
   VkPhysicalDeviceMemoryBudgetPropertiesEXT *out)
{
   if (!out)
      return false;
   memset(out->heapBudget, 0, sizeof(out->heapBudget));
   memset(out->heapUsage, 0, sizeof(out->heapUsage));
   if (!device || device->memory.heap_count != 1 ||
       device->info.has_local_mem || device->memory.heaps[0].is_local_mem ||
       !device->memory.heaps_budget || !device->memory.heaps[0].size)
      return false;
   const uint64_t size = device->memory.heaps[0].size;
   const uint64_t used = MIN2(size,
      p_atomic_read(&device->memory.heaps_budget->used[0]));
   struct cubit_gpu_budget_snapshot observed;
   const bool valid = cubit_gpu_query_budget(call, endpoint, &observed) &&
      observed.total == size;
   const uint64_t available = valid && observed.unused_tickets ?
      MIN2(observed.available, size - used) : 0;
   /* Vulkan requires nonzero budgets for present heaps even when exhausted.
    * One byte is the smallest representable estimate, NOT a promise that an
    * allocation can succeed. Never resurrect stale availability on failure.
    * No MB rounding (which can produce zero) or overflowing used+free sum.
    * https://docs.vulkan.org/refpages/latest/refpages/source/VkPhysicalDeviceMemoryBudgetPropertiesEXT.html
    */
   out->heapUsage[0] = used;
   out->heapBudget[0] = MAX2(UINT64_C(1), used + available);
   return valid;
}

bool
cubit_mesa_refresh_memory_info(struct anv_physical_device *device,
                               cubit_gpu_query_call call, void *endpoint)
{
   if (!device)
      return false;
   /* A previous observation must not survive transport/ownership failure as
    * apparently allocatable memory. The factory serializes this update. */
   device->sys.available = 0;
   device->info.mem.sram.mappable.free = 0;
   struct cubit_gpu_budget_snapshot observed;
   if (device->info.has_local_mem ||
       !cubit_gpu_query_budget(call, endpoint, &observed) ||
       (device->sys.size && device->sys.size != observed.total) ||
       (device->info.mem.sram.mappable.size &&
        device->info.mem.sram.mappable.size != observed.total))
      return false;
   device->sys.size = observed.total;
   device->sys.available = observed.unused_tickets ? observed.available : 0;
   /* Common ANV init/update copies these into sys. Updating only sys would
    * be overwritten by offline defaults immediately after this callback. */
   device->info.mem.sram.mappable.size = observed.total;
   device->info.mem.sram.mappable.free = device->sys.available;
   /* Do not set memory.need_flush, heap/type counts, region identity or KMD:
    * those require independently validated platform/cache contracts. */
   return true;
}

bool
cubit_mesa_init_memory_types(struct anv_physical_device *device,
                             cubit_gpu_query_call call, void *endpoint)
{
   if (!device || device->memory.type_count || device->has_protected_contexts ||
       device->info.has_local_mem || device->memory.heap_count != 1 ||
       device->memory.heaps[0].is_local_mem ||
       device->memory.heaps[0].flags != VK_MEMORY_HEAP_DEVICE_LOCAL_BIT ||
       device->sys.region != &device->info.mem.sram.mem)
      return false;
   enum cubit_gpu_memory_contract policy;
   if (!cubit_gpu_query_memory(call, endpoint, &policy) ||
       policy != CUBIT_GPU_MEMORY_OWNED_WB_COHERENT ||
       !cubit_mesa_refresh_memory_info(device, call, endpoint) ||
       device->memory.heaps[0].size != device->sys.size)
      return false;
   /* UMA RAM is DEVICE_LOCAL without being a discrete VRAM region. Common
    * ANV owns heap setup and appends its dynamic-visible type afterwards.
    * Explicit-only memory cannot satisfy Vulkan's required coherent type. */
   device->memory.types[0] = (struct anv_memory_type) {
      .propertyFlags = VK_MEMORY_PROPERTY_DEVICE_LOCAL_BIT |
                       VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT |
                       VK_MEMORY_PROPERTY_HOST_COHERENT_BIT |
                       VK_MEMORY_PROPERTY_HOST_CACHED_BIT,
      .heapIndex = 0,
   };
   device->memory.need_flush = false;
   device->memory.type_count = 1;
   return true;
}
