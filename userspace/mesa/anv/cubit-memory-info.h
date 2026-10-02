#pragma once
#include "cubit-device-query.h"
struct anv_physical_device;
struct VkPhysicalDeviceMemoryBudgetPropertiesEXT;
/* Runtime read-only snapshot: no mutation of device->sys or info.mem. The
 * caller retains the physical device, and its provider serializes endpoint
 * IPC. Usage is atomically sampled from shared process/GPU accounting.
 * Valid one-UMA-heap devices always get a nonzero, heap-bounded budget,
 * including query failure or exhausted tickets; false reports unavailable
 * observation, not an uninitialized output. sType/pNext are preserved.
 */
bool cubit_mesa_memory_budget(const struct anv_physical_device *device,
   cubit_gpu_query_call call, void *endpoint,
   struct VkPhysicalDeviceMemoryBudgetPropertiesEXT *out);
/* Serialized factory/refresh operation on the same pinned endpoint used for
 * discovery. No heap/type initialization or extension enablement here.
 * Updates sys and the canonical info.mem.sram.mappable fields consumed by
 * common ANV. Failure clears both availability fields; established capacity
 * is never silently changed.
 * No allocation or reservation is made by observing this shared pool. */
bool cubit_mesa_refresh_memory_info(struct anv_physical_device *device,
                                   cubit_gpu_query_call call, void *endpoint);
/* Backend init_memory_types helper, after common ANV establishes one UMA
 * heap/region. Requires policy2 on the same pinned endpoint and fresh budget;
 * does not install a backend, claim authority, or publish a physical device.
 * Failed construction leaves type_count unchanged (zero). */
bool cubit_mesa_init_memory_types(struct anv_physical_device *device,
                                 cubit_gpu_query_call call, void *endpoint);
