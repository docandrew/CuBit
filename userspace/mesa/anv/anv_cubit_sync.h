#pragma once
#include "vk_sync.h"
#include "vk_sync_binary.h"
/* Process-local CPU timeline. Not an imported fence or GPU-pending event.
 * Native factory must validate libc wait support before advertising this. */
extern const struct vk_sync_type anv_cubit_cpu_timeline_type;
VkResult anv_cubit_sync_prepare(void);
struct anv_physical_device;
/* One-time publication into physical-device-owned storage. */
VkResult anv_cubit_init_sync_types(struct anv_physical_device *device);
/* Store the returned type for the physical device's entire lifetime. */
struct vk_sync_binary_type anv_cubit_binary_sync_type(void);
