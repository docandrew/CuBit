#pragma once
#include "vk_sync.h"
#include "vk_sync_binary.h"
#include "native_gpu_timeline.h"
/* The GPU timeline: a process-local reached value plus points the session
 * queue signals later. Not an imported fence or sync file. The native
 * factory must validate libc wait support before advertising this. */
extern const struct vk_sync_type anv_cubit_gpu_timeline_type;
VkResult anv_cubit_sync_prepare(void);
struct anv_physical_device;
/* One-time publication into physical-device-owned storage. */
VkResult anv_cubit_init_sync_types(struct anv_physical_device *device);
/* Store the returned type for the physical device's entire lifetime. */
struct vk_sync_binary_type anv_cubit_binary_sync_type(void);
/* For the queue (anv_cubit_memory.c). How a wait reaches a descriptor:
 * reached, a point on slot's queue (*context, *gpu), or not submitted.
 * A timeline or Mesa binary of this type; a point from another queue is
 * refused. Resolves first; no IPC, no wait. */
VkResult anv_cubit_sync_find(struct vk_device *device, const struct vk_sync_wait *wait,
   uint64_t slot, enum native_gpu_wait_kind *kind, uint32_t *context, uint64_t *gpu);
/* After slot's queue took a job that signals context's value gpu: the
 * signal resolves then. A failure marks the device lost. No IPC, no wait. */
VkResult anv_cubit_sync_add_point(struct vk_device *device, const struct vk_sync_signal *signal,
   uint64_t slot, uint32_t context, uint64_t gpu);
