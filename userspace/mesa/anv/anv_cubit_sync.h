#pragma once
#include "vk_sync.h"
#include "cubit_handles.h"
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
 * reached, a point on endpoint's queue (*context, *gpu), or not submitted.
 * A timeline or Mesa binary of this type; a point from another queue is
 * refused. Resolves first; no IPC, no wait. */
VkResult anv_cubit_sync_find(struct vk_device *device, const struct vk_sync_wait *wait,
   cubit_endpoint endpoint, enum native_gpu_wait_kind *kind, uint32_t *context, uint64_t *gpu);
/* After endpoint's queue took a job that signals context's value gpu: the
 * signal resolves then. A failure marks the device lost. No IPC, no wait. */
VkResult anv_cubit_sync_add_point(struct vk_device *device, const struct vk_sync_signal *signal,
   cubit_endpoint endpoint, uint32_t context, uint64_t gpu);

/* Blocking-call evidence: every call this backend makes that can block its
 * caller's thread (device IPC or a wait), counted by call site with total
 * and longest duration. Desktop prints one line per second. */
enum anv_cubit_blocking_site {
   ANV_CUBIT_BLOCK_CREATE, ANV_CUBIT_BLOCK_CLOSE, ANV_CUBIT_BLOCK_MAP,
   ANV_CUBIT_BLOCK_UNMAP, ANV_CUBIT_BLOCK_VM_UPDATE, ANV_CUBIT_BLOCK_STATUS,
   ANV_CUBIT_BLOCK_PREPARE, ANV_CUBIT_BLOCK_GPU_WAIT, ANV_CUBIT_BLOCK_CPU_WAIT,
   ANV_CUBIT_BLOCK_SITES
};
/* Monotonic nanoseconds (0 when the clock fails: the sample is dropped). */
uint64_t anv_cubit_blocking_start(void);
void anv_cubit_blocking_note(enum anv_cubit_blocking_site site, uint64_t started);
/* "site=count/total_us/max_us ..." for sites used since the last report,
 * then reset; NUL-terminated within capacity. Returns the length. */
uint32_t cubit_mesa_blocking_report(char *buffer, uint32_t capacity);
