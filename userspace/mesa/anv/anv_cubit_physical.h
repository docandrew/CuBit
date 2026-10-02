#pragma once
#include "anv_private.h"
#include "cubit-device-query.h"

/* Trusted bootstrap integration, not application-supplied Vulkan pNext data.
 * retain pins one authenticated discovery endpoint and serializes its IPC;
 * release drops that reference, NOT live/deferred logical-device endpoints.
 * open_device must obtain a fresh admitted session and attach it using
 * anv_cubit_attach_owned_session with a process-owned endpoint pin. Each
 * logical/deferred session owns an independent
 * endpoint reference. A physical-device release must not invalidate those.
 * Callbacks and context remain valid until the matching release.
 * budget is the shared process/GPU accounting record, identical across all
 * physical objects/instances for that GPU, retained with the provider. It is
 * zero-initialized once by its owner, NEVER reset on physical construction.
 * Common ANV atomically adds/removes logical device-memory usage there.
 */
struct anv_cubit_provider {
   void *context;
   bool (*retain)(void *context);
   void (*release)(void *context);
   cubit_gpu_query_call query;
   VkResult (*open_device)(void *context, struct anv_device *device);
   struct anv_memory_budget *budget;
};

/* Compose the native backend and common upstream ANV physical device. Output
 * unchanged on failure; every successful retain gets exactly one release.
 * Does not install instance enumeration or acquire authority by PID/name.
 * Explicit-only cache policy still rejects Vulkan device construction.
 */
VkResult anv_cubit_physical_device_create(struct anv_instance *instance,
   const struct anv_cubit_provider *provider, struct vk_physical_device **out);

/* Trusted bootstrap: copy and retain the authorized inventory once before
 * enumeration. Empty inventory is explicit; no ambient authority lookup.
 * Caller obeys Vulkan external synchronization for instance destruction. */
VkResult anv_cubit_install_discovery(struct anv_instance *instance,
   const struct anv_cubit_provider *providers, uint32_t count);
/* Common runtime holds physical_devices.mutex during enumeration. */
VkResult anv_cubit_enumerate_physical_devices(struct vk_instance *instance);
/* Called after common instance finish destroys physical devices. */
void anv_cubit_finish_discovery(struct anv_instance *instance);
