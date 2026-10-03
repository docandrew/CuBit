/* Trusted in-process compositor integration, not a public IPC/driver ABI. */
#pragma once
#include <vulkan/vulkan.h>
#include <stdint.h>

struct cubit_mesa_service;
struct cubit_mesa_service_device {
   VkInstance instance;
   VkPhysicalDevice physical;
   VkDevice device;
   VkQueue queue;
   uint32_t family;
   PFN_vkGetInstanceProcAddr instance_proc;
};
enum cubit_mesa_service_retirement {
   CUBIT_MESA_SERVICE_RETIRED = 0,
   CUBIT_MESA_SERVICE_PENDING = 1,
   CUBIT_MESA_SERVICE_UNSAFE = 2,
};

/* Exactly one launch-supplied session per process, externally serialized.
 * out must point to NULL. If accepted, *out is set BEFORE any fallible GPU
 * query/setup. A failing VkResult with non-NULL *out still owns the endpoint:
 * call close/poll and retain its capability until confirmed retirement.
 * Failure with NULL *out never accepted ownership; the caller retains it.
 * No authority is acquired and no attempt is replayed, even after retirement.
 */
VkResult cubit_mesa_service_start(uint64_t slot, struct cubit_mesa_service **out);

/* Borrow handles only while ready, before close; no ownership transfer.
 * All consumers must retire before close, including compositor/display reads.
 * Output cleared on failure. No handle may be used after close begins. */
VkBool32 cubit_mesa_service_device(struct cubit_mesa_service *owner,
                                  struct cubit_mesa_service_device *out);

/* Read the existing native session-health path; no submission or WaitIdle.
 * Success is an observation, NOT a lease or proof of frame completion.
 * Any backend failure makes device loss sticky and disables future handle
 * borrows. Previously borrowed handles/work still require normal retirement.
 * Invalid/not-ready/closing owners return INITIALIZATION_FAILED without IPC.
 * As with other bridge calls, serialize externally; transport can block. */
VkResult cubit_mesa_service_status(struct cubit_mesa_service *owner);

/* Caller has already retired submitted work, child Vulkan objects and external
 * consumers. No WaitIdle, sleep, retry, or poll loop. Vulkan teardown and each
 * native IPC call can still block; this is not asynchronous IPC. Close destroys
 * each device/instance once, then performs one retirement pass. Repeat close
 * only polls; pending/unsafe never release or recycle the endpoint slot.
 * Context/budget storage is process-static and is never freed or reset. */
enum cubit_mesa_service_retirement
cubit_mesa_service_close(struct cubit_mesa_service *owner);
