#ifndef CUBIT_VULKAN_CONTEXT_H
#define CUBIT_VULKAN_CONTEXT_H
#include "vulkan_submission.h"
#include "../../mesa/service-device.h"
/* Trusted object-creation boundary. All storage is private, zero-initialized,
 * never copied/reused, and externally serialized. The admitted Mesa owner
 * outlives this context and every child. This does not create a device or
 * acquire authority. Allocation inside Vulkan remains a foreign operation. */
struct cubit_vulkan_context {
    uint32_t attempted, live;
    VkDevice device;
    VkCommandPool pool;
    VkFence fence;
    VkRenderPass pass;
    struct cubit_vulkan_submission submission;
    PFN_vkDestroyCommandPool destroy_pool;
    PFN_vkDestroyFence destroy_fence;
    PFN_vkDestroyRenderPass destroy_pass;
};
struct cubit_vulkan_context_request {
    struct cubit_vulkan_context *fresh;
    const struct cubit_mesa_service_device *device;
};
/* 0 ready, 1 rejected with no retained children, 2 invalid/repeated ownership.
 * Output is cleared on failure. LOAD preserves undamaged target pixels;
 * callers establish COLOR_ATTACHMENT_OPTIMAL and initialize fresh targets.
 * No queue submission, wait, pixel allocation or presentation occurs here. */
uint32_t cubit_vulkan_context_create(void *description, void **submission);
/* Only the SPARK owner may call after command/source/target/display retirement.
 * No wait or implicit device teardown. Request/fresh identity remains retained
 * even after release; repeated create/release is rejected. */
uint32_t cubit_vulkan_context_release(void *description);
#endif
