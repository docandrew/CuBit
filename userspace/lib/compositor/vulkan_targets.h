#ifndef CUBIT_VULKAN_TARGETS_H
#define CUBIT_VULKAN_TARGETS_H
#include "vulkan_submission.h"
#define CUBIT_VULKAN_TARGET_COUNT 3u
/* Metadata only. Each supplied image has a separate authorized writable lease;
 * different VkImage handles alone do not prove backing-memory nonaliasing.
 * Images are local BGRA8 UNORM, one mip/layer, sample count 1, color attachments.
 * Device, render pass and all image leases outlive this set. */
struct cubit_vulkan_targets {
    VkDevice device;
    PFN_vkDestroyImageView destroy_view;
    PFN_vkDestroyFramebuffer destroy_framebuffer;
    VkImageView views[CUBIT_VULKAN_TARGET_COUNT];
    VkFramebuffer framebuffers[CUBIT_VULKAN_TARGET_COUNT];
    VkClearValue clear;
    struct cubit_vulkan_scene scenes[CUBIT_VULKAN_TARGET_COUNT];
};
/* Request storage is private and immutable through release, including fresh. */
struct cubit_vulkan_target_request {
    struct cubit_vulkan_targets *fresh;
    VkDevice device;
    PFN_vkGetDeviceProcAddr proc;
    VkRenderPass pass;
    VkImage images[CUBIT_VULKAN_TARGET_COUNT];
    uint32_t width,height;
    VkClearValue clear;
};
/* 0 published all three; 1 rejected/known-clean rollback; 2 uncertain, retain
 * the set and ALL requested leases. Fresh storage/unique ownership required.
 * No pixel allocation, copy, queue, wait or physical presentation is performed. */
uint32_t cubit_vulkan_targets_create(void *description,void **a,void **b,void **c);
/* Only SPARK's quiescent GPU + empty display-role gate may invoke destruction. */
uint32_t cubit_vulkan_targets_release(void *description);
#endif
