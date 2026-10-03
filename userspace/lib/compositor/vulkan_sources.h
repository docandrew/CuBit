#ifndef CUBIT_VULKAN_SOURCES_H
#define CUBIT_VULKAN_SOURCES_H
#include "vulkan_affine.h"
#define CUBIT_VULKAN_SOURCE_CAPACITY 140u
struct cubit_vulkan_sources;
struct cubit_vulkan_source {
    /* Must be first: the stable draw address is the provider's release key. */
    struct cubit_vulkan_affine_draw draw;
    struct cubit_vulkan_sources *owner;
    VkImageView view;
    VkDescriptorSet descriptor;
};
/* Fixed metadata and descriptor capacity; no pixel allocation, copy or wait.
 * Caller provides fresh storage, and destroys only after Can_Destroy plus all
 * external users are quiescent. Engine/device/command outlive this provider. */
struct cubit_vulkan_sources {
    const struct cubit_vulkan_affine_engine *engine;
    VkCommandBuffer command;
    VkDescriptorPool pool;
    PFN_vkDestroyDescriptorPool destroy_pool;
    PFN_vkCreateImageView create_view;
    PFN_vkDestroyImageView destroy_view;
    PFN_vkUpdateDescriptorSets update;
    struct cubit_vulkan_source entries[CUBIT_VULKAN_SOURCE_CAPACITY];
};
/* A previously authorized, compatible, same-device sampled image. The owner
 * retains its pixels and memory lease until confirmed Release_Source. This is
 * NOT a capability importer. Images use one mip/layer, BGRA8 or R8 UNORM, with
 * source extents in 1..65535 and SHADER_READ_ONLY_OPTIMAL layout at drawing.
 * Source and output must not alias. No layout transitions occur here. */
struct cubit_vulkan_source_request {
    struct cubit_vulkan_sources *provider;
    uint32_t slot;
    VkImage image;
    VkFormat format;
    uint32_t output_width,output_height;
};
VkResult cubit_vulkan_sources_init(struct cubit_vulkan_sources *fresh,
    const struct cubit_vulkan_affine_engine *engine,VkCommandBuffer command,
    PFN_vkGetDeviceProcAddr proc);
/* Fails without destroying anything if an entry remains occupied. */
uint32_t cubit_vulkan_sources_destroy(struct cubit_vulkan_sources *quiescent);
/* Import: 0 success; 1 rejected with no published view; 2 uncertain.
 * Release: 0 quiescent view destroyed; nonzero uncertain (retain caller lease).
 * Call only through SPARK's idle/source-retention gate. */
uint32_t cubit_vulkan_source_import(void *description,void **draw);
uint32_t cubit_vulkan_source_release(void *draw);
#endif
