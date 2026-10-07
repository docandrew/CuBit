#ifndef CUBIT_VULKAN_AFFINE_H
#define CUBIT_VULKAN_AFFINE_H
#include <vulkan/vulkan.h>
#include "compositor.h"
/* Fixed Vulkan resources, no pixel allocations or queue/scene policy.
 * Initialize ONLY fresh storage. Destroy ONLY after all referring command
 * buffers, submitted work and descriptors have retired. This layer cannot
 * determine quiescence. Failure during creation is cleaned before publication.
 * Caller owns device/render-pass/source views/descriptors/framebuffers/pixels.
 * Compatible pass: one BGRA8 UNORM color attachment, sample count 1, subpass 0.
 */
struct cubit_vulkan_affine_engine {
    VkDevice device;
    VkDescriptorSetLayout descriptors;
    VkPipelineLayout layout;
    VkPipeline pipeline[3];
    VkSampler sampler;
    PFN_vkDestroyDescriptorSetLayout destroy_descriptors;
    PFN_vkDestroyPipelineLayout destroy_layout;
    PFN_vkDestroyPipeline destroy_pipeline;
    PFN_vkDestroySampler destroy_sampler;
    PFN_vkCmdBindPipeline bind_pipeline;
    PFN_vkCmdBindDescriptorSets bind_descriptors;
    PFN_vkCmdSetViewport viewport;
    PFN_vkCmdSetScissor scissor;
    PFN_vkCmdPushConstants constants;
    PFN_vkCmdDraw draw;
};
VkResult cubit_vulkan_affine_create(struct cubit_vulkan_affine_engine *fresh,
    VkDevice device, PFN_vkGetDeviceProcAddr proc, VkRenderPass compatible_pass);
void cubit_vulkan_affine_destroy(struct cubit_vulkan_affine_engine *quiescent);
/* Caller holds exclusively recording command buffer INSIDE compatible pass.
 * Descriptor has one combined image sampler (engine.sampler, readonly image),
 * descriptor layout engine.descriptors. Source and target are authorized,
 * nonaliasing, with matching dimensions, coherent and in correct layouts.
 * BGRA over=1 uses premultiplied alpha, over=2 straight alpha, over=0 replace.
 * Masks require over=1 and are R8_UNORM coverage. All use
 * exact nearest/clamp sampling; source dimensions are in 1..65535.
 * Width/height equal active target/viewport dimensions.
 * No descriptor updates, retirement, submission, barriers, allocation or waits
 * occur during record. All referenced state must outlive actual GPU completion.
 */
struct cubit_vulkan_affine_draw {
    const struct cubit_vulkan_affine_engine *engine;
    VkCommandBuffer command;
    VkDescriptorSet source;
    uint32_t width, height;
};
struct cubit_vulkan_coefficients { int64_t u0, ux, uy, v0, vx, vy, ud, vd; };
/* 0=recorded only, 1=rejected before any recording. Not completion evidence. */
uint32_t cubit_vulkan_record_affine(void *borrowed,
    const struct cubit_mesa_affine *draw, const struct cubit_vulkan_coefficients *coefficients,
    uint32_t width, uint32_t height, uint32_t mask, uint32_t argb);
/* A texel window inside the immutable source descriptor. image dimensions
 * must describe that descriptor; the caller retains the entire backing image.
 * Empty/out-of-bounds windows reject before recording. Sampling clamps within
 * the window, including outward-rounded fractional-DPI destination edges. */
struct cubit_vulkan_source_region { uint32_t x,y,width,height,image_width,image_height; };
uint32_t cubit_vulkan_record_affine_region(void *borrowed,
    const struct cubit_mesa_affine *draw,const struct cubit_vulkan_coefficients *coefficients,
    uint32_t width,uint32_t height,uint32_t mask,uint32_t argb,
    const struct cubit_vulkan_source_region *region);
/* Logical preview placement, derived by the SPARK geometry planner.
 * left/top are relative to the logical preview, not the output. The bound
 * source is opaque BGRA and remains owned through completion. */
struct cubit_vulkan_preview { int32_t left,top; uint32_t width,height; };
uint32_t cubit_vulkan_record_preview(void *borrowed,
    const struct cubit_mesa_affine *draw,const struct cubit_vulkan_coefficients *coefficients,
    uint32_t width,uint32_t height,const struct cubit_vulkan_preview *placement);
#endif
