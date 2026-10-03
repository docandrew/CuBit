#ifndef CUBIT_VULKAN_CHECKER_H
#define CUBIT_VULKAN_CHECKER_H
#include <vulkan/vulkan.h>
/* Fixed descriptor-free pipeline. Fresh storage on create; destroy only after
 * every recording/submitted reference retires. No pixel allocations or waits.
 * Compatible pass: one BGRA8 UNORM attachment, one sample, subpass zero.
 */
struct cubit_vulkan_checker {
    VkDevice device;
    VkPipelineLayout layout;
    VkPipeline pipeline;
    PFN_vkDestroyPipelineLayout destroy_layout;
    PFN_vkDestroyPipeline destroy_pipeline;
    PFN_vkCmdBindPipeline bind;
    PFN_vkCmdSetViewport viewport;
    PFN_vkCmdSetScissor scissor;
    PFN_vkCmdPushConstants constants;
    PFN_vkCmdDraw draw;
};
/* POD borrowed request, no authority or owned handles. Area uses desktop
 * logical coordinates; clip uses output physical coordinates. */
#include "vulkan_checker_request.h"
VkResult cubit_vulkan_checker_create(struct cubit_vulkan_checker *fresh,
    VkDevice device, PFN_vkGetDeviceProcAddr proc, VkRenderPass pass);
void cubit_vulkan_checker_destroy(struct cubit_vulkan_checker *quiescent);
/* 0=recorded, 1=rejected before recording. Not completion evidence.
 * Command must be exclusively recording inside the compatible render pass;
 * request dimensions must equal its active target. No descriptor required.
 */
uint32_t cubit_vulkan_checker_record(const struct cubit_vulkan_checker *engine,
    VkCommandBuffer command, const struct cubit_vulkan_checker_request *request);
#endif
