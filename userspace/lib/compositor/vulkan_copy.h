#ifndef CUBIT_VULKAN_COPY_H
#define CUBIT_VULKAN_COPY_H
#include <stdint.h>
#include <vulkan/vulkan.h>
/* Private foreign boundary; never an IPC ABI or allocation authority.
 * Borrowed command buffer must be recording OUTSIDE a render pass, on a queue
 * supporting transfer. Images must be distinct, nonaliasing, fully bound 2D
 * BGRA8 UNORM, single-sample, mip 0/layer 0, with transfer SRC/DST usage.
 * Both images must be in GENERAL layout. Caller establishes prior-write/read
 * dependencies, queue ownership and subsequent use barriers. Reported sizes
 * to Ada must match these images. No mutable front/pending target may be used.
 * All borrowed objects and their backing storage stay alive through confirmed
 * queue completion, and presentation retirement if scanned out. This adapter
 * neither submits nor waits, allocates pixel storage, maps, imports or retires.
 */
struct cubit_vulkan_copy {
    PFN_vkCmdCopyImage record;
    VkCommandBuffer command;
    VkImage target, source;
};
struct cubit_vulkan_copy_plan {
    uint32_t target_x, target_y, source_x, source_y, width, height;
};
/* 0 = command recorded only; 1 = rejected before recording. A void Vulkan
 * recording call provides no success/completion status. Device loss/queue
 * failure must be handled by the caller without releasing uncertain images. */
uint32_t cubit_vulkan_record_copy(void *borrowed,
                                const struct cubit_vulkan_copy_plan *plan);
#endif
