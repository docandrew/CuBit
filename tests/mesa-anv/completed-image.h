#ifndef CUBIT_MESA_COMPLETED_IMAGE_H
#define CUBIT_MESA_COMPLETED_IMAGE_H
#include <vulkan/vulkan.h>
typedef VkResult (*mesa_completed_pixels)(VkDevice, VkDeviceMemory, VkDeviceSize,
                                         uint32_t, uint32_t, uint32_t);
/* Same-device, synchronous borrowed source, NOT a GPU-import wire protocol.
 * The producer fence has completed. Source is BGRA8, single sample/mip/layer,
 * COLOR_ATTACHMENT | TRANSFER_SRC | SAMPLED, in TRANSFER_SRC_OPTIMAL.
 * The consumer must transition it to SHADER_READ_ONLY_OPTIMAL before sampling.
 * Queue family 0 owns it. Image, view, allocation and device remain alive until
 * return; the producer will not write or reuse them during this call.
 *
 * Every return (including error) certifies that ALL consumer GPU references,
 * descriptors and downstream presentation loans have retired. Uncertain
 * submission/retirement must retain and wait, NOT return a timeout. The
 * consumer must not destroy producer objects or recursively invoke a probe.
 * Readback memory is unmapped at entry; it is optional baseline evidence, not
 * a substitute for sampling the source image. A compositor forwards its OWN
 * completed readback to present, and returns only after that borrow retires.
 */
struct mesa_completed_image {
    VkInstance instance;
    VkPhysicalDevice physical;
    VkDevice device;
    PFN_vkGetInstanceProcAddr instance_proc;
    VkQueue queue;
    VkImage image;
    VkImageView view;
    uint32_t width, height;
    VkDeviceMemory readback;
    VkDeviceSize readback_bytes;
};
typedef VkResult (*mesa_completed_image_consumer)(
    const struct mesa_completed_image *, mesa_completed_pixels);
#endif
