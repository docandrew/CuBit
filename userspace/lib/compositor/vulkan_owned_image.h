#ifndef CUBIT_VULKAN_OWNED_IMAGE_H
#define CUBIT_VULKAN_OWNED_IMAGE_H
#include <vulkan/vulkan.h>
#include <stdint.h>

/* Trusted foreign boundary, private non-exported images only. The caller owns
 * this zero-initialized record exclusively, for one lifetime. Vulkan 1.1 is
 * required. physical/device must be an authorized matching pair. No external
 * memory, scanout, host mapping or cross-process import authority is created.
 * The caller must retire all GPU work AND destroy views/descriptors before
 * release. A successful release means Vulkan API destruction, not proof of
 * physical page reclamation. No reset/reuse of a quarantined record. */
struct cubit_vulkan_owned_image {
    VkPhysicalDevice physical;
    VkDevice device;
    PFN_vkGetInstanceProcAddr instance_proc;
    VkInstance instance;
    PFN_vkGetDeviceProcAddr proc;
    uint32_t width, height;
    VkFormat format;
    VkImageUsageFlags usage;
    VkImage image;
    VkDeviceMemory memory;
    VkMemoryRequirements requirements;
    PFN_vkDestroyImage destroy;
    PFN_vkFreeMemory free_memory;
    PFN_vkAllocateMemory allocate;
    PFN_vkBindImageMemory bind;
    uint32_t stage; /* 0 fresh, 1 prepared, 2 live, 3 closed, 4 uncertain */
};
/* Result 0 success, 1 clean rejection, 2 uncertain (retain accounting).
 * prepare creates metadata only and returns actual allocation bytes, never a
 * width*height estimate. bind requires those bytes already charged by SPARK. */
uint32_t cubit_vulkan_owned_image_prepare(void *, uint64_t *, uint32_t *);
uint32_t cubit_vulkan_owned_image_bind(void *, uint64_t, uint32_t);
uint32_t cubit_vulkan_owned_image_release(void *);
#endif
