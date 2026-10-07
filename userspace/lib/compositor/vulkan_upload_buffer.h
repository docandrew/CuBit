#ifndef CUBIT_VULKAN_UPLOAD_BUFFER_H
#define CUBIT_VULKAN_UPLOAD_BUFFER_H
#include <vulkan/vulkan.h>
#include <stdint.h>
#define CUBIT_VULKAN_UPLOAD_MAX_BYTES (16u*1024u*1024u)
/* Private, serialized record on an already-admitted matching device. No external
 * import/export. Charge actual requirements before Bind. A mapped producer may
 * write only Capacity bytes and only before submission/after known completion.
 * All command readers must retire before release. Unknown state is never reset. */
struct cubit_vulkan_upload_buffer {
    VkInstance instance; VkPhysicalDevice physical; VkDevice device;
    PFN_vkGetInstanceProcAddr instance_proc; PFN_vkGetDeviceProcAddr proc;
    VkBuffer buffer; VkDeviceMemory memory; void *mapped;
    VkMemoryRequirements requirements;
    PFN_vkDestroyBuffer destroy; PFN_vkFreeMemory free_memory;
    PFN_vkUnmapMemory unmap; PFN_vkAllocateMemory allocate;
    PFN_vkBindBufferMemory bind; PFN_vkMapMemory map;
    uint32_t capacity,types,stage;
    VkBufferUsageFlags usage;
};
/* 0 success, 1 confirmed clean rejection, 2 uncertain: retain dependencies. */
uint32_t cubit_vulkan_upload_prepare(void *,uint32_t,uint64_t *,uint32_t *);
/* Same private staging allocation lifecycle, opposite transfer direction.
 * Charge actual requirements before Bind. Mapping is NOT readable until the
 * exact GPU transfer completes with a transfer-write -> host-read barrier.
 * Keep backing held through every CPU reader; coherent memory is not a fence.
 * Bind/release are shared with uploads; owners enforce direction/lifetime. */
uint32_t cubit_vulkan_readback_prepare(void *,uint32_t,uint64_t *,uint32_t *);
uint32_t cubit_vulkan_upload_bind(void *,uint64_t,uint32_t,void **);
uint32_t cubit_vulkan_upload_release(void *);
#endif
