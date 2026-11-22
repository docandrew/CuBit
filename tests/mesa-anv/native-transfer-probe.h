/* Optional native Vulkan queue test. Uses Mesa entrypoints, never raw batches. */
#include <vulkan/vulkan.h>
#include <stdint.h>
#include <string.h>
#include <unistd.h>

static VkResult
mesa_transfer_probe(VkInstance instance, VkPhysicalDevice physical, VkDevice device,
                    PFN_vkGetInstanceProcAddr instance_proc,
                    void (*log_message)(const char *, ...))
{
   PFN_vkGetDeviceProcAddr device_proc = (PFN_vkGetDeviceProcAddr)
      instance_proc(instance, "vkGetDeviceProcAddr");
   PFN_vkGetPhysicalDeviceMemoryProperties memory_properties =
      (PFN_vkGetPhysicalDeviceMemoryProperties)
      instance_proc(instance, "vkGetPhysicalDeviceMemoryProperties");
   if (!device_proc || !memory_properties)
      return VK_ERROR_INITIALIZATION_FAILED;
#define LOAD(name) PFN_vk##name name = (PFN_vk##name)device_proc(device, "vk" #name)
   LOAD(CreateBuffer); LOAD(DestroyBuffer); LOAD(GetBufferMemoryRequirements);
   LOAD(AllocateMemory); LOAD(FreeMemory); LOAD(BindBufferMemory);
   LOAD(MapMemory); LOAD(UnmapMemory);
   LOAD(CreateCommandPool); LOAD(DestroyCommandPool); LOAD(AllocateCommandBuffers);
   LOAD(BeginCommandBuffer); LOAD(EndCommandBuffer);
   LOAD(CmdFillBuffer); LOAD(CmdPipelineBarrier);
   LOAD(CreateFence); LOAD(DestroyFence); LOAD(GetDeviceQueue);
   LOAD(QueueSubmit); LOAD(WaitForFences); LOAD(DeviceWaitIdle);
#undef LOAD
   if (!CreateBuffer || !DestroyBuffer || !GetBufferMemoryRequirements ||
       !AllocateMemory || !FreeMemory || !BindBufferMemory || !MapMemory ||
       !UnmapMemory || !CreateCommandPool || !DestroyCommandPool ||
       !AllocateCommandBuffers || !BeginCommandBuffer || !EndCommandBuffer ||
       !CmdFillBuffer || !CmdPipelineBarrier || !CreateFence || !DestroyFence ||
       !GetDeviceQueue || !QueueSubmit || !WaitForFences || !DeviceWaitIdle)
      return VK_ERROR_INITIALIZATION_FAILED;

   const VkDeviceSize bytes = 4096;
   const uint32_t pattern = 0x43554249;
   VkBuffer buffer = VK_NULL_HANDLE;
   VkDeviceMemory memory = VK_NULL_HANDLE;
   VkCommandPool pool = VK_NULL_HANDLE;
   VkCommandBuffer command = VK_NULL_HANDLE;
   VkFence fence = VK_NULL_HANDLE;
   VkQueue queue = VK_NULL_HANDLE;
   void *mapped = NULL;
   VkResult result;
   const VkBufferCreateInfo buffer_info = {
      .sType = VK_STRUCTURE_TYPE_BUFFER_CREATE_INFO,
      .size = bytes, .usage = VK_BUFFER_USAGE_TRANSFER_DST_BIT,
      .sharingMode = VK_SHARING_MODE_EXCLUSIVE,
   };
   result = CreateBuffer(device, &buffer_info, NULL, &buffer);
   if (result != VK_SUCCESS) goto cleanup;
   VkMemoryRequirements requirements;
   GetBufferMemoryRequirements(device, buffer, &requirements);
   VkPhysicalDeviceMemoryProperties properties;
   memory_properties(physical, &properties);
   uint32_t memory_type = UINT32_MAX;
   const VkMemoryPropertyFlags wanted =
      VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT;
   for (uint32_t i = 0; i < properties.memoryTypeCount && i < 32; ++i) {
      if ((requirements.memoryTypeBits & (1u << i)) &&
          (properties.memoryTypes[i].propertyFlags & wanted) == wanted) {
         memory_type = i;
         break;
      }
   }
   if (memory_type == UINT32_MAX || requirements.size < bytes) {
      result = VK_ERROR_FEATURE_NOT_PRESENT;
      goto cleanup;
   }
   const VkMemoryAllocateInfo allocation = {
      .sType = VK_STRUCTURE_TYPE_MEMORY_ALLOCATE_INFO,
      .allocationSize = requirements.size, .memoryTypeIndex = memory_type,
   };
   result = AllocateMemory(device, &allocation, NULL, &memory);
   if (result != VK_SUCCESS) goto cleanup;
   result = BindBufferMemory(device, buffer, memory, 0);
   if (result != VK_SUCCESS) goto cleanup;
   result = MapMemory(device, memory, 0, bytes, 0, &mapped);
   if (result != VK_SUCCESS) goto cleanup;
   memset(mapped, 0, (size_t)bytes);
   const VkCommandPoolCreateInfo pool_info = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_POOL_CREATE_INFO,
      .flags = VK_COMMAND_POOL_CREATE_TRANSIENT_BIT, .queueFamilyIndex = 0,
   };
   result = CreateCommandPool(device, &pool_info, NULL, &pool);
   if (result != VK_SUCCESS) goto cleanup;
   const VkCommandBufferAllocateInfo command_info = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_ALLOCATE_INFO,
      .commandPool = pool, .level = VK_COMMAND_BUFFER_LEVEL_PRIMARY,
      .commandBufferCount = 1,
   };
   result = AllocateCommandBuffers(device, &command_info, &command);
   if (result != VK_SUCCESS) goto cleanup;
   const VkCommandBufferBeginInfo begin = {
      .sType = VK_STRUCTURE_TYPE_COMMAND_BUFFER_BEGIN_INFO,
      .flags = VK_COMMAND_BUFFER_USAGE_ONE_TIME_SUBMIT_BIT,
   };
   result = BeginCommandBuffer(command, &begin);
   if (result != VK_SUCCESS) goto cleanup;
   CmdFillBuffer(command, buffer, 0, bytes, pattern);
   const VkBufferMemoryBarrier barrier = {
      .sType = VK_STRUCTURE_TYPE_BUFFER_MEMORY_BARRIER,
      .srcAccessMask = VK_ACCESS_TRANSFER_WRITE_BIT,
      .dstAccessMask = VK_ACCESS_HOST_READ_BIT,
      .srcQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
      .dstQueueFamilyIndex = VK_QUEUE_FAMILY_IGNORED,
      .buffer = buffer, .offset = 0, .size = bytes,
   };
   CmdPipelineBarrier(command, VK_PIPELINE_STAGE_TRANSFER_BIT,
                      VK_PIPELINE_STAGE_HOST_BIT, 0, 0, NULL, 1, &barrier, 0, NULL);
   result = EndCommandBuffer(command);
   if (result != VK_SUCCESS) goto cleanup;
   const VkFenceCreateInfo fence_info = {.sType = VK_STRUCTURE_TYPE_FENCE_CREATE_INFO};
   result = CreateFence(device, &fence_info, NULL, &fence);
   if (result != VK_SUCCESS) goto cleanup;
   GetDeviceQueue(device, 0, 0, &queue);
   if (!queue) {
      result = VK_ERROR_INITIALIZATION_FAILED;
      goto cleanup;
   }
   const VkSubmitInfo submit = {
      .sType = VK_STRUCTURE_TYPE_SUBMIT_INFO,
      .commandBufferCount = 1, .pCommandBuffers = &command,
   };
   log_message("MESA-TRANSFER submit beginning bytes=4096\n");
   result = QueueSubmit(queue, 1, &submit, fence);
   log_message("MESA-TRANSFER submit=%d\n", result);
   if (result != VK_SUCCESS) {
      /* Do not assume an unsuccessful submission proves no work escaped.
       * Preserve its error, but require idle/device-loss before destroying. */
      VkResult idle;
      do {
         idle = DeviceWaitIdle(device);
         if (idle != VK_SUCCESS && idle != VK_ERROR_DEVICE_LOST)
            usleep(100000);
      } while (idle != VK_SUCCESS && idle != VK_ERROR_DEVICE_LOST);
      goto cleanup;
   }
   result = WaitForFences(device, 1, &fence, VK_TRUE, 1000000000ull);
   if (result != VK_SUCCESS && result != VK_ERROR_DEVICE_LOST) {
      log_message("MESA-TRANSFER pending; all resources retained (no resubmit)\n");
      do {
         usleep(100000);
         result = WaitForFences(device, 1, &fence, VK_TRUE, 1000000000ull);
      } while (result != VK_SUCCESS && result != VK_ERROR_DEVICE_LOST);
   }
   log_message("MESA-TRANSFER fence=%d\n", result);
   if (result != VK_SUCCESS) goto cleanup;
   const volatile uint32_t *words = mapped;
   uint32_t matches = 0;
   for (uint32_t i = 0; i < bytes / sizeof(*words); ++i)
      matches += words[i] == pattern;
   log_message("MESA-TRANSFER readback matches=%u expected=1024\n", matches);
   if (matches != 1024) result = VK_ERROR_UNKNOWN;
cleanup:
   /* Vulkan permits teardown after device loss. CuBit's backend separately
    * retains backing until its native retirement protocol allows reclamation. */
   if (fence) DestroyFence(device, fence, NULL);
   if (pool) DestroyCommandPool(device, pool, NULL);
   if (mapped) UnmapMemory(device, memory);
   if (buffer) DestroyBuffer(device, buffer, NULL);
   if (memory) FreeMemory(device, memory, NULL);
   return result;
}
