/* Vulkan device/queue bootstrap shared by trusted CuBit render clients.
 * The physical device must already come from authorized native discovery.
 * This helper neither discovers hardware nor acquires or retries authority.
 * Calls and the returned queue require Vulkan external synchronization.
 */
#pragma once
#include <vulkan/vulkan.h>
#include <stdint.h>
#include <stdlib.h>

struct cubit_mesa_device {
   VkDevice device;
   VkQueue queue;
   uint32_t family;
   PFN_vkDestroyDevice destroy;
};

/* Output must be empty and is unchanged on failure. Queue 0 of the selected
 * family is created with no optional features/extensions. Consumers that need
 * additional features must negotiate them explicitly, not assume this enables
 * them. A CreateDevice failure is terminal for a one-shot admitted session.
 */
static inline VkResult
cubit_mesa_device_create(VkInstance instance, VkPhysicalDevice physical,
                         PFN_vkGetInstanceProcAddr get_proc, uint32_t family,
                         VkQueueFlags required, struct cubit_mesa_device *out)
{
   if (!instance || !physical || !get_proc || !out || out->device || out->queue ||
       out->destroy || !required)
      return VK_ERROR_INITIALIZATION_FAILED;
   PFN_vkGetPhysicalDeviceQueueFamilyProperties properties =
      (PFN_vkGetPhysicalDeviceQueueFamilyProperties)
      get_proc(instance, "vkGetPhysicalDeviceQueueFamilyProperties");
   PFN_vkCreateDevice create = (PFN_vkCreateDevice)
      get_proc(instance, "vkCreateDevice");
   PFN_vkDestroyDevice destroy = (PFN_vkDestroyDevice)
      get_proc(instance, "vkDestroyDevice");
   PFN_vkGetDeviceQueue get_queue = (PFN_vkGetDeviceQueue)
      get_proc(instance, "vkGetDeviceQueue");
   if (!properties || !create || !destroy || !get_queue)
      return VK_ERROR_INITIALIZATION_FAILED;
   uint32_t count = 0;
   properties(physical, &count, NULL);
   if (family >= count)
      return VK_ERROR_INITIALIZATION_FAILED;
   /* calloc checks multiplication overflow and returns no partial array. */
   VkQueueFamilyProperties *families = calloc(count, sizeof(*families));
   if (!families)
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   uint32_t capacity = count;
   properties(physical, &count, families);
   const VkBool32 valid = count <= capacity && family < count &&
      families[family].queueCount != 0 &&
      (families[family].queueFlags & required) == required;
   free(families);
   if (!valid)
      return VK_ERROR_INITIALIZATION_FAILED;
   const float priority = 1.0f;
   const VkDeviceQueueCreateInfo queue_info = {
      .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      .queueFamilyIndex = family, .queueCount = 1,
      .pQueuePriorities = &priority,
   };
   const VkDeviceCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      .queueCreateInfoCount = 1, .pQueueCreateInfos = &queue_info,
   };
   VkDevice device = VK_NULL_HANDLE;
   VkResult result = create(physical, &info, NULL, &device);
   if (result != VK_SUCCESS)
      return result;
   if (!device)
      return VK_ERROR_INITIALIZATION_FAILED;
   VkQueue queue = VK_NULL_HANDLE;
   get_queue(device, family, 0, &queue);
   if (!queue) {
      /* No queue or work has escaped this helper. Native backend retirement
       * still owns any deferred setup resources; DestroyDevice is not proof
       * that the admitted endpoint can be reused. */
      destroy(device, NULL);
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   *out = (struct cubit_mesa_device){device, queue, family, destroy};
   return VK_SUCCESS;
}

/* Caller has completed all GPU work and retired every consumer/borrow first.
 * This does not wait, cancel, recycle a capability, or certify native endpoint
 * retirement. Those remain the device owner's explicit responsibilities.
 */
static inline void
cubit_mesa_device_destroy_retired(struct cubit_mesa_device *owned)
{
   if (owned && owned->device && owned->destroy) {
      owned->destroy(owned->device, NULL);
      *owned = (struct cubit_mesa_device){0};
   }
}
