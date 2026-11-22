/* Linux-hosted Vulkan device for the GPU canvas tests: the system loader
 * and whatever ICD VK_DRIVER_FILES names (run.sh picks Mesa lavapipe, a CPU
 * rasterizer). Never a CuBit or Intel hardware result. */
#include "gpu_canvas_vk.h"
#include <stddef.h>

#define HOST_DEVICE_LIMIT 8u
#define HOST_FAMILY_LIMIT 16u

static VkInstance instance;
static VkDevice device;

uint32_t gpu_canvas_host_open(struct gpu_canvas_device *out)
{
   const VkApplicationInfo application = {
      .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
      .pApplicationName = "cubit-gpu-canvas-host",
      .apiVersion = VK_API_VERSION_1_1,
   };
   const VkInstanceCreateInfo create = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
      .pApplicationInfo = &application,
   };
   if (vkCreateInstance(&create, NULL, &instance) != VK_SUCCESS)
      return 1;
   VkPhysicalDevice physical[HOST_DEVICE_LIMIT];
   uint32_t count = HOST_DEVICE_LIMIT;
   if (vkEnumeratePhysicalDevices(instance, &count, physical) < 0 || count == 0)
      return 2;
   VkQueueFamilyProperties families[HOST_FAMILY_LIMIT];
   uint32_t family_count = HOST_FAMILY_LIMIT;
   vkGetPhysicalDeviceQueueFamilyProperties(physical[0], &family_count, families);
   uint32_t family = UINT32_MAX;
   for (uint32_t n = 0; n < family_count; n++)
      if (families[n].queueFlags & VK_QUEUE_GRAPHICS_BIT) { family = n; break; }
   if (family == UINT32_MAX)
      return 3;
   const float priority = 1.0f;
   const VkDeviceQueueCreateInfo queue = {
      .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      .queueFamilyIndex = family, .queueCount = 1, .pQueuePriorities = &priority,
   };
   /* Like the native bridge: no optional features or extensions. */
   const VkDeviceCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      .queueCreateInfoCount = 1, .pQueueCreateInfos = &queue,
   };
   if (vkCreateDevice(physical[0], &info, NULL, &device) != VK_SUCCESS)
      return 4;
   VkQueue handle;
   vkGetDeviceQueue(device, family, 0, &handle);
   *out = (struct gpu_canvas_device){
      .instance = instance, .physical = physical[0], .device = device,
      .queue = handle, .family = family, .instance_proc = vkGetInstanceProcAddr,
   };
   return 0;
}

void gpu_canvas_host_close(void)
{
   if (device) { vkDeviceWaitIdle(device); vkDestroyDevice(device, NULL); device = NULL; }
   if (instance) { vkDestroyInstance(instance, NULL); instance = NULL; }
}
