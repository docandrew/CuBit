/* Real hosted Vulkan creation with injected dispatch failures. No Intel or
 * native capability-admission claim. Run under the Mesa host Nix shell. */
#include "../../userspace/mesa/device-bootstrap.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

static unsigned creates, destroys;
static int fault;
static PFN_vkCreateDevice real_create;
static PFN_vkDestroyDevice real_destroy;
static PFN_vkGetPhysicalDeviceQueueFamilyProperties real_properties;

static VKAPI_ATTR VkResult VKAPI_CALL
create_device(VkPhysicalDevice physical, const VkDeviceCreateInfo *info,
              const VkAllocationCallbacks *alloc, VkDevice *device)
{
   creates++;
   if (fault == 5)
      return VK_ERROR_OUT_OF_DEVICE_MEMORY;
   return real_create(physical, info, alloc, device);
}

static VKAPI_ATTR void VKAPI_CALL
destroy_device(VkDevice device, const VkAllocationCallbacks *alloc)
{
   destroys++;
   real_destroy(device, alloc);
}

static VKAPI_ATTR void VKAPI_CALL
properties(VkPhysicalDevice physical, uint32_t *count,
           VkQueueFamilyProperties *families)
{
   real_properties(physical, count, families);
   if (fault == 2)
      *count = 0;
   if (families && *count) {
      if (fault == 3)
         families[0].queueCount = 0;
      if (fault == 4)
         families[0].queueFlags = VK_QUEUE_TRANSFER_BIT;
   }
}

static VKAPI_ATTR void VKAPI_CALL
no_queue(VkDevice device, uint32_t family, uint32_t index, VkQueue *queue)
{
   (void)device; (void)family; (void)index;
   *queue = VK_NULL_HANDLE;
}

static VKAPI_ATTR PFN_vkVoidFunction VKAPI_CALL
dispatch(VkInstance instance, const char *name)
{
   if (!strcmp(name, "vkGetDeviceQueue") && fault == 1)
      return NULL;
   if (!strcmp(name, "vkGetDeviceQueue") && fault == 6)
      return (PFN_vkVoidFunction)no_queue;
   if (!strcmp(name, "vkGetPhysicalDeviceQueueFamilyProperties"))
      return (PFN_vkVoidFunction)properties;
   if (!strcmp(name, "vkCreateDevice"))
      return (PFN_vkVoidFunction)create_device;
   if (!strcmp(name, "vkDestroyDevice"))
      return (PFN_vkVoidFunction)destroy_device;
   return vkGetInstanceProcAddr(instance, name);
}

int main(void)
{
   const VkApplicationInfo app = {
      .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
      .apiVersion = VK_API_VERSION_1_1,
   };
   const VkInstanceCreateInfo info = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO, .pApplicationInfo = &app,
   };
   VkInstance instance = VK_NULL_HANDLE;
   assert(vkCreateInstance(&info, NULL, &instance) == VK_SUCCESS);
   uint32_t count = 1;
   VkPhysicalDevice physical;
   assert(vkEnumeratePhysicalDevices(instance, &count, &physical) == VK_SUCCESS);
   assert(count == 1);
   real_create = (PFN_vkCreateDevice)vkGetInstanceProcAddr(instance, "vkCreateDevice");
   real_destroy = (PFN_vkDestroyDevice)vkGetInstanceProcAddr(instance, "vkDestroyDevice");
   real_properties = (PFN_vkGetPhysicalDeviceQueueFamilyProperties)
      vkGetInstanceProcAddr(instance, "vkGetPhysicalDeviceQueueFamilyProperties");
   assert(real_create && real_destroy && real_properties);
   struct cubit_mesa_device owned = {0};
   /* Invalid family and missing dispatch must never consume device admission. */
   assert(cubit_mesa_device_create(instance, physical, dispatch, UINT32_MAX,
      VK_QUEUE_GRAPHICS_BIT, &owned) == VK_ERROR_INITIALIZATION_FAILED);
   assert(creates == 0);
   for (fault = 1; fault <= 6; fault++) {
      const unsigned before = creates, retired = destroys;
      assert(cubit_mesa_device_create(instance, physical, dispatch, 0,
         VK_QUEUE_GRAPHICS_BIT, &owned) ==
         (fault == 5 ? VK_ERROR_OUT_OF_DEVICE_MEMORY : VK_ERROR_INITIALIZATION_FAILED));
      assert(!owned.device && !owned.queue && !owned.destroy);
      assert(creates == before + (fault >= 5));
      assert(destroys == retired + (fault == 6));
   }
   fault = 0;
   for (unsigned i = 0; i < 32; i++) {
      assert(cubit_mesa_device_create(instance, physical, dispatch, 0,
         VK_QUEUE_GRAPHICS_BIT, &owned) == VK_SUCCESS);
      assert(owned.device && owned.queue && owned.destroy && owned.family == 0);
      const VkDevice device = owned.device;
      const unsigned before = creates;
      assert(cubit_mesa_device_create(instance, physical, dispatch, 0,
         VK_QUEUE_GRAPHICS_BIT, &owned) == VK_ERROR_INITIALIZATION_FAILED);
      assert(creates == before && owned.device == device);
      /* No work submitted, so there are no GPU or consumer references. */
      cubit_mesa_device_destroy_retired(&owned);
      assert(!owned.device && !owned.queue && !owned.destroy);
      cubit_mesa_device_destroy_retired(&owned);
   }
   assert(creates == 34 && destroys == 33); /* One failed creation, no device. */
   vkDestroyInstance(instance, NULL);
   puts("Mesa device bootstrap PASS: 32 real lifetimes, 6 dispatch faults, no overwrite/double destroy");
   return 0;
}
