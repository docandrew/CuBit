/* Native static-link integration probe; no Linux Vulkan loader or fake GPU. */
#include <stdio.h>
#include "anv_entrypoints.h"

int main(void)
{
   VkApplicationInfo app = {
      .sType = VK_STRUCTURE_TYPE_APPLICATION_INFO,
      .pApplicationName = "CuBit native Mesa instance probe",
      .apiVersion = VK_API_VERSION_1_0,
   };
   VkInstanceCreateInfo create = {
      .sType = VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO,
      .pApplicationInfo = &app,
   };
   VkInstance instance = VK_NULL_HANDLE;
   VkResult result = anv_CreateInstance(&create, NULL, &instance);
   printf("MESA-NATIVE-INSTANCE: create=%d\n", result);
   if (result != VK_SUCCESS)
      return 1;
   PFN_vkEnumeratePhysicalDevices enumerate = (PFN_vkEnumeratePhysicalDevices)
      anv_GetInstanceProcAddr(instance, "vkEnumeratePhysicalDevices");
   if (enumerate == NULL) {
      anv_DestroyInstance(instance, NULL);
      return 1;
   }
   uint32_t count = 0;
   result = enumerate(instance, &count, NULL);
   printf("MESA-NATIVE-INSTANCE: enumerate=%d devices=%u\n", result, count);
   anv_DestroyInstance(instance, NULL);
   /* Zero devices is not a successful hardware integration test. */
   return result == VK_SUCCESS && count > 0 ? 0 : 2;
}
