#include "service-device.h"
#include "device-bootstrap.h"
#include "launch-session.h"
#include "anv_entrypoints.h"

struct cubit_mesa_service {
   struct cubit_mesa_launch_session launch;
   struct anv_memory_budget budget;
   struct cubit_mesa_device owned;
   VkInstance instance;
   VkPhysicalDevice physical;
   bool ready, closing, lost;
};
/* One owner/GPU per process prevents accidentally splitting per-GPU accounting.
 * General multi-GPU bootstrap requires an authenticated per-device inventory. */
static struct cubit_mesa_service service;

VkResult cubit_mesa_service_start(uint64_t slot, struct cubit_mesa_service **out)
{
   if (!out || *out || !cubit_mesa_launch_start(&service.launch, slot))
      return VK_ERROR_INITIALIZATION_FAILED;
   *out = &service; /* Caller observes retained ownership on every later failure. */
   enum cubit_gpu_memory_contract policy;
   if (!cubit_gpu_query_memory(cubit_gpu_native_query_call,
                              &service.launch.endpoint, &policy) ||
       policy != CUBIT_GPU_MEMORY_OWNED_WB_COHERENT)
      return VK_ERROR_INITIALIZATION_FAILED;
   const VkApplicationInfo app = {
      .sType=VK_STRUCTURE_TYPE_APPLICATION_INFO, .apiVersion=VK_API_VERSION_1_1,
   };
   const VkInstanceCreateInfo info = {
      .sType=VK_STRUCTURE_TYPE_INSTANCE_CREATE_INFO, .pApplicationInfo=&app,
   };
   VkInstance handle = VK_NULL_HANDLE;
   VkResult result = anv_CreateInstance(&info, NULL, &handle);
   if (result != VK_SUCCESS)
      return result;
   if (!handle)
      return VK_ERROR_INITIALIZATION_FAILED;
   service.instance = handle;
   ANV_FROM_HANDLE(anv_instance, instance, service.instance);
   const struct anv_cubit_provider provider =
      cubit_mesa_launch_provider(&service.launch, &service.budget);
   result = anv_cubit_install_discovery(instance, &provider, 1);
   if (result != VK_SUCCESS)
      return result;
   PFN_vkEnumeratePhysicalDevices enumerate = (PFN_vkEnumeratePhysicalDevices)
      anv_GetInstanceProcAddr(service.instance, "vkEnumeratePhysicalDevices");
   if (!enumerate)
      return VK_ERROR_INITIALIZATION_FAILED;
   uint32_t count = 0;
   result = enumerate(service.instance, &count, NULL);
   if (result != VK_SUCCESS || count != 1)
      return result == VK_SUCCESS ? VK_ERROR_INITIALIZATION_FAILED : result;
   result = enumerate(service.instance, &count, &service.physical);
   if (result != VK_SUCCESS || count != 1 || !service.physical)
      return result == VK_SUCCESS ? VK_ERROR_INITIALIZATION_FAILED : result;
   result = cubit_mesa_device_create(service.instance, service.physical,
      anv_GetInstanceProcAddr, 0, VK_QUEUE_GRAPHICS_BIT, &service.owned);
   service.ready = result == VK_SUCCESS;
   return result;
}

VkBool32 cubit_mesa_service_device(struct cubit_mesa_service *owner,
                                  struct cubit_mesa_service_device *out)
{
   if (!out)
      return VK_FALSE;
   *out = (struct cubit_mesa_service_device){0};
   if (owner != &service || !service.ready || service.closing || service.lost)
      return VK_FALSE;
   *out = (struct cubit_mesa_service_device){
      service.instance, service.physical, service.owned.device,
      service.owned.queue, service.owned.family, anv_GetInstanceProcAddr,
   };
   return VK_TRUE;
}

VkResult cubit_mesa_service_status(struct cubit_mesa_service *owner)
{
   if (owner != &service || !service.ready || service.closing)
      return VK_ERROR_INITIALIZATION_FAILED;
   if (service.lost)
      return VK_ERROR_DEVICE_LOST;
   ANV_FROM_HANDLE(anv_device, device, service.owned.device);
   if (anv_cubit_check_status(&device->vk) == VK_SUCCESS)
      return VK_SUCCESS;
   service.lost = true;
   return VK_ERROR_DEVICE_LOST;
}

enum cubit_mesa_service_retirement
cubit_mesa_service_close(struct cubit_mesa_service *owner)
{
   if (owner != &service || !service.launch.started)
      return CUBIT_MESA_SERVICE_UNSAFE;
   if (!service.closing) {
      service.closing = true;
      service.ready = false;
      cubit_mesa_device_destroy_retired(&service.owned);
      if (service.instance) {
         anv_DestroyInstance(service.instance, NULL);
         service.instance = VK_NULL_HANDLE;
      }
      service.physical = VK_NULL_HANDLE;
   }
   switch (cubit_mesa_launch_finish(&service.launch)) {
   case CUBIT_MESA_RETIRED: return CUBIT_MESA_SERVICE_RETIRED;
   case CUBIT_MESA_RETIRE_PENDING: return CUBIT_MESA_SERVICE_PENDING;
   default: return CUBIT_MESA_SERVICE_UNSAFE;
   }
}
