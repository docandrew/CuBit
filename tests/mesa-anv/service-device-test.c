/* Real service bootstrap implementation, Mesa types, mocked Vulkan/IPC.
 * Each scenario runs in a fresh process: production owner never resets. */
#include "../../userspace/mesa/service-device.h"
#include "anv_cubit_physical.h"
#include "anv_cubit_memory.h"
#include "anv_entrypoints.h"
#include "native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static unsigned mode, instance_destroys, device_destroys, closes;
static bool installed, transferred, allow_retire;
static struct anv_instance instance;
static struct anv_device device;
static struct anv_cubit_provider provider;
static struct anv_cubit_endpoint_pin pin;
static unsigned health_queries;
static VkResult health = VK_SUCCESS;

VkResult anv_cubit_check_status(struct vk_device *d)
{
   assert(d == &device.vk);
   health_queries++;
   return health;
}

bool cubit_gpu_native_query_call(void *ctx, const struct cubit_gpu_query_message *r,
                                struct cubit_gpu_query_message *reply)
{ (void)ctx; (void)r; (void)reply; abort(); }
bool cubit_gpu_query_memory(cubit_gpu_query_call call, void *ctx,
                            enum cubit_gpu_memory_contract *policy)
{
   assert(call == cubit_gpu_native_query_call && ctx);
   *policy = mode == 9 ? CUBIT_GPU_MEMORY_OWNED_WB_EXPLICIT :
                         CUBIT_GPU_MEMORY_OWNED_WB_COHERENT;
   return mode != 1;
}
VkResult anv_CreateInstance(const VkInstanceCreateInfo *info,
   const VkAllocationCallbacks *alloc, VkInstance *out)
{
   (void)alloc;
   assert(info->pApplicationInfo->apiVersion == VK_API_VERSION_1_1);
   if (mode == 2) {
      *out = (VkInstance)(uintptr_t)1; /* Failed output must never be destroyed. */
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   }
   if (mode == 10) { *out = VK_NULL_HANDLE; return VK_SUCCESS; }
   *out = anv_instance_to_handle(&instance);
   return VK_SUCCESS;
}
void anv_DestroyInstance(VkInstance handle, const VkAllocationCallbacks *alloc)
{
   (void)alloc; assert(handle == anv_instance_to_handle(&instance));
   instance_destroys++;
   if (installed) provider.release(provider.context);
}
VkResult anv_cubit_install_discovery(struct anv_instance *i,
   const struct anv_cubit_provider *p, uint32_t count)
{
   assert(i == &instance && count == 1 && p->budget);
   if (mode == 3) return VK_ERROR_INITIALIZATION_FAILED;
   provider = *p;
   assert(provider.retain(provider.context));
   installed = true;
   return VK_SUCCESS;
}
static VkResult enumerate(VkInstance i, uint32_t *count, VkPhysicalDevice *out)
{
   assert(i == anv_instance_to_handle(&instance));
   if (out && mode == 20) return VK_ERROR_DEVICE_LOST;
   if (out && mode == 21) { *count = 0; return VK_SUCCESS; }
   if (out && mode == 22) return VK_INCOMPLETE;
   if (mode == 4) return VK_ERROR_INITIALIZATION_FAILED;
   *count = mode == 11 ? 0 : mode == 15 ? 2 : 1;
   if (out) *out = mode == 14 ? VK_NULL_HANDLE : (VkPhysicalDevice)(uintptr_t)1;
   return VK_SUCCESS;
}
static void properties(VkPhysicalDevice p, uint32_t *count, VkQueueFamilyProperties *out)
{
   assert(p); *count = 1;
   if (out) *out = (VkQueueFamilyProperties){.queueCount=1,
      .queueFlags=mode == 12 ? VK_QUEUE_TRANSFER_BIT : VK_QUEUE_GRAPHICS_BIT};
}
VkResult anv_cubit_attach_owned_session(struct anv_device *d, uint64_t slot,
                                       struct anv_cubit_endpoint_pin *incoming)
{
   assert(d == &device && slot == 7);
   pin = *incoming;
   *incoming = (struct anv_cubit_endpoint_pin){0};
   transferred = true;
   return mode == 6 ? VK_ERROR_DEVICE_LOST : VK_SUCCESS;
}
static VkResult create_device(VkPhysicalDevice p, const VkDeviceCreateInfo *info,
   const VkAllocationCallbacks *alloc, VkDevice *out)
{
   (void)alloc; assert(p && info->queueCreateInfoCount == 1);
   if (mode == 5 || mode == 8) return VK_ERROR_OUT_OF_DEVICE_MEMORY;
   VkResult result = provider.open_device(provider.context, &device);
   if (result == VK_SUCCESS) *out = anv_device_to_handle(&device);
   return result;
}
static void destroy_device(VkDevice d, const VkAllocationCallbacks *alloc)
{ (void)alloc; assert(d == anv_device_to_handle(&device)); device_destroys++; }
static void get_queue(VkDevice d, uint32_t family, uint32_t index, VkQueue *out)
{
   assert(d && !family && !index);
   *out = mode == 7 ? VK_NULL_HANDLE : (VkQueue)(uintptr_t)2;
}
PFN_vkVoidFunction anv_GetInstanceProcAddr(VkInstance i, const char *name)
{
   assert(i);
   if (mode == 19 && !strcmp(name, "vkEnumeratePhysicalDevices")) return NULL;
   if (mode == 13 && !strcmp(name, "vkCreateDevice")) return NULL;
#define PROC(n, f) if (!strcmp(name, n)) return (PFN_vkVoidFunction)f
   PROC("vkEnumeratePhysicalDevices", enumerate);
   PROC("vkGetPhysicalDeviceQueueFamilyProperties", properties);
   PROC("vkCreateDevice", create_device);
   PROC("vkDestroyDevice", destroy_device);
   PROC("vkGetDeviceQueue", get_queue);
#undef PROC
   return NULL;
}
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{ assert(slot == 7 && !transferred); closes++; *tag=1; return mode == 8 ? 3 : 0; }
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{ assert(slot == 7 && closes == 1); return allow_retire ? 0 : 4; }
uint32_t anv_cubit_memory_poll(void)
{
   assert(transferred);
   if (!allow_retire) return 1;
   if (pin.retired) { pin.retired(pin.context); pin = (struct anv_cubit_endpoint_pin){0}; }
   return 0;
}

int main(int argc, char **argv)
{
   assert(argc == 2); mode = (unsigned)atoi(argv[1]); assert(mode <= 22);
   instance.vk.base.type = VK_OBJECT_TYPE_INSTANCE;
   device.vk.base.type = VK_OBJECT_TYPE_DEVICE;
   struct cubit_mesa_service *owner = NULL, *other = NULL;
   assert(cubit_mesa_service_start(64, &owner) != VK_SUCCESS && !owner);
   VkResult result = cubit_mesa_service_start(7, &owner);
   assert((result == VK_SUCCESS) == (mode == 0 || (mode >= 16 && mode <= 18)));
   if (mode == 19 || mode == 21) assert(result == VK_ERROR_INITIALIZATION_FAILED);
   if (mode == 20) assert(result == VK_ERROR_DEVICE_LOST);
   if (mode == 22) assert(result == VK_INCOMPLETE);
   assert(owner); /* Accepted endpoint remains owned even when setup fails. */
   assert(cubit_mesa_service_start(7, &other) != VK_SUCCESS && !other);
   struct cubit_mesa_service_device facts;
   assert(cubit_mesa_service_device(owner, &facts) == (result == VK_SUCCESS));
   if (result == VK_SUCCESS) assert(facts.device && facts.queue && facts.instance_proc);
   else assert(!facts.device && !facts.queue && !facts.instance);
   assert(cubit_mesa_service_status(owner) ==
      (result == VK_SUCCESS ? VK_SUCCESS : VK_ERROR_INITIALIZATION_FAILED));
   assert(health_queries == (result == VK_SUCCESS ? 1u : 0u));
   if (mode == 16 || mode == 17) {
      health = mode == 16 ? VK_ERROR_DEVICE_LOST : VK_ERROR_UNKNOWN;
      assert(cubit_mesa_service_status(owner) == VK_ERROR_DEVICE_LOST);
      assert(!cubit_mesa_service_device(owner, &facts) && !facts.device);
      health = VK_SUCCESS; /* Later replies may not resurrect this device. */
      assert(cubit_mesa_service_status(owner) == VK_ERROR_DEVICE_LOST);
      assert(health_queries == 2);
   }
   assert(cubit_mesa_service_close(owner) ==
      (mode == 8 ? CUBIT_MESA_SERVICE_UNSAFE : CUBIT_MESA_SERVICE_PENDING));
   assert(!cubit_mesa_service_device(owner, &facts) && !facts.device);
   const unsigned health_before_close = health_queries;
   assert(cubit_mesa_service_status(owner) == VK_ERROR_INITIALIZATION_FAILED);
   assert(health_queries == health_before_close);
   /* Recovery polls may span many event-loop turns. Pending retirement must
    * not replay destruction, expose a borrowed device, or allow rebinding. */
   const unsigned instances_after_close = instance_destroys;
   const unsigned devices_after_close = device_destroys;
   const unsigned closes_after_close = closes;
   for (unsigned i = 0; i < 4; i++) {
      assert(cubit_mesa_service_close(owner) ==
         (mode == 8 ? CUBIT_MESA_SERVICE_UNSAFE : CUBIT_MESA_SERVICE_PENDING));
      assert(instance_destroys == instances_after_close);
      assert(device_destroys == devices_after_close && closes == closes_after_close);
      assert(!cubit_mesa_service_device(owner, &facts) && !facts.device);
      assert(cubit_mesa_service_start(7, &other) != VK_SUCCESS && !other);
   }
   allow_retire = true;
   for (unsigned i=0; i<4; i++)
      assert(cubit_mesa_service_close(owner) ==
         (mode == 8 ? CUBIT_MESA_SERVICE_UNSAFE : CUBIT_MESA_SERVICE_RETIRED));
   assert(instance_destroys == (mode == 1 || mode == 2 || mode == 9 || mode == 10 ? 0u : 1u));
   assert(device_destroys == (mode == 0 || mode == 7 ||
      (mode >= 16 && mode <= 18) ? 1u : 0u));
   assert(closes == (transferred ? 0u : 1u));
   assert(cubit_mesa_service_start(7, &other) != VK_SUCCESS && !other);
   printf("Service bootstrap scenario %u PASS: retained failure, borrowed facts, one-shot teardown\n", mode);
}
