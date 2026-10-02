/* Native physical-device composition. No DRM descriptors or Linux authority. */
#include "anv_cubit_physical.h"
#include "anv_cubit_memory.h"
#include "anv_cubit_sync.h"
#include "anv_measure.h"
#include "cubit-memory-info.h"

struct cubit_physical_state {
   struct anv_kmd_backend backend;
   struct anv_cubit_provider provider;
};

struct anv_cubit_discovery {
   uint32_t count;
   struct anv_cubit_provider providers[];
};

static bool valid_provider(const struct anv_cubit_provider *p)
{
   return p && p->retain && p->release && p->query && p->open_device && p->budget;
}

VkResult
anv_cubit_install_discovery(struct anv_instance *instance,
   const struct anv_cubit_provider *providers, uint32_t count)
{
   if (!instance || (count && !providers) ||
       count > (SIZE_MAX - sizeof(struct anv_cubit_discovery)) / sizeof(*providers))
      return VK_ERROR_INITIALIZATION_FAILED;
   for (uint32_t n = 0; n < count; n++)
      if (!valid_provider(&providers[n]))
         return VK_ERROR_INITIALIZATION_FAILED;
   struct anv_cubit_discovery *d = vk_zalloc(&instance->vk.alloc,
      sizeof(*d) + count * sizeof(*providers), 8, VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   if (!d)
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   for (; d->count < count; d->count++) {
      d->providers[d->count] = providers[d->count];
      if (!providers[d->count].retain(providers[d->count].context))
         goto fail;
   }
   mtx_lock(&instance->vk.physical_devices.mutex);
   bool available = !instance->cubit_discovery &&
                    !instance->vk.physical_devices.enumerated &&
                    list_is_empty(&instance->vk.physical_devices.list);
   if (available)
      instance->cubit_discovery = d;
   mtx_unlock(&instance->vk.physical_devices.mutex);
   if (available)
      return VK_SUCCESS;
fail:
   for (uint32_t n = 0; n < d->count; n++)
      d->providers[n].release(d->providers[n].context);
   vk_free(&instance->vk.alloc, d);
   return VK_ERROR_INITIALIZATION_FAILED;
}

VkResult
anv_cubit_enumerate_physical_devices(struct vk_instance *vk)
{
   struct anv_instance *instance = container_of(vk, struct anv_instance, vk);
   struct anv_cubit_discovery *d = instance->cubit_discovery;
   if (!d || !list_is_empty(&vk->physical_devices.list))
      return VK_ERROR_INITIALIZATION_FAILED;
   struct list_head pending;
   list_inithead(&pending);
   for (uint32_t n = 0; n < d->count; n++) {
      struct vk_physical_device *device;
      VkResult result = anv_cubit_physical_device_create(instance,
         &d->providers[n], &device);
      if (result != VK_SUCCESS) {
         list_for_each_entry_safe(struct vk_physical_device, old, &pending, link) {
            list_del(&old->link);
            anv_physical_device_destroy(old);
         }
         return result;
      }
      list_addtail(&device->link, &pending);
   }
   list_for_each_entry_safe(struct vk_physical_device, device, &pending, link) {
      list_del(&device->link);
      list_addtail(&device->link, &vk->physical_devices.list);
   }
   return VK_SUCCESS;
}

void anv_cubit_finish_discovery(struct anv_instance *instance)
{
   struct anv_cubit_discovery *d = instance->cubit_discovery;
   if (!d) return;
   instance->cubit_discovery = NULL;
   for (uint32_t n = 0; n < d->count; n++)
      d->providers[n].release(d->providers[n].context);
   vk_free(&instance->vk.alloc, d);
}

static struct cubit_physical_state *state(struct anv_physical_device *device)
{
   return container_of(device->kmd_backend, struct cubit_physical_state, backend);
}

static void finish(struct anv_physical_device *device)
{
   struct cubit_physical_state *s = state(device);
   device->memory.heaps_budget = NULL;
   device->kmd_backend = NULL;
   s->provider.release(s->provider.context);
   vk_free(&device->instance->vk.alloc, s);
}

static void get_budget(struct anv_physical_device *device,
                       VkPhysicalDeviceMemoryBudgetPropertiesEXT *out)
{
   struct cubit_physical_state *s = state(device);
   (void)cubit_mesa_memory_budget(device, s->provider.query,
                                  s->provider.context, out);
}

static VkResult parameters(struct anv_physical_device *device)
{
   /* The measured retained pool is the capacity, not host RAM/sysconf. No
    * Linux context-priority/VM-control ioctl capability is implied. */
   /* Keep EXT_global_priority unadvertised: native admission currently accepts
    * only the default queue request, not priority-selection pNext chains. */
   device->max_context_priority = VK_QUEUE_GLOBAL_PRIORITY_LOW;
   struct cubit_physical_state *s = state(device);
   return cubit_mesa_refresh_memory_info(device, s->provider.query,
      s->provider.context) ? VK_SUCCESS : VK_ERROR_INITIALIZATION_FAILED;
}

static uint64_t restrict_heap(struct anv_physical_device *device, uint64_t size)
{
   return MIN2(size, device->sys.size);
}

static VkResult memory_types(struct anv_physical_device *device)
{
   struct cubit_physical_state *s = state(device);
   return cubit_mesa_init_memory_types(device, s->provider.query,
                                       s->provider.context) ?
      VK_SUCCESS : VK_ERROR_INITIALIZATION_FAILED;
}

static VkResult engines(struct anv_physical_device *device)
{
   if (device->engine_info)
      return VK_ERROR_INITIALIZATION_FAILED;
   struct intel_query_engine_info *info =
      calloc(1, sizeof(*info) + sizeof(info->engines[0]));
   if (!info)
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   /* The current admitted service protocol implements only RCS0. Physical
    * fused media/compute units are not automatically exposed as queues. */
   info->num_engines = 1;
   info->engines[0].engine_class = INTEL_ENGINE_CLASS_RENDER;
   device->engine_info = info;
   return VK_SUCCESS;
}

static VkResult open_device(struct anv_device *device)
{
   struct cubit_physical_state *s = state(device->physical);
   return s->provider.open_device(s->provider.context, device);
}

VkResult
anv_cubit_physical_device_create(struct anv_instance *instance,
   const struct anv_cubit_provider *provider, struct vk_physical_device **out)
{
   if (!instance || !out || !valid_provider(provider))
      return VK_ERROR_INITIALIZATION_FAILED;

   struct cubit_physical_state *s = vk_zalloc(&instance->vk.alloc, sizeof(*s),
      8, VK_SYSTEM_ALLOCATION_SCOPE_INSTANCE);
   if (!s)
      return VK_ERROR_OUT_OF_HOST_MEMORY;
   s->provider = *provider;
   if (!s->provider.retain(s->provider.context)) {
      vk_free(&instance->vk.alloc, s);
      return VK_ERROR_INITIALIZATION_FAILED;
   }
   s->backend = anv_cubit_transport_backend;
   s->backend.finish_physical = finish;
   s->backend.get_physical_parameters = parameters;
   s->backend.init_memory_types = memory_types;
   s->backend.restrict_sys_heap_size = restrict_heap;
   /* Runtime queries must not mutate discovery fields shared by threads. */
   s->backend.refresh_memory_info = NULL;
   s->backend.get_memory_budget = get_budget;
   s->backend.init_sync_types = anv_cubit_init_sync_types;
   s->backend.init_engine_info = engines;
   s->backend.open_device = open_device;

   struct anv_physical_device *device;
   VkResult result = anv_physical_device_alloc(instance, &s->backend, &device);
   if (result != VK_SUCCESS)
      goto fail_state;
   device->local_fd = device->master_fd = -1;
   device->memory.heaps_budget = s->provider.budget;
   if (!cubit_mesa_query_runtime_device(s->provider.query, s->provider.context,
                                        &device->info)) {
      result = VK_ERROR_INITIALIZATION_FAILED;
      goto fail_device;
   }
   device->info.kmd_type = INTEL_KMD_TYPE_CUBIT;
   memset(device->info.engine_class_supported_count, 0,
          sizeof(device->info.engine_class_supported_count));
   device->info.engine_class_supported_count[INTEL_ENGINE_CLASS_RENDER] = 1;
   result = anv_physical_device_init_common(device);
   if (result != VK_SUCCESS)
      goto fail_common;
   result = engines(device);
   if (result != VK_SUCCESS)
      goto fail_common;

   /* Do not let ANV_QUEUE_OVERRIDE manufacture unsupported native engines,
    * priorities or multiple queues. Expand this with the service protocol. */
   device->queue.family_count = 1;
   device->queue.families[0] = (struct anv_queue_family) {
      .queueFlags = VK_QUEUE_GRAPHICS_BIT | VK_QUEUE_COMPUTE_BIT |
                    VK_QUEUE_TRANSFER_BIT,
      .queueCount = 1,
      .engine_class = INTEL_ENGINE_CLASS_RENDER,
   };
   anv_shader_init_uuid(device);
   anv_physical_device_init_properties(device);
   result = anv_init_wsi(device);
   if (result != VK_SUCCESS)
      goto fail_common;
   anv_measure_device_init(device);
   anv_genX(&device->info, init_physical_device_state)(device);
   anv_genX(&device->info, init_instructions)(device);
   *out = &device->vk;
   return VK_SUCCESS;

fail_common:
   anv_physical_device_finish_common(device);
fail_device:
   anv_physical_device_free(device);
fail_state:
   s->provider.release(s->provider.context);
   vk_free(&instance->vk.alloc, s);
   return result;
}
