/* Actual Mesa physical-device layout, CuBit provider and host pthreads. */
#include "anv_private.h"
#include "vk_sync.c"
#include "vk_sync_binary.c"
#include "util/os_time.c"
#include "c11/impl/time.c"
#include "../../userspace/mesa/anv/anv_cubit_sync.c"
#include "../../userspace/mesa/anv/anv_cubit_sync_types.c"
#include <assert.h>
#include <stdio.h>
int64_t debug_get_num_option(const char *name, int64_t fallback)
{ (void)name; return fallback; }
/* Logging is outside this fixture; preserve the real dispatch error result. */
VkResult __vk_errorf(const void *object, VkResult error, const char *file,
                    int line, const char *format, ...)
{
   (void)object; (void)file; (void)line; (void)format;
   return error;
}
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}
static struct anv_physical_device first, second;
static void *allocate(void *data, size_t size, size_t alignment,
                      VkSystemAllocationScope scope)
{ (void)data; (void)scope; return aligned_alloc(alignment, (size + alignment - 1) & ~(alignment - 1)); }
static void release(void *data, void *memory) { (void)data; free(memory); }
static void *fail_allocate(void *data, size_t size, size_t alignment,
                           VkSystemAllocationScope scope)
{ (void)data; (void)size; (void)alignment; (void)scope; return NULL; }
int main(void)
{
   assert(anv_cubit_init_sync_types(&first) == VK_SUCCESS);
   assert(anv_cubit_init_sync_types(&second) == VK_SUCCESS);
   assert(first.vk.supported_sync_types == first.sync_types);
   assert(first.sync_types[0] == &anv_cubit_cpu_timeline_type);
   assert(first.sync_types[1] == &first.cubit_binary_sync_type.sync);
   assert(first.sync_types[2] == NULL);
   assert(first.sync_types[1] != second.sync_types[1]);
   assert(anv_cubit_init_sync_types(&first) == VK_ERROR_INITIALIZATION_FAILED);
   assert(first.sync_types[1] == &first.cubit_binary_sync_type.sync);
   struct vk_device device = {.physical=&first.vk,
      .alloc={.pfnAllocation=allocate,.pfnFree=release}};
   for (unsigned i = 0; i < 2; i++) {
      const struct vk_sync_type *type = first.sync_types[i];
      struct vk_sync *sync = calloc(1, type->size);
      assert(sync);
      assert(!type->import_sync_file && !type->export_sync_file);
      assert(vk_sync_init(&device, sync, type, i ? 0 : VK_SYNC_IS_TIMELINE, 0) == VK_SUCCESS);
      assert(vk_sync_wait(&device, sync, i ? 0 : 1, 0, 0) == VK_TIMEOUT);
      assert(vk_sync_signal(&device, sync, i ? 0 : 1) == VK_SUCCESS);
      assert(vk_sync_wait(&device, sync, i ? 0 : 1, 0, 0) == VK_SUCCESS);
      vk_sync_finish(&device, sync); free(sync);
      sync = NULL;
      assert(anv_backend_sync_create(&device, i ? 0 : VK_SYNC_IS_TIMELINE, 0, &sync) == VK_SUCCESS);
      assert(sync && sync->type == type);
      vk_sync_destroy(&device, sync);
   }
   struct vk_sync *unchanged = (struct vk_sync *)(uintptr_t)1;
   assert(anv_backend_sync_create(&device, VK_SYNC_IS_SHAREABLE, 0, &unchanged) == VK_ERROR_FEATURE_NOT_PRESENT);
   assert(unchanged == (struct vk_sync *)(uintptr_t)1);
   device.alloc.pfnAllocation = fail_allocate;
   assert(anv_backend_sync_create(&device, 0, 0, &unchanged) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(unchanged == (struct vk_sync *)(uintptr_t)1);
   device.alloc.pfnAllocation = allocate;
   /* Providers without both CPU and queue waits cannot back internal fences.
    * Skip one rather than selecting it merely for its binary/timeline bit. */
   struct vk_sync_type insufficient = *first.sync_types[0];
   insufficient.features &= ~VK_SYNC_FEATURE_GPU_WAIT;
   const struct vk_sync_type *candidates[] = {
      &insufficient, first.sync_types[0], first.sync_types[1], NULL,
   };
   struct vk_physical_device filtered = {.supported_sync_types = candidates};
   device.physical = &filtered;
   struct vk_sync *selected = NULL;
   assert(anv_backend_sync_create(&device, VK_SYNC_IS_TIMELINE, 7, &selected) == VK_SUCCESS);
   assert(selected->type == first.sync_types[0]);
   assert(vk_sync_wait(&device, selected, 7, 0, 0) == VK_SUCCESS);
   vk_sync_destroy(&device, selected);
   candidates[1] = NULL;
   assert(anv_backend_sync_create(&device, VK_SYNC_IS_TIMELINE, 0, &unchanged) == VK_ERROR_FEATURE_NOT_PRESENT);
   assert(unchanged == (struct vk_sync *)(uintptr_t)1);
   struct vk_physical_device empty = {0};
   device.physical = &empty;
   assert(anv_backend_sync_create(&device, 0, 0, &unchanged) == VK_ERROR_FEATURE_NOT_PRESENT);
   assert(unchanged == (struct vk_sync *)(uintptr_t)1);
   puts("Physical sync provider PASS: owned lifetime, repeat rejection, real timeline/binary dispatch");
}
