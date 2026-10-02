/* Real ANV adapter, mocked service. No GPU or Vulkan timeline execution. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>

static unsigned binds, unbinds, updates;
static uint32_t epoch;
static uint64_t expected_gpu;
static bool registered, fail;
static bool expected_remove;
static uint64_t expected_slot = 63;
uint32_t cubit_intel_prepare_context(uint64_t slot)
{ assert(slot == 63); return 0; }
uint32_t cubit_intel_register_context(uint64_t slot)
{ assert(slot == 63); registered = true; return 0; }
uint32_t cubit_intel_bind_buffer(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   assert(slot == expected_slot && handle == 17 && gpu == expected_gpu &&
          offset == 0 && bytes == 4096 && !registered);
   binds++; return 0;
}
uint32_t cubit_intel_unbind_buffer(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   assert(slot == expected_slot && handle == 17 && gpu == expected_gpu &&
          offset == 0 && bytes == 4096 && !registered);
   unbinds++; return fail ? 4 : 0;
}
uint32_t cubit_intel_update_binding(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t remove,
   uint32_t previous, uint32_t *generation)
{
   assert(slot == 63 && handle == 17 && gpu == expected_gpu &&
          offset == 0 && bytes == 4096 && remove == expected_remove &&
          registered && previous == epoch);
   updates++; *generation = fail ? 0 : ++epoch; return fail ? 4 : 0;
}
VkResult _vk_device_set_lost(struct vk_device *d, const char *file,
                            int line, const char *message, ...)
{
   (void)d; (void)file; (void)line; (void)message;
   return VK_ERROR_DEVICE_LOST;
}
int main(void)
{
   static struct anv_device device;
   struct anv_bo bo = { .gem_handle = 17, .actual_size = 4096 };
   const uint64_t addresses[] = {4096, UINT64_C(1) << 47,
                                (UINT64_C(1) << 48) - 4096};
   assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
   assert(binds == 0 && updates == 0);
   for (unsigned phase = 0; phase < 2; phase++) {
      for (unsigned i = 0; i < 3; i++) {
         expected_gpu = addresses[i];
         bo.offset = intel_canonical_address(expected_gpu);
         assert(anv_cubit_bind_bo(&device, &bo) == VK_SUCCESS);
         if (!phase) {
            assert(anv_cubit_unbind_bo(&device, &bo) == VK_SUCCESS);
            assert(anv_cubit_bind_bo(&device, &bo) == VK_SUCCESS);
         }
      }
      unsigned before = binds + unbinds + updates;
      bo.offset = UINT64_C(1) << 47; /* unextended sign bit */
      assert(anv_cubit_bind_bo(&device, &bo) == VK_ERROR_INITIALIZATION_FAILED);
      bo.offset = 0;
      assert(anv_cubit_bind_bo(&device, &bo) == VK_ERROR_INITIALIZATION_FAILED);
      bo.offset = 4097;
      assert(anv_cubit_bind_bo(&device, &bo) == VK_ERROR_INITIALIZATION_FAILED);
      assert(anv_cubit_bind_bo(&device, NULL) == VK_ERROR_INITIALIZATION_FAILED);
      bo.offset = 4096;
      struct anv_bo child = { .slab_parent = &bo, .offset = 4096, .actual_size = 4096 };
      assert(anv_cubit_bind_bo(&device, &child) == VK_ERROR_INITIALIZATION_FAILED);
      assert(anv_cubit_unbind_bo(&device, &child) == VK_ERROR_INITIALIZATION_FAILED);
      assert(binds + unbinds + updates == before);
      if (!phase) assert(anv_cubit_prepare_submission(&device) == VK_SUCCESS);
   }
   assert(binds == 6 && unbinds == 3 && updates == 3 && epoch == 3);
   expected_remove = true;
   for (unsigned i = 0; i < 3; i++) {
      expected_gpu = addresses[i];
      bo.offset = intel_canonical_address(expected_gpu);
      assert(anv_cubit_unbind_bo(&device, &bo) == VK_SUCCESS);
   }
   assert(updates == 6 && epoch == 6);
   fail = true; expected_gpu = 4096; bo.offset = 4096;
   assert(anv_cubit_unbind_bo(&device, &bo) == VK_ERROR_DEVICE_LOST);
   fail = false;
   assert(anv_cubit_unbind_bo(&device, &bo) == VK_ERROR_DEVICE_LOST);
   assert(anv_cubit_bind_bo(&device, &bo) == VK_ERROR_DEVICE_LOST);
   assert(updates == 7 && binds == 6 && unbinds == 3);
   /* An uncertain unpublished change is not retried or upgraded to a live
    * update, even if the transport subsequently recovers. */
   static struct anv_device offline_failed;
   expected_slot = 62; registered = false;
   assert(anv_cubit_memory_init(&offline_failed, 62) == VK_SUCCESS);
   assert(anv_cubit_bind_bo(&offline_failed, &bo) == VK_SUCCESS);
   fail = true;
   assert(anv_cubit_unbind_bo(&offline_failed, &bo) == VK_ERROR_DEVICE_LOST);
   fail = false;
   assert(anv_cubit_unbind_bo(&offline_failed, &bo) == VK_ERROR_DEVICE_LOST);
   assert(anv_cubit_bind_bo(&offline_failed, &bo) == VK_ERROR_DEVICE_LOST);
   assert(anv_cubit_prepare_submission(&offline_failed) == VK_ERROR_INITIALIZATION_FAILED);
   assert(unbinds == 4 && binds == 7 && updates == 7);
   puts("ANV binding route PASS: canonical VA, offline-to-live transition, invalid requests, sticky failure; mocked IPC");
   return 0;
}
