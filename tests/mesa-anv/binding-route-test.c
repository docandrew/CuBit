/* Real ANV adapter, mocked service. No GPU or Vulkan timeline execution. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>
#include "../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
/* The index a test endpoint was made from. */
#define TEST_INDEX(e) ((unsigned)((e).opaque & 0xffffffffu))

static unsigned binds, unbinds, updates;
static uint32_t epoch;
static uint64_t expected_gpu;
static bool registered, fail;
static bool expected_remove;
static uint64_t expected_slot = 63;
uint32_t cubit_intel_prepare_context(cubit_endpoint endpoint)
{ assert(endpoint.opaque == TEST_ENDPOINT(63).opaque); return 0; }
uint32_t cubit_intel_register_context(cubit_endpoint endpoint)
{ assert(endpoint.opaque == TEST_ENDPOINT(63).opaque); registered = true; return 0; }
uint32_t cubit_intel_bind_buffer(cubit_endpoint endpoint, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   assert(endpoint.opaque == TEST_ENDPOINT(expected_slot).opaque && handle == 17 && gpu == expected_gpu &&
          offset == 0 && bytes == 4096 && !registered);
   binds++; return 0;
}
uint32_t cubit_intel_unbind_buffer(cubit_endpoint endpoint, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   assert(endpoint.opaque == TEST_ENDPOINT(expected_slot).opaque && handle == 17 && gpu == expected_gpu &&
          offset == 0 && bytes == 4096 && !registered);
   unbinds++; return fail ? 4 : 0;
}
uint32_t cubit_intel_update_binding(cubit_endpoint endpoint, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t remove,
   uint32_t previous, uint32_t *generation)
{
   assert(endpoint.opaque == TEST_ENDPOINT(63).opaque && handle == 17 && gpu == expected_gpu &&
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
   /* Device-address-visible BOs use the same checked binding route. This
    * fixture checks native routing, not common ANV's VA allocator. */
   struct anv_bo bo = { .gem_handle = 17, .actual_size = 4096,
      .alloc_flags = ANV_BO_ALLOC_CLIENT_VISIBLE_ADDRESS };
   const uint64_t addresses[] = {4096, UINT64_C(1) << 47,
                                (UINT64_C(1) << 48) - 4096};
   assert(anv_cubit_memory_init(&device, TEST_ENDPOINT(63)) == VK_SUCCESS);
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
   assert(anv_cubit_memory_init(&offline_failed, TEST_ENDPOINT(62)) == VK_SUCCESS);
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
