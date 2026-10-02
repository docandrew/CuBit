/* Actual Mesa sync dispatcher, CuBit adapter/timeline, host pthreads.
 * Only device IPC and the debug-option source are mocked. Not native GPU. */
#include "vk_sync.c"
#include "vk_sync_binary.c"
#include "util/os_time.c"
#include "c11/impl/time.c"
#include "../../userspace/mesa/anv/anv_cubit_sync.c"
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <stdio.h>
int64_t debug_get_num_option(const char *name, int64_t fallback)
{ (void)name; return fallback; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}
static struct anv_device device;
static struct vk_physical_device physical;
static struct cubit_sync dependency, output;
static struct anv_bo batch = {.gem_handle=17, .actual_size=4096};
static unsigned submits;
static bool fail_submit;
uint32_t cubit_intel_prepare_context(uint64_t slot) { assert(slot == 63); return 0; }
uint32_t cubit_intel_register_context(uint64_t slot) { assert(slot == 63); return 0; }
uint32_t cubit_intel_submit_batch(uint64_t slot, uint32_t handle, uint64_t gpu,
   uint64_t offset, uint64_t bytes, uint32_t previous, uint32_t *completion)
{
   assert(slot == 63 && handle == 17 && gpu == 0x20000 && offset == 0 && bytes == 4096);
   uint64_t value;
   assert(vk_sync_get_value(&device.vk, &dependency.base, &value) == VK_SUCCESS && value >= 1);
   assert(vk_sync_get_value(&device.vk, &output.base, &value) == VK_SUCCESS && value == 0);
   submits++;
   *completion = previous + 1;
   return fail_submit ? 4 : 0;
}
static void *worker(void *unused)
{
   (void)unused;
   struct vk_sync_wait wait = {.sync=&dependency.base, .wait_value=1};
   struct vk_sync_signal signal = {.sync=&output.base, .signal_value=1};
   VkResult result = anv_cubit_submit_bo_sync(&device, &batch, 0x20000, 0, 4096,
                         1, &wait, 1, &signal, UINT64_MAX);
   assert(result == (fail_submit ? VK_ERROR_DEVICE_LOST : VK_SUCCESS));
   return NULL;
}
int main(void)
{
   struct vk_sync_binary_type binary_type = anv_cubit_binary_sync_type();
   const struct vk_sync_type *types[] = {&anv_cubit_cpu_timeline_type, &binary_type.sync, NULL};
   physical.supported_sync_types = types;
   device.vk.physical = &physical;
   struct vk_sync *binary = calloc(1, binary_type.sync.size);
   struct vk_sync *moved = calloc(1, binary_type.sync.size);
   assert(binary && moved);
   assert(vk_sync_init(&device.vk, binary, &binary_type.sync, 0, 0) == VK_SUCCESS);
   assert(vk_sync_init(&device.vk, moved, &binary_type.sync, 0, 0) == VK_SUCCESS);
   assert(vk_sync_move(&device.vk, moved, binary) == VK_ERROR_UNKNOWN);
   assert(vk_sync_wait(&device.vk, binary, 0, 0, 0) == VK_TIMEOUT);
   for (unsigned cycle = 0; cycle < 4; cycle++) {
      assert(vk_sync_signal(&device.vk, binary, 0) == VK_SUCCESS);
      assert(vk_sync_wait(&device.vk, binary, 0, VK_SYNC_WAIT_PENDING, 0) == VK_SUCCESS);
      assert(vk_sync_move(&device.vk, moved, binary) == VK_SUCCESS);
      assert(vk_sync_wait(&device.vk, moved, 0, 0, 0) == VK_SUCCESS);
      assert(vk_sync_wait(&device.vk, binary, 0, 0, 0) == VK_TIMEOUT);
      assert(vk_sync_reset(&device.vk, moved) == VK_SUCCESS);
      assert(vk_sync_wait(&device.vk, moved, 0, 0, 0) == VK_TIMEOUT);
   }
   struct vk_sync_binary *edge = vk_sync_as_binary(binary);
   edge->next_point = UINT64_MAX - 1;
   assert(vk_sync_reset(&device.vk, binary) == VK_SUCCESS);
   assert(edge->next_point == UINT64_MAX);
   assert(vk_sync_reset(&device.vk, binary) == VK_ERROR_UNKNOWN);
   assert(edge->next_point == UINT64_MAX);
   assert(vk_sync_wait(&device.vk, binary, 0, 0, 0) == VK_TIMEOUT);
   assert(vk_sync_signal(&device.vk, binary, 0) == VK_SUCCESS);
   assert(vk_sync_move(&device.vk, moved, binary) == VK_ERROR_UNKNOWN);
   assert(edge->next_point == UINT64_MAX);
   vk_sync_finish(&device.vk, binary); vk_sync_finish(&device.vk, moved);
   free(binary); free(moved);
   assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
   assert(anv_cubit_prepare_submission(&device) == VK_SUCCESS);
   assert(vk_sync_init(&device.vk, &dependency.base, types[0], VK_SYNC_IS_TIMELINE, 0) == VK_SUCCESS);
   assert(vk_sync_init(&device.vk, &output.base, types[0], VK_SYNC_IS_TIMELINE, 0) == VK_SUCCESS);
   assert(vk_sync_wait(&device.vk, &dependency.base, 1, VK_SYNC_WAIT_PENDING, 0) == VK_TIMEOUT);
   pthread_t thread;
   assert(!pthread_create(&thread, NULL, worker, NULL));
   assert(vk_sync_wait(&device.vk, &output.base, 1, 0, 0) == VK_TIMEOUT);
   assert(vk_sync_signal(&device.vk, &dependency.base, 1) == VK_SUCCESS);
   assert(vk_sync_wait(&device.vk, &output.base, 1, 0, UINT64_MAX) == VK_SUCCESS);
   assert(!pthread_join(thread, NULL) && submits == 1);
   vk_sync_finish(&device.vk, &output.base);
   assert(vk_sync_init(&device.vk, &output.base, types[0], VK_SYNC_IS_TIMELINE, 0) == VK_SUCCESS);
   fail_submit = true;
   worker(NULL);
   assert(submits == 2 && output.value == 0);
   assert(vk_sync_wait(&device.vk, &output.base, 1, 0, UINT64_MAX) == VK_ERROR_DEVICE_LOST);
   vk_sync_finish(&device.vk, &dependency.base);
   vk_sync_finish(&device.vk, &output.base);
   puts("Mesa real sync dispatch + native adapter PASS: dependency, completion signal, failed submission; mocked IPC/host pthreads");
}
