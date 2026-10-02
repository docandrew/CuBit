/* Actual adapter/Mesa types; mock synchronization and native IPC, NOT GPU. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>

static unsigned waits_seen, submits_seen, signals_seen, step;
static VkResult wait_result = VK_SUCCESS, signal_result = VK_SUCCESS;
static uint32_t submit_result;
static uint32_t markers[64];
static bool lose_during_wait;
static struct anv_bo bo = { .gem_handle = 17, .actual_size = 8192 };
static struct vk_sync_wait waits[2];
static struct vk_sync_signal signals[2];

VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{ (void)device; (void)file; (void)line; (void)message; return VK_ERROR_DEVICE_LOST; }
uint32_t cubit_intel_prepare_context(uint64_t slot)
{ markers[slot] = 1; return 0; }
uint32_t cubit_intel_register_context(uint64_t slot)
{ assert(slot < 64); return 0; }
uint32_t cubit_intel_submit_batch(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t previous, uint32_t *completion)
{
   assert(step == 1 && handle == 17 && gpu == 0x20000 && offset == 0 && bytes == 4096);
   assert(previous == markers[slot]);
   submits_seen++; step = 2;
   *completion = submit_result ? 0 : ++markers[slot];
   return submit_result;
}
VkResult vk_sync_wait_many(struct vk_device *device, uint32_t count,
   const struct vk_sync_wait *values, enum vk_sync_wait_flags flags, uint64_t deadline)
{
   assert(step == 0 && count == 2 && values == waits && flags == 0 && deadline == 123);
   waits_seen++; step = 1;
   struct anv_device *anv = container_of(device, struct anv_device, vk);
   /* Re-enters the same tracker mutex: would deadlock if held over waiting. */
   assert(anv_cubit_memory_slot_retained(63));
   if (lose_during_wait) {
      assert(anv_cubit_submit_bo(anv, NULL, 0, 0, 0) == VK_ERROR_DEVICE_LOST);
   }
   return wait_result;
}
VkResult vk_sync_signal_many(struct vk_device *device, uint32_t count,
   const struct vk_sync_signal *values)
{
   (void)device;
   assert(step == 2 && count == 2 && values == signals);
   signals_seen++; step = 3;
   return signal_result;
}
static VkResult run(struct anv_device *device)
{
   step = 0;
   return anv_cubit_submit_bo_sync(device, &bo, 0x20000, 0, 4096,
                                  2, waits, 2, signals, 123);
}
int main(void)
{
   static struct anv_device devices[5];
   for (unsigned i = 0; i < 5; i++) {
      assert(anv_cubit_memory_init(&devices[i], 63-i) == VK_SUCCESS);
      assert(anv_cubit_prepare_submission(&devices[i]) == VK_SUCCESS);
   }
   wait_result = VK_TIMEOUT;
   assert(run(&devices[0]) == VK_TIMEOUT);
   assert(waits_seen == 1 && submits_seen == 0 && signals_seen == 0);
   wait_result = VK_SUCCESS;
   step = 0;
   assert(anv_cubit_wait_dependencies(&devices[0], 2, waits, 123) == VK_SUCCESS);
   assert(step == 1 && submits_seen == 0 && signals_seen == 0);
   assert(run(&devices[0]) == VK_SUCCESS && step == 3);
   assert(submits_seen == 1 && signals_seen == 1);
   submit_result = 4;
   assert(run(&devices[1]) == VK_ERROR_DEVICE_LOST);
   assert(submits_seen == 2 && signals_seen == 1);
   submit_result = 0;
   assert(run(&devices[1]) == VK_ERROR_DEVICE_LOST && step == 0);
   signal_result = VK_ERROR_OUT_OF_HOST_MEMORY;
   assert(run(&devices[2]) == VK_ERROR_DEVICE_LOST);
   assert(submits_seen == 3 && signals_seen == 2);
   signal_result = VK_SUCCESS;
   assert(run(&devices[2]) == VK_ERROR_DEVICE_LOST && step == 0);
   wait_result = VK_ERROR_DEVICE_LOST;
   assert(run(&devices[3]) == VK_ERROR_DEVICE_LOST);
   wait_result = VK_SUCCESS;
   assert(run(&devices[3]) == VK_ERROR_DEVICE_LOST && step == 0);
   lose_during_wait = true;
   step = 0;
   assert(anv_cubit_wait_dependencies(&devices[4], 2, waits, 123) == VK_ERROR_DEVICE_LOST);
   assert(step == 1 && submits_seen == 3 && signals_seen == 2);
   assert(run(&devices[4]) == VK_ERROR_DEVICE_LOST);
   assert(submits_seen == 3 && signals_seen == 2);
   puts("Queue sync ordering PASS: timeout, completion, signal failure, unlocked waits, lost-session revalidation (mock IPC/sync)");
}
