/* Actual ANV adapter/types, mocked IPC and session queue: not GPU execution.
 * Preparation, offline binding, the session queue's open and close around
 * the session, live VM generations and sticky failure, recycled lifetimes. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "gpu-queue-mock.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <assert.h>
#include <stdio.h>

static unsigned prepares, registers, fail_prepare, fail_register;
static unsigned binds, fail_bind;
static unsigned updates;
static uint32_t vm_generation, update_status;
static bool override_generation;
static uint32_t supplied_generation;
static unsigned drains, closes, retirement_polls;
static unsigned update_failures;
void cubit_test_mesa_transport_failure(const char *operation, uint32_t status,
                                       uint32_t handle);
void cubit_test_mesa_transport_failure(const char *operation, uint32_t status,
                                       uint32_t handle)
{
   (void)status;
   if (strcmp(operation, "vm-bind") && strcmp(operation, "vm-unbind")) return;
   assert(handle == 17);
   update_failures++;
}
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{
   assert(tracker && (tracker->slot == 53 || tracker->slot == 47));
   tracker->lost = true;
   drains++;
   return true;
}
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{
   assert(slot == 53 || slot == 47);
   assert(!gpu_queue_mock[slot].open);   /* the queue closes first */
   closes++;
   *tag = closes;
   return 0;
}
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{
   assert(slot == 53 || slot == 47);
   retirement_polls++;
   return 0;
}
uint32_t cubit_intel_update_binding(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t remove,
   uint32_t previous, uint32_t *generation)
{
   assert(slot < 64 && handle == 17 && gpu == 0x20000 && offset == 4096 && bytes == 4096);
   assert(remove <= 1 && previous == vm_generation);
   updates++;
   *generation = override_generation ? supplied_generation :
      update_status ? 0 : ++vm_generation;
   return update_status;
}
uint32_t cubit_intel_bind_buffer(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   assert(slot < 64 && handle == 17 && gpu == 0x20000 && offset == 0 && bytes == 8192);
   binds++;
   return fail_bind;
}
uint32_t cubit_intel_unbind_buffer(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes)
{
   (void)slot; (void)handle; (void)gpu; (void)offset; (void)bytes;
   assert(!"unexpected offline unbind in submission-only fixture");
   return 4;
}
uint32_t cubit_intel_prepare_context(uint64_t slot)
{ assert(slot < 64); prepares++; return fail_prepare; }
uint32_t cubit_intel_register_context(uint64_t slot)
{ assert(slot < 64); registers++; return fail_register; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                            int line, const char *message, ...)
{
   (void)device; (void)file; (void)line; (void)message;
   return VK_ERROR_DEVICE_LOST;
}
int main(void)
{
   static struct anv_device device, failed_prepare, failed_register, refused_queue, premature,
      failed_binding;
   struct anv_bo bo = { .gem_handle = 17, .actual_size = 8192 };
   assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
   assert(anv_cubit_bind_bo_offline(&device, NULL, 0x20000) == VK_ERROR_INITIALIZATION_FAILED);
   const uint64_t bad_gpu[] = {0, 1, UINT64_C(1) << 48, (UINT64_C(1) << 48) - 4096, UINT64_MAX};
   for (unsigned n = 0; n < sizeof(bad_gpu)/sizeof(bad_gpu[0]); n++)
      assert(anv_cubit_bind_bo_offline(&device, &bo, bad_gpu[n]) == VK_ERROR_INITIALIZATION_FAILED);
   struct anv_bo slab = { .slab_parent = &bo, .actual_size = 8192 };
   assert(anv_cubit_bind_bo_offline(&device, &slab, 0x20000) == VK_ERROR_INITIALIZATION_FAILED);
   assert(binds == 0);
   assert(anv_cubit_bind_bo_offline(&device, &bo, 0x20000) == VK_SUCCESS && binds == 1);
   gpu_queue_mock_reset();
   assert(anv_cubit_prepare_submission(&device) == VK_SUCCESS);
   assert(gpu_queue_mock_opens == 1 && gpu_queue_mock[63].open);
   assert(anv_cubit_bind_bo_offline(&device, &bo, 0x20000) == VK_ERROR_FEATURE_NOT_PRESENT);
   assert(binds == 1);
   assert(prepares == 1 && registers == 1);
   assert(anv_cubit_prepare_submission(&device) == VK_ERROR_INITIALIZATION_FAILED);
   assert(prepares == 1 && registers == 1 && gpu_queue_mock_opens == 1);
   assert(anv_cubit_memory_init(&failed_prepare, 62) == VK_SUCCESS);
   fail_prepare = 4;
   assert(anv_cubit_prepare_submission(&failed_prepare) == VK_ERROR_DEVICE_LOST);
   fail_prepare = 0;
   assert(anv_cubit_prepare_submission(&failed_prepare) == VK_ERROR_INITIALIZATION_FAILED);
   assert(prepares == 2 && registers == 1 && gpu_queue_mock_opens == 1);
   assert(anv_cubit_memory_init(&failed_register, 61) == VK_SUCCESS);
   fail_register = 3;
   assert(anv_cubit_prepare_submission(&failed_register) == VK_ERROR_DEVICE_LOST);
   fail_register = 0;
   assert(anv_cubit_prepare_submission(&failed_register) == VK_ERROR_INITIALIZATION_FAILED);
   assert(prepares == 3 && registers == 2 && gpu_queue_mock_opens == 1);
   /* A queue the driver refuses fails preparation, never replayed. */
   assert(anv_cubit_memory_init(&refused_queue, 60) == VK_SUCCESS);
   gpu_queue_mock_refuse_open = 60;
   assert(anv_cubit_prepare_submission(&refused_queue) == VK_ERROR_DEVICE_LOST);
   gpu_queue_mock_refuse_open = UINT64_MAX;
   assert(anv_cubit_prepare_submission(&refused_queue) == VK_ERROR_INITIALIZATION_FAILED);
   assert(prepares == 4 && registers == 3 && gpu_queue_mock_opens == 2);
   assert(anv_cubit_memory_init(&premature, 59) == VK_SUCCESS);
   assert(anv_cubit_update_bo_binding(&premature, &bo, 0x20000, 4096, 4096, false) ==
          VK_ERROR_INITIALIZATION_FAILED);
   assert(prepares == 4 && registers == 3);
   assert(anv_cubit_memory_init(&failed_binding, 58) == VK_SUCCESS);
   fail_bind = 4;
   assert(anv_cubit_bind_bo_offline(&failed_binding, &bo, 0x20000) == VK_ERROR_DEVICE_LOST);
   fail_bind = 0;
   assert(anv_cubit_bind_bo_offline(&failed_binding, &bo, 0x20000) == VK_ERROR_DEVICE_LOST);
   assert(anv_cubit_prepare_submission(&failed_binding) == VK_ERROR_INITIALIZATION_FAILED);
   assert(binds == 2 && prepares == 4);
   static struct anv_device live, bad_update[4], rejected_update;
   assert(anv_cubit_memory_init(&live, 53) == VK_SUCCESS);
   assert(anv_cubit_update_bo_binding(&live, &bo, 0x20000, 4096, 4096, false) ==
          VK_ERROR_INITIALIZATION_FAILED);
   assert(updates == 0);
   assert(anv_cubit_prepare_submission(&live) == VK_SUCCESS);
   for (unsigned n = 0; n < sizeof(bad_gpu)/sizeof(bad_gpu[0]); n++) {
      /* The final aligned page is valid for this smaller, one-page slice. */
      if (bad_gpu[n] == (UINT64_C(1) << 48) - 4096)
         continue;
      assert(anv_cubit_update_bo_binding(&live, &bo, bad_gpu[n], 4096, 4096, false) ==
             VK_ERROR_INITIALIZATION_FAILED);
   }
   assert(anv_cubit_update_bo_binding(&live, &bo, 0x20000, 8192, 4096, false) ==
          VK_ERROR_INITIALIZATION_FAILED);
   assert(anv_cubit_update_bo_binding(&live, &slab, 0x20000, 4096, 4096, false) ==
          VK_ERROR_INITIALIZATION_FAILED);
   assert(updates == 0);
   for (unsigned n = 0; n < 20; n++)
      assert(anv_cubit_update_bo_binding(&live, &bo, 0x20000, 4096, 4096, n & 1) == VK_SUCCESS);
   assert(updates == 20 && vm_generation == 20);
   const uint32_t invalid_generations[] = {0, 2, 7, UINT32_MAX};
   for (unsigned n = 0; n < 4; n++) {
      assert(anv_cubit_memory_init(&bad_update[n], 52 - n) == VK_SUCCESS);
      assert(anv_cubit_prepare_submission(&bad_update[n]) == VK_SUCCESS);
      vm_generation = 0;
      override_generation = true;
      supplied_generation = invalid_generations[n];
      unsigned before = updates, failures = update_failures;
      assert(anv_cubit_update_bo_binding(&bad_update[n], &bo, 0x20000, 4096, 4096, false) == VK_ERROR_DEVICE_LOST);
      supplied_generation = 1;
      /* Sticky: never replayed, never reported twice. */
      assert(anv_cubit_update_bo_binding(&bad_update[n], &bo, 0x20000, 4096, 4096, false) == VK_ERROR_DEVICE_LOST);
      assert(updates == before + 1 && update_failures == failures + 1);
   }
   assert(anv_cubit_memory_init(&rejected_update, 48) == VK_SUCCESS);
   assert(anv_cubit_prepare_submission(&rejected_update) == VK_SUCCESS);
   override_generation = false;
   vm_generation = 0;
   update_status = 3;
   assert(anv_cubit_update_bo_binding(&rejected_update, &bo, 0x20000, 4096, 4096, false) == VK_ERROR_DEVICE_LOST);
   update_status = 0;
   unsigned before = updates;
   assert(anv_cubit_update_bo_binding(&rejected_update, &bo, 0x20000, 4096, 4096, false) == VK_ERROR_DEVICE_LOST);
   assert(updates == before);
   /* Recycle a genuinely used submission lifetime, not an empty tracker.
    * The old session reached VM generation 20. A new session must
    * prepare/register and open its queue anew and start at VM 0. */
   struct cubit_cpu_mapping_tracker *retired_tracker = live.cubit_cpu_mappings;
   const unsigned queue_closes = gpu_queue_mock_closes;
   assert(anv_cubit_memory_finish(&live) == VK_SUCCESS);
   assert(gpu_queue_mock_closes == queue_closes + 1 && !gpu_queue_mock[53].open);
   assert(!live.cubit_cpu_mappings && !anv_cubit_memory_slot_retained(53));
   for (unsigned cycle = 0; cycle < 64; cycle++) {
      uint64_t slot = cycle & 1 ? 53 : 47;
      memset(&live, 0, sizeof(live));
      assert(anv_cubit_memory_init(&live, slot) == VK_SUCCESS);
      assert(live.cubit_cpu_mappings == retired_tracker);
      unsigned before_prepare = prepares, before_register = registers;
      assert(anv_cubit_bind_bo_offline(&live, &bo, 0x20000) == VK_SUCCESS);
      assert(anv_cubit_prepare_submission(&live) == VK_SUCCESS);
      assert(prepares == before_prepare + 1 && registers == before_register + 1);
      assert(gpu_queue_mock[slot].open);
      vm_generation = 0;
      for (unsigned operation = 0; operation < 3; operation++)
         assert(anv_cubit_update_bo_binding(&live, &bo, 0x20000, 4096, 4096, false) == VK_SUCCESS);
      assert(vm_generation == 3);
      assert(anv_cubit_memory_finish(&live) == VK_SUCCESS);
      unsigned saved_closes = closes;
      assert(anv_cubit_memory_poll() == 0 && closes == saved_closes);
      assert(!anv_cubit_memory_slot_retained(slot));
      assert(anv_cubit_memory_slot_retained(63)); /* other failed session retained */
   }
   assert(drains == 65 && closes == 65 && retirement_polls == 65);
   puts("ANV submission lifecycle PASS: real types, mocked IPC/queue, one-shot preparation and queue open, refused queue, live VM generations with sticky failure, queue closed before the session over 65 recycled lifetimes");
   return 0;
}
