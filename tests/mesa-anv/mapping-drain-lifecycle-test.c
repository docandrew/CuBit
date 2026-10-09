/* Real ANV lifetime coordinator + real CPU mapping tracker; mock transport.
 * No native IPC, GPU work, mapped RAM access or physical reclamation. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_memory.h"
#include <assert.h>
#include <stdio.h>

enum { COUNT = 2 * CUBIT_CPU_DRAIN_QUANTUM + 1 };
static unsigned maps, returns, retire_calls, closes, session_polls;
static unsigned returned[COUNT + 1], retired[COUNT + 1];
static bool pending_first;

uint32_t cubit_intel_map_buffer(uint64_t slot, uint32_t handle,
   uint64_t offset, uint64_t bytes, uint32_t writable,
   uint32_t *mapping, uint64_t *reference)
{
   assert(slot == 63 && handle == 1 && offset == 4096 && bytes == 8192 && writable == 1);
   assert(maps < COUNT);
   *mapping = ++maps;
   *reference = maps;
   return 0;
}
uint32_t cubit_intel_acquire_view(uint64_t slot, uint64_t reference,
   uint64_t offset, uint64_t bytes, uint64_t writable, uint64_t *address)
{
   assert(slot == 63 && reference && reference <= maps);
   assert(!offset && bytes == 8192 && writable == 1);
   *address = UINT64_C(0x400000000000) + reference * (16 * 1024 * 1024);
   return 0;
}
uint32_t cubit_intel_return_view(uint64_t reference)
{
   assert(reference && reference <= maps && !returned[reference]);
   returned[reference] = 1;
   returns++;
   return 0;
}
uint32_t cubit_intel_retire_mapping(uint64_t slot, uint32_t mapping)
{
   assert(slot == 63 && mapping && mapping <= maps && returned[mapping]);
   assert(!retired[mapping]);
   retire_calls++;
   if (mapping == 1 && pending_first) return 4;
   retired[mapping] = 1;
   return 0;
}
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{
   assert(slot == 63 && returns == COUNT && !closes);
   for (unsigned i = 1; i <= COUNT; i++) assert(retired[i]);
   closes++;
   *tag = 1;
   return 0;
}
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{
   assert(slot == 63 && closes == 1);
   session_polls++;
   return 0;
}
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                             int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}
int main(void)
{
   for (unsigned delayed = 0; delayed < 2; delayed++) {
      struct anv_device device = {0}, other = {0};
      maps = returns = retire_calls = closes = session_polls = 0;
      for (unsigned i = 0; i <= COUNT; i++) returned[i] = retired[i] = 0;
      pending_first = delayed;
      assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
      for (unsigned i = 0; i < COUNT; i++) {
         uint64_t address;
         assert(!cubit_cpu_tracker_map(device.cubit_cpu_mappings,
           1, 4096, 8192, 1, &address));
         assert(address);
      }
      assert(anv_cubit_memory_finish(&device) == VK_ERROR_DEVICE_LOST);
      assert(!device.cubit_cpu_mappings && returns == CUBIT_CPU_DRAIN_QUANTUM);
      assert(!closes && !session_polls && anv_cubit_memory_slot_retained(63));
      assert(anv_cubit_memory_init(&other, 63) == VK_ERROR_INITIALIZATION_FAILED);
      assert(anv_cubit_memory_poll() == 1);
      assert(returns == 2 * CUBIT_CPU_DRAIN_QUANTUM && !closes);
      pending_first = false;
      assert(anv_cubit_memory_poll() == delayed);
      assert(returns == COUNT);
      if (delayed) {
         assert(!closes && !session_polls && anv_cubit_memory_slot_retained(63));
         assert(anv_cubit_memory_poll() == 1);
         assert(anv_cubit_memory_poll() == 1);
         assert(!closes && anv_cubit_memory_slot_retained(63));
         assert(anv_cubit_memory_poll() == 0);
      }
      assert(closes == 1 && session_polls == 1 && returns == COUNT);
      assert(retire_calls == COUNT + delayed);
      assert(!anv_cubit_memory_slot_retained(63));
      assert(anv_cubit_memory_poll() == 0 && closes == 1);
   }
   puts("Composed ANV drain PASS:129 mappings,64-record batches, pending sweep, exact close/endpoint gate");
}
