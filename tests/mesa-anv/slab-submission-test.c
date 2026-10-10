/* Slab batches through the session queue: actual ANV types and adapter,
 * real vk_sync, mock queue and IPC. A batch in a slab child is described by
 * its parent's handle at the translated offset; bad extents fail closed
 * before any descriptor. No hardware execution. */
#include "queue-rig.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <stdio.h>

uint32_t cubit_intel_memory_contract(uint64_t slot) { (void)slot; return 1; }
uint32_t cubit_intel_session_status(uint64_t slot) { (void)slot; return 0; }
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ (void)tracker; return true; }
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag) { *tag = slot + 1; return 0; }
uint32_t cubit_intel_poll_session_retirement(uint64_t slot) { (void)slot; return 0; }
uint32_t cubit_intel_prepare_context(uint64_t slot) { (void)slot; return 0; }
uint32_t cubit_intel_register_context(uint64_t slot) { (void)slot; return 0; }
void util_flush_range(void *start, size_t size) { (void)start; (void)size; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
   int line, const char *message, ...)
{
   (void)file; (void)line; (void)message;
   p_atomic_set(&device->_lost.lost, 1);
   return VK_ERROR_DEVICE_LOST;
}

int main(void)
{
   rig_physical();
   static struct rig r, rejected[9];
   static uint32_t memory[4096];
   struct anv_bo parent = {.gem_handle = 17, .actual_size = 16384};
   struct anv_bo child = {.slab_parent = &parent, .actual_size = 4096, .size = 4096, .map = memory};
   open_rig(&r, 63);
   r.batches[0] = &child;
   struct vk_sync *fence = make(&r, &binary_type.sync, 0);
   const uint64_t bases[] = {0x10000, (UINT64_C(1) << 47) - 4096, (UINT64_C(1) << 47) + 4096};
   unsigned slices = 0;
   for (unsigned b = 0; b < 3; b++) {
      parent.offset = intel_canonical_address(bases[b]);
      for (uint64_t offset = 0; offset <= 8192; offset += 8) {
         child.offset = intel_canonical_address(bases[b] + 4096 + offset);
         assert(vk_sync_reset(&r.device.vk, fence) == VK_SUCCESS);
         assert(startup(&r, fence, 0, NULL) == VK_SUCCESS);
         const struct gpu_queue_mock_slot *q = &gpu_queue_mock[r.slot];
         assert(q->last.handle == 17 && q->last.offset == 4096 + offset && q->last.bytes == 4096 &&
                q->last.gpu == intel_48b_address(child.offset));
         gpu_queue_mock_complete(r.slot, 0, q->submitted.value[0]);
         assert(vk_sync_wait(&r.device.vk, fence, 0, 0, 0) == VK_SUCCESS);
         slices++;
      }
   }
   for (unsigned n = 0; n < 9; n++) {
      parent.offset = 0x10000; parent.gem_handle = 17;
      child.offset = 0x11000; child.actual_size = 8192; child.size = 4096;
      child.slab_parent = &parent;
      switch (n) {
      case 0: child.offset = 0xF000; break;       /* before parent */
      case 1: child.offset = 0x14000; break;      /* after parent */
      case 2: child.actual_size = UINT64_MAX; break;
      case 3: child.offset = 0x13800; child.actual_size = 4096; break; /* crosses parent end */
      case 4: child.offset |= UINT64_C(1) << 60; break; /* noncanonical */
      case 5: parent.offset |= UINT64_C(1) << 60; break;
      case 6: child.slab_parent = &child; break; /* invalid parent chain */
      case 7: parent.gem_handle = 0; break;
      case 8:
         parent.offset = child.offset = intel_canonical_address((UINT64_C(1) << 48) - 4096);
         break; /* parent extent crosses the GPU address-space limit */
      }
      open_rig(&rejected[n], 62 - n);
      rejected[n].batches[0] = &child;
      struct vk_sync *f = make(&rejected[n], &binary_type.sync, 0);
      assert(startup(&rejected[n], f, 0, NULL) == VK_ERROR_DEVICE_LOST);
      assert(gpu_queue_mock[rejected[n].slot].jobs == 0);
      close_rig(&rejected[n]);
   }
   close_rig(&r);
   printf("ANV slab submission PASS: %u translated slices through the queue, canonical boundary, "
          "9 invalid extents fail before any descriptor (mock queue/IPC)\n", slices);
   return 0;
}
