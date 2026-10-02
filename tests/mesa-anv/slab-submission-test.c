/* Actual ANV types and adapter; IPC is mocked, no GPU execution. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include <assert.h>
#include <stdio.h>

static unsigned calls;
static uint64_t expected_offset;
static uint32_t marker = 1;
uint32_t cubit_intel_prepare_context(uint64_t slot) { return 0; }
uint32_t cubit_intel_register_context(uint64_t slot) { return 0; }
uint32_t cubit_intel_submit_batch(uint64_t slot, uint32_t handle,
   uint64_t gpu, uint64_t offset, uint64_t bytes, uint32_t previous,
   uint32_t *completion)
{
   assert(slot == 63 && handle == 17 && gpu == 0x20000);
   assert(offset == expected_offset && bytes == 4096 && previous == marker);
   calls++;
   *completion = ++marker;
   return 0;
}
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                            int line, const char *message, ...)
{ return VK_ERROR_DEVICE_LOST; }

int main(void)
{
   static struct anv_device device, rejected[9];
   struct anv_bo parent = { .gem_handle = 17, .actual_size = 16384 };
   struct anv_bo child = { .slab_parent = &parent, .actual_size = 8192 };
   assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
   assert(anv_cubit_prepare_submission(&device) == VK_SUCCESS);
   const uint64_t bases[] = {0x10000, (UINT64_C(1) << 47) - 4096,
                            (UINT64_C(1) << 47) + 4096};
   for (unsigned b = 0; b < 3; b++) {
      parent.offset = intel_canonical_address(bases[b]);
      child.offset = intel_canonical_address(bases[b] + 4096);
      for (uint64_t offset = 0; offset <= 4096; offset += 8) {
         expected_offset = 4096 + offset;
         assert(anv_cubit_submit_bo(&device, &child, 0x20000, offset, 4096) == VK_SUCCESS);
      }
   }
   const unsigned good_calls = calls;
   for (unsigned n = 0; n < 9; n++) {
      parent.offset = 0x10000; parent.gem_handle = 17;
      child.offset = 0x11000; child.actual_size = 8192;
      child.slab_parent = &parent;
      uint64_t offset = 0;
      switch (n) {
      case 0: child.offset = 0xF000; break;       /* before parent */
      case 1: child.offset = 0x14000; break;      /* after parent */
      case 2: child.actual_size = UINT64_MAX; break;
      case 3: offset = 4097; break;              /* crosses child end */
      case 4: child.offset |= UINT64_C(1) << 60; break; /* noncanonical */
      case 5: parent.offset |= UINT64_C(1) << 60; break;
      case 6: child.slab_parent = &child; break; /* invalid parent chain */
      case 7: parent.gem_handle = 0; break;
      case 8:
         parent.offset = child.offset = intel_canonical_address((UINT64_C(1) << 48) - 4096);
         break; /* parent extent crosses the GPU address-space limit */
      }
      assert(anv_cubit_memory_init(&rejected[n], 62 - n) == VK_SUCCESS);
      assert(anv_cubit_prepare_submission(&rejected[n]) == VK_SUCCESS);
      assert(anv_cubit_submit_bo(&rejected[n], &child, 0x20000, offset, 4096) == VK_ERROR_DEVICE_LOST);
      assert(calls == good_calls);
      assert(anv_cubit_submit_bo(&rejected[n], &parent, 0x20000, 0, 4096) == VK_ERROR_DEVICE_LOST);
      assert(calls == good_calls);
   }
   puts("ANV slab submission PASS: 1539 translated slices, canonical boundary, invalid extents and sticky failure (mock IPC)");
}
