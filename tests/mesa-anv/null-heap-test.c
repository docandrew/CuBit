#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <assert.h>
#include <stdio.h>
#include "../../userspace/c/cubit_handles.h"
/* A test endpoint handle (generation 1). */
#define TEST_ENDPOINT(n) ((cubit_endpoint){(UINT64_C(1) << 32) | (uint64_t)(n)})
/* The index a test endpoint was made from. */
#define TEST_INDEX(e) ((unsigned)((e).opaque & 0xffffffffu))
static unsigned creates;
static unsigned drains, closes, polls;
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ assert(tracker->endpoint.opaque == TEST_ENDPOINT(63).opaque); drains++; return true; }
uint32_t cubit_intel_close_session(cubit_endpoint endpoint, uint64_t *tag)
{ assert(endpoint.opaque == TEST_ENDPOINT(63).opaque && drains == 1); closes++; *tag = 1; return 0; }
uint32_t cubit_intel_poll_session_retirement(cubit_endpoint endpoint)
{ assert(endpoint.opaque == TEST_ENDPOINT(63).opaque && closes == 1); return ++polls == 1 ? 4 : 0; }
uint32_t cubit_intel_prepare_context(cubit_endpoint endpoint)
{ assert(endpoint.opaque == TEST_ENDPOINT(62).opaque); return 0; }
uint32_t cubit_intel_register_context(cubit_endpoint endpoint)
{ assert(endpoint.opaque == TEST_ENDPOINT(62).opaque); return 0; }
uint32_t cubit_intel_create_buffer(cubit_endpoint endpoint, uint64_t bytes, uint32_t *handle)
{ assert(endpoint.opaque == TEST_ENDPOINT(63).opaque && bytes == 4096); creates++; *handle = 17; return 0; }
VkResult _vk_device_set_lost(struct vk_device *d, const char *file,
                            int line, const char *message, ...)
{ (void)d; (void)file; (void)line; (void)message; return VK_ERROR_DEVICE_LOST; }
int main(void)
{
   static struct anv_device device;
   static struct anv_physical_device physical;
   struct intel_memory_class_instance region = {0};
   const struct intel_memory_class_instance *regions[] = {&region};
   physical.va.null_initialized_heap.addr = UINT64_C(1) << 36;
   physical.va.null_initialized_heap.size = UINT64_C(8) << 30;
   physical.sys.region = &region;
   physical.memory.need_flush = true;
   device.physical = &physical;
   assert(anv_cubit_memory_init(&device, TEST_ENDPOINT(63)) == VK_SUCCESS);
   uint64_t actual = 99;
   enum anv_bo_alloc_flags flags = ANV_BO_ALLOC_HOST_CACHED | ANV_BO_ALLOC_NULL_INITIALIZED_HEAP;
   assert(!anv_cubit_gem_create(&device, regions, 1, 4096, flags, &actual));
   assert(actual == 0 && creates == 0);
   struct anv_vm_bind bind = {.address = physical.va.null_initialized_heap.addr,
      .size = physical.va.null_initialized_heap.size, .op = ANV_VM_BIND};
   struct anv_sparse_submission submit = {.binds = &bind, .binds_len = 1, .binds_capacity = 1};
   for (unsigned malformed = 0; malformed < 10; malformed++) {
      struct anv_vm_bind b = bind;
      struct anv_sparse_submission s = submit;
      s.binds = &b;
      enum anv_vm_bind_flags f = ANV_VM_BIND_FLAG_SIGNAL_BIND_TIMELINE;
      switch (malformed) {
      case 0: b.address += 4096; break;
      case 1: b.size -= 4096; break;
      case 2: b.bo_offset = 4096; break;
      case 3: b.bo = (struct anv_bo *)(uintptr_t)1; break;
      case 4: b.op = ANV_VM_UNBIND_ALL; break;
      case 5: s.binds_len = 2; break;
      case 6: s.wait_count = 1; break;
      case 7: s.signal_count = 1; break;
      case 8: s.queue = (struct anv_queue *)(uintptr_t)1; break;
      case 9: f = (enum anv_vm_bind_flags)2; break;
      }
      assert(anv_cubit_vm_bind(&device, &s, f) == VK_ERROR_FEATURE_NOT_PRESENT);
      assert(creates == 0);
   }
   bind.op = ANV_VM_UNBIND;
   assert(anv_cubit_vm_bind(&device, &submit, 0) != VK_SUCCESS);
   bind.op = ANV_VM_BIND;
   assert(anv_cubit_vm_bind(&device, &submit, 1) == VK_SUCCESS);
   assert(anv_cubit_vm_bind(&device, &submit, 1) != VK_SUCCESS);
   assert(anv_cubit_gem_create(&device, regions, 1, 4096, flags, &actual) == 17);
   assert(actual == 4096 && creates == 1);
   assert(!anv_cubit_gem_create(&device, regions, 1, 4096,
      flags | ANV_BO_ALLOC_HOST_COHERENT, &actual));
   bind.op = ANV_VM_UNBIND;
   assert(anv_cubit_vm_bind(&device, &submit, 1) == VK_SUCCESS);
   assert(anv_cubit_vm_bind(&device, &submit, 1) != VK_SUCCESS);
   bind.op = ANV_VM_BIND;
   assert(anv_cubit_vm_bind(&device, &submit, 1) != VK_SUCCESS);
   assert(!anv_cubit_gem_create(&device, regions, 1, 4096, flags, &actual));
   assert(creates == 1);
   static struct anv_device late;
   late.physical = &physical;
   assert(anv_cubit_memory_init(&late, TEST_ENDPOINT(62)) == VK_SUCCESS);
   assert(anv_cubit_prepare_submission(&late) == VK_SUCCESS);
   assert(anv_cubit_vm_bind(&late, &submit, 1) != VK_SUCCESS);
   assert(anv_cubit_memory_finish(&device) == VK_ERROR_DEVICE_LOST);
   assert(device.cubit_cpu_mappings == NULL && drains == 1 && closes == 1 && polls == 1);
   assert(anv_cubit_memory_endpoint_retained(TEST_ENDPOINT(63)));
   assert(anv_cubit_memory_poll() == 0);
   assert(!anv_cubit_memory_endpoint_retained(TEST_ENDPOINT(63)) && drains == 1 && closes == 1 && polls == 2);
   static struct anv_device reused;
   reused.physical = &physical;
   assert(anv_cubit_memory_init(&reused, TEST_ENDPOINT(63)) == VK_SUCCESS);
   assert(anv_cubit_vm_bind(&reused, &submit, 1) == VK_SUCCESS);
   puts("ANV null heap PASS: exact lifecycle, reject sparse/sync/replay, gated BO flags (mock IPC)");
}
