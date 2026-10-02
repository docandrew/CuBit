/* Actual ANV types/adapter, mocked drain completion.
 * Hosted execution: no kernel IPC or GPU submission. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <stdatomic.h>
#include <sched.h>
#include "../../userspace/mesa/anv/native_gpu_buffers.h"

static atomic_uint active_calls, create_calls;
static uint64_t expected_bytes = 4096;
uint32_t cubit_intel_create_buffer(uint64_t slot, uint64_t bytes, uint32_t *handle)
{
   assert(slot == 63 && bytes == expected_bytes);
   assert(atomic_fetch_add(&active_calls, 1) == 0);
   sched_yield();
   *handle = atomic_fetch_add(&create_calls, 1) + 1;
   assert(atomic_fetch_sub(&active_calls, 1) == 1);
   return 0;
}
static void *create_worker(void *arg)
{
   struct anv_device *device = arg;
   const struct intel_memory_class_instance *region = device->physical->sys.region;
   for (unsigned i = 0; i < 100; i++) {
      uint64_t actual = 0;
      assert(anv_cubit_gem_create(device, &region, 1, 1,
                                 ANV_BO_ALLOC_HOST_CACHED |
                                 (i % 2 ? ANV_BO_ALLOC_FIXED_ADDRESS : 0) |
                                 (i % 4 >= 2 ? ANV_BO_ALLOC_CAPTURE : 0), &actual) != 0);
      assert(actual == 4096);
   }
   return NULL;
}

static unsigned allocations, frees, drains, losses;
static bool drained;
static unsigned closes, retirement_queries;
static uint32_t retirement_status = 4;
static uint32_t close_status;
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{
   assert(slot <= 63 && drained);
   closes++;
   *tag = close_status ? 0 : UINT64_C(0x4750000000000001);
   return close_status;
}
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{
   assert(slot <= 63 && closes && drained);
   retirement_queries++;
   return retirement_status;
}
static void *VKAPI_PTR allocate(void *user, size_t size, size_t alignment,
                               VkSystemAllocationScope scope)
{
   (void)user;
   assert(alignment == 8 && scope == VK_SYSTEM_ALLOCATION_SCOPE_DEVICE);
   allocations++;
   return malloc(size);
}
static void VKAPI_PTR release(void *user, void *ptr)
{
   (void)user;
   frees++;
   free(ptr);
}
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{
   assert(tracker && tracker->slot <= 63);
   drains++;
   tracker->lost = true;
   return drained;
}
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                             int line, const char *message, ...)
{
   (void)device; (void)file; (void)line; (void)message;
   losses++;
   return VK_ERROR_DEVICE_LOST;
}
int main(void)
{
   static struct anv_device device;
   for (unsigned bit = 0; bit < 31; bit++)
      assert(anv_cubit_bo_flags(&device, (enum anv_bo_alloc_flags)(1u << bit)) == 0);
   device.vk.alloc.pfnAllocation = allocate;
   device.vk.alloc.pfnFree = release;
   assert(anv_cubit_memory_finish(&device) == VK_SUCCESS && drains == 0);
   assert(!anv_cubit_memory_slot_retained(63));
   assert(anv_cubit_memory_slot_retained(64));
   assert(anv_cubit_memory_init(&device, 64) == VK_ERROR_INITIALIZATION_FAILED);
   assert(allocations == 0 && !device.cubit_cpu_mappings);
   assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
   static struct anv_physical_device physical;
   static struct intel_memory_class_instance region;
   physical.sys.region = &region;
   physical.memory.need_flush = true;
   device.physical = &physical;
   const struct intel_memory_class_instance *cache_region = &region;
   uint64_t rejected_size = 99;
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 4096, 0, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   /* Common ANV owns AUX-CCS size expansion and AUX-TT GPU VA alignment.
    * The callback must not reject those policies on an initialized platform,
    * nor add the metadata size a second time. No GPU work in this fixture. */
   struct intel_device_info info = {0};
   device.info = &info;
   const enum anv_bo_alloc_flags aux_flags = ANV_BO_ALLOC_HOST_CACHED |
      ANV_BO_ALLOC_AUX_TT_ALIGNED | ANV_BO_ALLOC_AUX_CCS;
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 8192,
                              aux_flags, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   info.has_aux_map = true;
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 8192,
                              aux_flags, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   /* Opaque context presence only; adapter must never dereference it. */
   device.aux_map_ctx = (struct intel_aux_map_context *)&info;
   expected_bytes = 8192;
   assert(anv_cubit_gem_create(&device, &cache_region, 1, 8192,
                             aux_flags, &rejected_size));
   assert(rejected_size == 8192 && create_calls == 1);
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 8192,
      aux_flags | ANV_BO_ALLOC_COMPRESSED, &rejected_size));
   assert(rejected_size == 0 && create_calls == 1);
   info.has_aux_map = false;
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 8192,
                              aux_flags, &rejected_size));
   assert(rejected_size == 0 && create_calls == 1);
   info.has_aux_map = true;
   const enum anv_bo_alloc_flags individual_aux[] = {
      ANV_BO_ALLOC_AUX_TT_ALIGNED, ANV_BO_ALLOC_AUX_CCS,
   };
   for (unsigned n = 0; n < 2; n++) {
      expected_bytes = 12288;
      assert(anv_cubit_gem_create(&device, &cache_region, 1, 8193,
         ANV_BO_ALLOC_HOST_CACHED | individual_aux[n], &rejected_size));
      assert(rejected_size == 12288 && create_calls == n + 2);
   }
   expected_bytes = UINT64_C(16) * 1024 * 1024;
   assert(anv_cubit_gem_create(&device, &cache_region, 1, expected_bytes,
                              aux_flags, &rejected_size));
   assert(rejected_size == expected_bytes && create_calls == 4);
   const uint64_t invalid_sizes[] = {0, expected_bytes + 1, UINT64_MAX};
   for (unsigned n = 0; n < 3; n++) {
      rejected_size = 99;
      assert(!anv_cubit_gem_create(&device, &cache_region, 1, invalid_sizes[n],
                                 aux_flags, &rejected_size));
      assert(rejected_size == 0 && create_calls == 4);
   }
   device.aux_map_ctx = NULL;
   device.info = NULL;
   expected_bytes = 4096;
   atomic_store(&create_calls, 0);
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 4096,
      ANV_BO_ALLOC_HOST_CACHED | ANV_BO_ALLOC_HOST_COHERENT, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   physical.memory.need_flush = false;
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 4096,
      ANV_BO_ALLOC_HOST_CACHED, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   physical.memory.need_flush = true;
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 4096,
      ANV_BO_ALLOC_HOST_CACHED | ANV_BO_ALLOC_FIXED_ADDRESS |
      ANV_BO_ALLOC_HOST_COHERENT, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 4096,
      ANV_BO_ALLOC_HOST_CACHED | ANV_BO_ALLOC_FIXED_ADDRESS |
      ANV_BO_ALLOC_CAPTURE | ANV_BO_ALLOC_HOST_COHERENT, &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   pthread_t workers[4];
   /* This is a GPU VA heap with null mappings in unused pages/prefetch guard,
    * not a request to zero the physical BO. Reject independently of coherence:
    * accepting it as a harmless allocation hint would hide missing VM support. */
   assert(!anv_cubit_gem_create(&device, &cache_region, 1, 4096,
      ANV_BO_ALLOC_HOST_CACHED | ANV_BO_ALLOC_NULL_INITIALIZED_HEAP,
      &rejected_size));
   assert(rejected_size == 0 && create_calls == 0);
   for (unsigned i = 0; i < 4; i++)
      assert(pthread_create(&workers[i], NULL, create_worker, &device) == 0);
   for (unsigned i = 0; i < 4; i++)
      assert(pthread_join(workers[i], NULL) == 0);
   assert(create_calls == 400);
   /* Mesa can declare device loss outside this transport. No new allocation
    * may reach IPC even while the CPU tracker itself remains healthy. */
   p_atomic_set(&device.vk._lost.lost, 1);
   const struct intel_memory_class_instance *selected = &region;
   uint64_t actual = 1234;
   assert(anv_cubit_gem_create(&device, &selected, 1, 4096,
                              ANV_BO_ALLOC_HOST_CACHED, &actual) == 0);
   assert(actual == 0 && create_calls == 400);
   assert(!device.cubit_cpu_mappings->lost);
   struct cubit_cpu_mapping_tracker *saved = device.cubit_cpu_mappings;
   assert(anv_cubit_memory_slot_retained(63));
   assert(saved && saved->slot == 63 && !saved->lost && saved->used == 0);
   for (unsigned i = 0; i < CUBIT_CPU_MAPPING_CAPACITY; i++)
      assert(saved->records[i].state == CUBIT_MAP_EMPTY);
   assert(anv_cubit_memory_init(&device, 62) == VK_ERROR_INITIALIZATION_FAILED);
   assert(allocations == 0 && device.cubit_cpu_mappings == saved);
   static struct anv_device second;
   p_atomic_set(&second.vk._lost.lost, 1);
   assert(anv_cubit_memory_init(&second, 62) == VK_ERROR_INITIALIZATION_FAILED);
   assert(!second.cubit_cpu_mappings && !anv_cubit_memory_slot_retained(62));
   /* Reset only the uninitialized test fixture, never a live lost device. */
   memset(&second, 0, sizeof(second));
   assert(anv_cubit_memory_init(&second, 63) == VK_ERROR_INITIALIZATION_FAILED);
   assert(!second.cubit_cpu_mappings);
   assert(anv_cubit_memory_finish(&device) == VK_ERROR_DEVICE_LOST);
   assert(!device.cubit_cpu_mappings && frees == 0 && losses == 1);
   assert(saved->lost);
   assert(anv_cubit_memory_slot_retained(63));
   assert(anv_cubit_memory_init(&second, 63) == VK_ERROR_INITIALIZATION_FAILED);
   assert(!second.cubit_cpu_mappings);
   /* Destroy every byte of the wrapper; polling must not use it or alloc. */
   memset(&device, 0xA5, sizeof(device));
   assert(anv_cubit_memory_poll() == 1 && drains == 2);
   assert(closes == 0 && retirement_queries == 0);
   drained = true;
   assert(anv_cubit_memory_poll() == 1 && drains == 3);
   assert(closes == 1 && retirement_queries == 1);
   assert(anv_cubit_memory_slot_retained(63));
   assert(anv_cubit_memory_poll() == 1 && drains == 3);
   assert(closes == 1 && retirement_queries == 2);
   retirement_status = 0;
   assert(anv_cubit_memory_poll() == 0 && drains == 3);
   assert(!anv_cubit_memory_slot_retained(63));
   assert(anv_cubit_memory_poll() == 0 && drains == 3);
   memset(&device, 0, sizeof(device));
   for (unsigned i = 0; i < 128; i++) {
      assert(anv_cubit_memory_init(&device, 63) == VK_SUCCESS);
      assert(device.cubit_cpu_mappings == saved);
      assert(!saved->lost && !saved->used);
      for (unsigned j = 0; j < CUBIT_CPU_MAPPING_CAPACITY; j++)
         assert(saved->records[j].state == CUBIT_MAP_EMPTY);
      assert(anv_cubit_memory_finish(&device) == VK_SUCCESS);
      assert(!anv_cubit_memory_slot_retained(63));
   }
   for (unsigned i = 0; i < 2; i++) {
      assert(anv_cubit_memory_init(&device, i ? 62 : 63) == VK_SUCCESS);
      close_status = i ? 0 : 4;
      retirement_status = i ? 5 : 0;
      assert(anv_cubit_memory_finish(&device) ==
             VK_ERROR_DEVICE_LOST);
   }
   unsigned previous_closes = closes, previous_queries = retirement_queries;
   assert(anv_cubit_memory_poll() == 2);
   assert(closes == previous_closes && retirement_queries == previous_queries);
   assert(anv_cubit_memory_slot_retained(63) && anv_cubit_memory_slot_retained(62));
   close_status = 0; retirement_status = 0;
   assert(anv_cubit_memory_init(&device, 61) == VK_SUCCESS);
   assert(device.cubit_cpu_mappings != saved); /* uncertain record retained */
   assert(anv_cubit_memory_finish(&device) == VK_SUCCESS);
   /* Capacity bounds outstanding uncertain lifetimes, not successful history. */
   close_status = 4;
   for (unsigned slot = 1; slot <= 14; slot++) {
      assert(anv_cubit_memory_init(&device, slot) == VK_SUCCESS);
      assert(anv_cubit_memory_finish(&device) == VK_ERROR_DEVICE_LOST);
   }
   assert(anv_cubit_memory_poll() == 16);
   assert(anv_cubit_memory_init(&device, 60) == VK_ERROR_OUT_OF_HOST_MEMORY);
   assert(!device.cubit_cpu_mappings && allocations == 0 && frees == 0);
   puts("ANV memory lifecycle PASS (actual types, 128 completed reuse cycles, 16 quarantined records, detached cleanup)");
}
