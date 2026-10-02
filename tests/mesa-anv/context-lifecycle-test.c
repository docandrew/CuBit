/* Actual ANV types and callbacks; mocked retirement, no GPU execution. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/native_gpu_buffers.h"
#include "../../userspace/mesa/anv/native_gpu_mapping.h"
#include <assert.h>
#include <math.h>
#include <stdio.h>
static unsigned drains, closes, polls;
static uint32_t retirement = 4;
bool cubit_cpu_tracker_drain(struct cubit_cpu_mapping_tracker *tracker)
{ assert(tracker->slot == 61); drains++; return true; }
uint32_t cubit_intel_close_session(uint64_t slot, uint64_t *tag)
{ assert(slot == 61); closes++; *tag = 1; return 0; }
uint32_t cubit_intel_poll_session_retirement(uint64_t slot)
{ assert(slot == 61); polls++; return retirement; }
VkResult _vk_device_set_lost(struct vk_device *device, const char *file,
                             int line, const char *message, ...)
{ (void)file; (void)line; (void)message;
  p_atomic_set(&device->_lost.lost, 1); return VK_ERROR_DEVICE_LOST; }
int main(void)
{
   static struct anv_physical_device physical;
   static struct anv_device device;
   device.physical = &physical;
   physical.queue.family_count = 1;
   physical.queue.families[0].engine_class = INTEL_ENGINE_CLASS_RENDER;
   float priority = 0.5f;
   VkDeviceQueueCreateInfo q = {
      .sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
      .queueCount = 1, .pQueuePriorities = &priority};
   VkDeviceCreateInfo c = {.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
      .queueCreateInfoCount = 1, .pQueueCreateInfos = &q};
   struct anv_queue queue = {.device = &device, .family = &physical.queue.families[0]};
   struct anv_queue foreign = {.device = &device, .family = &physical.queue.families[0]};
   assert(anv_cubit_setup_context(&device, &c, 1) == VK_ERROR_INITIALIZATION_FAILED);
   assert(anv_cubit_memory_init(&device, 61) == VK_SUCCESS);
   assert(anv_cubit_create_engine(&device, &queue, &q) == VK_ERROR_INITIALIZATION_FAILED);
   for (unsigned i = 0; i < 10; i++) {
      VkDeviceQueueCreateInfo saved = q;
      switch (i) {
      case 0: q.flags = VK_DEVICE_QUEUE_CREATE_PROTECTED_BIT; break;
      case 1: q.queueCount = 2; break;
      case 2: q.queueFamilyIndex = 1; break;
      case 3: q.pNext = &c; break;
      case 4: q.pQueuePriorities = NULL; break;
      case 5: priority = NAN; break;
      case 6: priority = -0.1f; break;
      case 7: priority = 1.1f; break;
      case 8: physical.queue.family_count = 2; break;
      case 9: physical.queue.families[0].engine_class = INTEL_ENGINE_CLASS_COPY; break;
      }
      assert(anv_cubit_setup_context(&device, &c, 1) == VK_ERROR_FEATURE_NOT_PRESENT);
      q = saved; priority = 0.5f; physical.queue.family_count = 1;
      physical.queue.families[0].engine_class = INTEL_ENGINE_CLASS_RENDER;
   }
   assert(anv_cubit_setup_context(&device, &c, 2) == VK_ERROR_FEATURE_NOT_PRESENT);
   assert(anv_cubit_setup_context(&device, &c, 1) == VK_SUCCESS);
   assert(anv_cubit_setup_context(&device, &c, 1) == VK_ERROR_INITIALIZATION_FAILED);
   queue.vk.index_in_family = 1;
   assert(anv_cubit_create_engine(&device, &queue, &q) == VK_ERROR_FEATURE_NOT_PRESENT);
   queue.vk.index_in_family = 0;
   assert(anv_cubit_create_engine(&device, &queue, &q) == VK_SUCCESS);
   assert(anv_cubit_create_engine(&device, &foreign, &q) == VK_ERROR_INITIALIZATION_FAILED);
   anv_cubit_destroy_engine(&device, &foreign);
   assert(anv_cubit_create_engine(&device, &foreign, &q) == VK_ERROR_INITIALIZATION_FAILED);
   anv_cubit_destroy_engine(&device, &queue);
   anv_cubit_destroy_engine(&device, &queue);
   assert(anv_cubit_create_engine(&device, &queue, &q) == VK_ERROR_INITIALIZATION_FAILED);
   assert(!drains && !closes && !polls);
   assert(!anv_cubit_destroy_context(&device)); /* pending, retained */
   assert(!device.cubit_cpu_mappings && drains == 1 && closes == 1 && polls == 1);
   assert(anv_cubit_memory_slot_retained(61));
   assert(anv_cubit_destroy_context(&device)); /* no close replay */
   anv_cubit_close_device(&device);
   assert(drains == 1 && closes == 1 && polls == 1);
   retirement = 0;
   assert(anv_cubit_memory_poll() == 0);
   assert(!anv_cubit_memory_slot_retained(61));
   /* Failed creation before context setup still closes the open transport. */
   static struct anv_device failed;
   assert(anv_cubit_memory_init(&failed, 61) == VK_SUCCESS);
   anv_cubit_close_device(&failed);
   assert(!failed.cubit_cpu_mappings && drains == 2 && closes == 2 && polls == 3);
   anv_cubit_close_device(&failed);
   assert(drains == 2 && closes == 2 && polls == 3);
   puts("ANV context lifecycle PASS: bounded render setup, duplicate denial, deferred retirement (mock IPC)");
}
