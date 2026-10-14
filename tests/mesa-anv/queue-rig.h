#pragma once
/* A device rig for hosted session-queue fixtures with actual ANV types:
 * attach, logical context, engine, a startup batch and two command buffers,
 * as device creation and vkQueueSubmit drive the adapter. The fixture
 * supplies the IPC mocks; the queue is gpu-queue-mock.c. */
#include "../../userspace/mesa/anv/anv_cubit_memory.h"
#include "../../userspace/mesa/anv/anv_cubit_sync.h"
#include "gpu-queue-mock.h"
#include <assert.h>
#include <stdlib.h>

static struct anv_physical_device physical;
static struct vk_sync_binary_type binary_type;
static float priority = 0.5f;
static VkDeviceQueueCreateInfo queue_info = {.sType = VK_STRUCTURE_TYPE_DEVICE_QUEUE_CREATE_INFO,
   .queueCount = 1, .pQueuePriorities = &priority};
static VkDeviceCreateInfo device_info = {.sType = VK_STRUCTURE_TYPE_DEVICE_CREATE_INFO,
   .queueCreateInfoCount = 1, .pQueueCreateInfos = &queue_info};
static uint32_t storage[1024];

struct rig {
   struct anv_device device;
   struct anv_queue queue;
   struct anv_bo bo;
   struct anv_bo *batches[1];
   struct anv_async_submit startup;
   struct anv_cmd_buffer cmd[2];
   struct anv_bo cmd_bo[2];
   struct anv_batch_bo cmd_batch[2];
   struct anv_cmd_buffer *cmds[2];
   cubit_endpoint endpoint;
};

static struct vk_sync *make(struct rig *r, const struct vk_sync_type *type,
                            enum vk_sync_flags flags)
{
   struct vk_sync *sync = calloc(1, type->size);
   assert(sync && vk_sync_init(&r->device.vk, sync, type, flags, 0) == VK_SUCCESS);
   return sync;
}

/* attach, logical context and engine: what device creation does first. */
static void open_rig(struct rig *r, cubit_endpoint endpoint)
{
   r->endpoint = endpoint;
   r->device.physical = &physical;
   r->device.vk.physical = &physical.vk;
   r->device.batch_bo_pool.bo_alloc_flags = ANV_BO_ALLOC_HOST_CACHED;
   assert(anv_cubit_attach_session(&r->device, endpoint) == VK_SUCCESS);
   assert(anv_cubit_setup_context(&r->device, &device_info, 1) == VK_SUCCESS);
   r->queue = (struct anv_queue){.device = &r->device, .family = &physical.queue.families[0]};
   assert(anv_cubit_create_engine(&r->device, &r->queue, &queue_info) == VK_SUCCESS);
   r->bo = (struct anv_bo){.gem_handle = 17, .offset = 0x20000, .size = 4096,
                           .actual_size = 4096, .map = storage};
   r->batches[0] = &r->bo;
   r->startup = (struct anv_async_submit){.queue = &r->queue, .bo_pool = &r->device.batch_bo_pool};
   r->startup.batch_bos.data = r->batches;
   r->startup.batch_bos.size = sizeof(r->batches);
   for (unsigned i = 0; i < 2; i++) {
      r->cmd_bo[i] = (struct anv_bo){.gem_handle = 30 + i, .offset = 0x40000 + 0x10000 * i,
                                     .actual_size = 8192};
      r->cmd_batch[i] = (struct anv_batch_bo){.bo = &r->cmd_bo[i], .length = 4096};
      r->cmd[i].device = &r->device;
      list_inithead(&r->cmd[i].batch_bos);
      list_addtail(&r->cmd_batch[i].link, &r->cmd[i].batch_bos);
      r->cmds[i] = &r->cmd[i];
   }
}

/* The private startup fence an internal batch signals. */
static VkResult startup(struct rig *r, struct vk_sync *fence, uint32_t signal_count,
                        const struct vk_sync_signal *signals)
{
   r->startup.signal = (struct vk_sync_signal){.sync = fence};
   gpu_queue_mock_forbid_waits = true;
   VkResult result = anv_cubit_queue_exec_async(&r->startup, 0, NULL, signal_count, signals);
   gpu_queue_mock_forbid_waits = false;
   return result;
}

static VkResult submit(struct rig *r, uint32_t cmd_count, uint32_t wait_count,
                       const struct vk_sync_wait *waits, uint32_t signal_count,
                       const struct vk_sync_signal *signals)
{
   gpu_queue_mock_forbid_waits = true;
   VkResult result = anv_cubit_queue_exec_locked(&r->queue, wait_count, waits, cmd_count,
      cmd_count ? r->cmds : NULL, signal_count, signals, NULL, 0, NULL);
   gpu_queue_mock_forbid_waits = false;
   return result;
}

static bool done(struct rig *r, struct vk_sync *sync, uint64_t value)
{
   return vk_sync_wait(&r->device.vk, sync, value, 0, 0) == VK_SUCCESS;
}

static void close_rig(struct rig *r)
{
   anv_cubit_destroy_engine(&r->device, &r->queue);
   (void)anv_cubit_destroy_context(&r->device);
   anv_cubit_close_device(&r->device);
   assert(!r->device.cubit_cpu_mappings);
}

/* The physical device every rig shares: one render family, explicit cache
 * maintenance, the GPU timeline and its binary wrapper as sync types. */
static const struct vk_sync_type *const *rig_physical(void)
{
   static const struct vk_sync_type *types[3];
   binary_type = anv_cubit_binary_sync_type();
   types[0] = &anv_cubit_gpu_timeline_type;
   types[1] = &binary_type.sync;
   physical.vk.supported_sync_types = types;
   physical.memory.need_flush = true;
   physical.queue.family_count = 1;
   physical.queue.families[0].engine_class = INTEL_ENGINE_CLASS_RENDER;
   physical.va.null_initialized_heap.addr = 0x20000;
   physical.va.null_initialized_heap.size = UINT64_C(8) << 30;
   gpu_queue_mock_reset();
   return types;
}
